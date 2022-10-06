-module(ph4).
-behaviour(gen_server).

-export([start_link/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {socket, kv}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    Socket = start_listening(),
    {ok, #state{socket = Socket, kv = #{}}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, FromIp, FromPort, Command}, State = #state{socket = Socket, kv = KV}) ->
    io:format("Got command: ~p~n", [Command]),
    Split = string:split(Command, "="),
    NewKV = handle_command(Split, Socket, {FromIp, FromPort}, KV),
    {noreply, State#state{kv = NewKV}}.

%% internal functions

start_listening() ->
    Port = application:get_env(phfly, udp_port, 8080),
    {ok, ListenIP} =
        case os:getenv("FLY_APP_NAME") of
            false ->
                {ok, {0, 0, 0, 0}};
            _AppName ->
                inet:getaddr("fly-global-services", inet)
        end,
    {ok, Socket} = gen_udp:open(Port, [{ip, ListenIP}]),
    io:format("UDP Server listening on ~p:~p~n", [ListenIP, Port]),
    Socket.

handle_command(["version"], Socket, Source, KV) ->
    gen_udp:send(Socket, Source, "version=v3"),
    KV;
handle_command([Key], Socket, Source, KV) ->
    Value = maps:get(Key, KV, ""),
    gen_udp:send(Socket, Source, [Key, "=", Value]),
    KV;
handle_command([Key, Value], _Socket, _Source, KV) ->
    maps:put(Key, Value, KV).
