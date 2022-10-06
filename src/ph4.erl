-module(ph4).
-behaviour(gen_server).

-export([start_link/0]).
-export([
    init/1,
    handle_info/2
]).

-record(state, {socket, kv}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, Socket} = gen_udp:open(8080),
    io:format("UDP Server started~n"),
    {ok, #state{socket = Socket, kv = #{}}}.

handle_info({udp, Socket, FromIp, FromPort, Command}, State = #state{socket = Socket, kv = KV}) ->
    io:format("Got command: ~p~n", [Command]),
    NewKV = handle_command(Command, Socket, {FromIp, FromPort}, KV),
    {noreply, State#state{kv = NewKV}}.

handle_command("version", Socket, Source, KV) ->
    gen_udp:send(Socket, Source, "version=v1"),
    KV;

handle_command(Command, Socket, Source, KV) ->
    NewKV = case string:split(Command, "=") of
        [Key] ->
            case maps:get(Key, KV, undefined) of
                undefined ->
                    gen_udp:send(Socket, Source, [Key, "="]);
                Value ->
                    gen_udp:send(Socket, Source, [Key, "=", Value])
            end,
            KV;
        [Key, Value] ->
            maps:put(Key, Value, KV)
    end,
    NewKV.
