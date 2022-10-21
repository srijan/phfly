%% https://protohackers.com/problem/5

-module(ph5).
-behaviour(ranch_protocol).
-behaviour(gen_statem).

%% For ranch_protocol behaviour
-export([start_link/3]).

%% gen_statem.
-export([
    callback_mode/0,
    init/1,
    connected/3,
    terminate/3
]).

-record(state, {ref, transport, socket, out_socket}).

start_link(Ref, Transport, Opts) ->
    gen_statem:start_link(?MODULE, {Ref, Transport, Opts}, []).

callback_mode() ->
    [state_functions, state_enter].

init({Ref, Transport, _Opts = []}) ->
    {ok, connected, #state{ref = Ref, transport = Transport}}.

connected(enter, connected, StateData = #state{ref = Ref, transport = Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    %% Line packaging also truncates at recbuf size, so increasing it as well
    ok = Transport:setopts(Socket, [
        {active, once}, {packet, line}, {mode, list}, {recbuf, 1024 * 1024}
    ]),
    {ok, {ClientIP, _}} = inet:peername(Socket),
    io:format("Client connected: ~p~n", [ClientIP]),
    %% TODO: Add actual name resolution for chat.protohackers.com
    {ok, OutSocket} = gen_tcp:connect(
        "206.189.113.124", 16963, [{active, once}, {packet, line}, {recbuf, 1024 * 1024}], 10000
    ),
    io:format("Out Connection established: ~n"),
    {keep_state, StateData#state{socket = Socket, out_socket = OutSocket}};
connected(
    info,
    {tcp, OutSocket, Data},
    StateData = #state{socket = Socket, transport = Transport, out_socket = OutSocket}
) ->
    TData = transform_data(Data),
    Transport:send(Socket, TData),
    ok = inet:setopts(OutSocket, [{active, once}]),
    keep_state_and_data;
connected(
    info,
    {tcp, Socket, Data},
    StateData = #state{socket = Socket, transport = Transport, out_socket = OutSocket}
) ->
    TData = transform_data(Data),
    gen_tcp:send(OutSocket, TData),
    ok = Transport:setopts(Socket, [{active, once}]),
    keep_state_and_data;
connected(info, {tcp_closed, _Socket}, _StateData) ->
    {stop, normal};
connected(info, {tcp_error, _, Reason}, _StateData) ->
    {stop, Reason}.

terminate(
    Reason,
    StateName,
    StateData = #state{
        socket = Socket, transport = Transport
    }
) when
    Socket =/= undefined, Transport =/= undefined
->
    catch Transport:close(Socket),
    terminate(
        Reason,
        StateName,
        StateData#state{socket = undefined, transport = undefined}
    );
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%

transform_data(Data) ->
    Words = string:lexemes(Data, " \r\n"),
    Words1 = lists:map(
        fun
            ([$7 | Rest]) when length(Rest) >= 25 andalso length(Rest) =< 34 ->
                "7YWHMfk9JZe0LM0g1ZauHuiSxhI";
            (D) ->
                D
        end,
        Words
    ),
    [string:join(Words1, " "), "\n"].
