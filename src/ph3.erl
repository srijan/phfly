-module(ph3).
-behaviour(ranch_protocol).
-behaviour(gen_statem).

%% For ranch_protocol behaviour
-export([start_link/3]).

%% gen_statem.
-export([
    callback_mode/0,
    init/1,
    connected/3,
    joined/3,
    terminate/3
]).

-record(state, {ref, transport, socket, name}).

start_link(Ref, Transport, Opts) ->
    gen_statem:start_link(?MODULE, {Ref, Transport, Opts}, []).

callback_mode() ->
    [state_functions, state_enter].

init({Ref, Transport, _Opts = []}) ->
    {ok, connected, #state{ref = Ref, transport = Transport}}.

connected(enter, connected, StateData = #state{ref = Ref, transport = Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    %% Line packaging also truncates at recbuf size, so increasing it as well
    ok = Transport:setopts(Socket, [{active, once}, {packet, line}, {recbuf, 1024 * 1024}]),
    {ok, {ClientIP, _}} = inet:peername(Socket),
    io:format("Client connected: ~p~n", [ClientIP]),
    Transport:send(Socket, <<"What shall I call you?\n">>),
    {keep_state, StateData#state{socket = Socket}};
connected(info, {tcp, Socket, NameIn}, StateData = #state{socket = Socket, transport = Transport}) ->
    Name = string:trim(NameIn),
    io:format("Got name: ~p~n", [Name]),
    case re:run(Name, "^[0-9A-Za-z]+$") of
        {match, _} ->
            case catch gproc:add_local_name({user, Name}) of
                true ->
                    ok = Transport:setopts(Socket, [{active, once}]),
                    {next_state, joined, StateData#state{name = Name}};
                {'EXIT', {badarg, _}} ->
                    Transport:send(Socket, <<"Name already taken, bye.\n">>),
                    {stop, normal}
            end;
        nomatch ->
            Transport:send(Socket, <<"Invalid name, bye.\n">>),
            {stop, normal}
    end;
connected(info, {tcp_closed, _Socket}, _StateData) ->
    {stop, normal};
connected(info, {tcp_error, _, Reason}, _StateData) ->
    {stop, Reason}.

joined(enter, connected, StateData = #state{socket = Socket, transport = Transport, name = Name}) ->
    gproc:reg({p, l, chat_messages}),
    %% Magic!
    Users = gproc:select([
        {{{n, l, {user, '$1'}}, '_', '_'}, [{'=/=', '$1', Name}], [['$1', <<" ">>]]}
    ]),
    InformMsg = [<<"* ">>, Name, <<" has entered the room\n">>],
    gproc:send({p, l, chat_messages}, {self(), chat_messages, InformMsg}),
    Transport:send(Socket, [<<"* The room contains: ">>, Users, <<"\n">>]),
    {keep_state, StateData};
joined(
    info,
    {tcp, Socket, Data},
    StateData = #state{socket = Socket, transport = Transport, name = Name}
) ->
    io:format("Received ~p bytes~n", [erlang:size(Data)]),
    gproc:send({p, l, chat_messages}, {self(), chat_messages, [<<"[">>, Name, <<"] ">>, Data]}),
    ok = Transport:setopts(Socket, [{active, once}]),
    {next_state, joined, StateData};
joined(info, {Pid, chat_messages, _Data}, _StateData) when Pid == self() ->
    keep_state_and_data;
joined(
    info,
    {_Pid, chat_messages, Data},
    StateData = #state{socket = Socket, transport = Transport}
) ->
    Transport:send(Socket, Data),
    {keep_state, StateData};
joined(info, {tcp_closed, _Socket}, #state{name = Name}) ->
    Msg = [<<"* ">>, Name, <<" has left the room\n">>],
    gproc:send({p, l, chat_messages}, {self(), chat_messages, Msg}),
    {stop, normal};
joined(info, {tcp_error, _, Reason}, #state{name = Name}) ->
    Msg = [<<"* ">>, Name, <<" has left the room\n">>],
    gproc:send({p, l, chat_messages}, {self(), chat_messages, Msg}),
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
