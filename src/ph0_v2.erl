%% I was not able to make this version work

-module(ph0_v2).
-behaviour(ranch_protocol).
-behaviour(gen_server).

%% For ranch_protocol behaviour
-export([start_link/3]).

%% For gen_server behaviour
-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {ref, transport, socket, recv_buffer :: list(iolist())}).

start_link(Ref, Transport, Opts) ->
    gen_server:start_link(?MODULE, {Ref, Transport, Opts}, []).

init({Ref, Transport, _Opts = []}) ->
    {ok, #state{ref = Ref, transport = Transport, recv_buffer = []}, {continue, do_handshake}}.

handle_continue(do_handshake, State = #state{ref = Ref, transport = Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, raw}]),
    {ok, {ClientIP, _}} = inet:peername(Socket),
    io:format("Client connected: ~p~n", [ClientIP]),
    {noreply, State#state{socket = Socket}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State = #state{socket = Socket, transport = Transport}) ->
    io:format("Received: ~p~n", [Data]),
    RecvBuffer = State#state.recv_buffer ++ [Data],
    {Valid, Remaining} = split_buffer([], RecvBuffer),
    lists:foreach(
        fun(V) ->
            ok = Transport:send(Socket, [V, <<"\n">>])
        end,
        Valid
    ),
    case Remaining of
        [<<4>>] ->
            ok = Transport:close(Socket),
            {stop, normal, State};
        _ ->
            case Data of
                <<4>> ->
                    ok = Transport:close(Socket),
                    {stop, normal, State};
                _ ->
                    Transport:setopts(Socket, [{active, once}]),
                    {noreply, State#state{recv_buffer = Remaining}}
            end
    end;
handle_info({tcp_closed, Socket}, State = #state{socket = Socket, transport = Transport}) ->
    io:format("TCP Close~n"),
    Transport:close(Socket),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State = #state{socket = Socket, transport = Transport}) ->
    io:format("TCP Error~n"),
    Transport:close(Socket),
    {stop, Reason, State}.

split_buffer(Valid, Remaining) ->
    case string:split(Remaining, <<"\n">>) of
        [Buffer] ->
            {lists:reverse(Valid), Buffer};
        [NextValid | [<<>>]] ->
            Final = lists:reverse([NextValid | Valid]),
            {Final, []};
        [NextValid | NextRemaining] ->
            split_buffer([NextValid | Valid], NextRemaining)
    end.
