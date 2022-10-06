%% Testing for prime numbers using a line based protocol with json requests and responses
%% https://protohackers.com/problem/1

-module(ph1).
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

-record(state, {ref, transport, socket}).

start_link(Ref, Transport, Opts) ->
    gen_server:start_link(?MODULE, {Ref, Transport, Opts}, []).

init({Ref, Transport, _Opts = []}) ->
    {ok, #state{ref = Ref, transport = Transport}, {continue, do_handshake}}.

handle_continue(do_handshake, State = #state{ref = Ref, transport = Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    %% Line packaging also truncates at recbuf size, so increasing it as well
    ok = Transport:setopts(Socket, [{active, once}, {packet, line}, {recbuf, 1024 * 1024}]),
    {ok, {ClientIP, _}} = inet:peername(Socket),
    io:format("Client connected: ~p~n", [ClientIP]),
    {noreply, State#state{socket = Socket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State = #state{socket = Socket, transport = Transport}) ->
    io:format("Received ~p bytes~n", [erlang:size(Data)]),
    SplitData = string:split(Data, <<"\n">>, all),
    lists:foreach(
        fun
            (<<>>) ->
                ok;
            (D) ->
                Reply = get_reply(D),
                Transport:send(Socket, Reply)
        end,
        SplitData
    ),
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, Socket}, State = #state{socket = Socket}) ->
    io:format("TCP Close~n"),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    io:format("TCP Error~n"),
    {stop, Reason, State}.

get_reply(Data) ->
    case thoas:decode(string:trim(Data)) of
        {ok, #{<<"method">> := <<"isPrime">>, <<"number">> := Number}} when is_number(Number) ->
            io:format("Number: ~p~n", [Number]),
            Response = #{<<"method">> => <<"isPrime">>, <<"prime">> => is_prime(Number)},
            [thoas:encode(Response), <<"\n">>];
        _ ->
            io:format("Malformed: ~p~n", [Data]),
            <<"malformed\n">>
    end.

is_prime(2) -> true;
is_prime(N) when is_integer(N) andalso N > 2 -> check(N, 2, erlang:floor(math:sqrt(N)) + 1);
is_prime(_) -> false.

check(_, Max, Max) ->
    true;
check(N, I, Max) ->
    if
        N rem I =:= 0 ->
            false;
        true ->
            check(N, I + 1, Max)
    end.
