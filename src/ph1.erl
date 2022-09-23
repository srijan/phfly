-module(ph1).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = inet:setopts(Socket, [{packet, line}, {recbuf, 1024*1024}]),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 60000) of
        {ok, Data} when Data =/= <<4>> ->
            SplitData = string:split(Data, <<"\n">>, all),
            lists:foreach(
              fun(<<>>) -> ok;
                 (D) ->
                      Reply = get_reply(D),
                      Transport:send(Socket, Reply)
              end, SplitData),
            loop(Socket, Transport);
        _ ->
            ok = Transport:close(Socket)
    end.

get_reply(Data) ->
    case thoas:decode(string:trim(Data)) of
        {ok, #{<<"method">> := <<"isPrime">>, <<"number">> := Number}} when is_number(Number) ->
            io:format("Number: ~p~n", [Number]),
            Response = #{<<"method">> => <<"isPrime">>, <<"prime">> => is_prime(Number)},
            <<(thoas:encode(Response))/binary, 10>>;
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
