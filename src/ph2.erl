-module(ph2).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    Prices = ets:new(price_data, [ordered_set]),
    loop(Socket, Transport, Prices).

loop(Socket, Transport, Prices) ->
    case Transport:recv(Socket, 9, 60000) of
        {ok, <<"I", Time:4/big-signed-integer-unit:8, Price:4/big-signed-integer-unit:8>>} ->
            ets:insert(Prices, {Time, Price}),
            loop(Socket, Transport, Prices);
        {ok, <<"Q", Min:4/big-signed-integer-unit:8, Max:4/big-signed-integer-unit:8>>} ->
            Results = ets:select(Prices, [
                {{'$1', '$2'}, [{'>=', '$1', Min}, {'=<', '$1', Max}], ['$2']}
            ]),
            Mean =
                case Results of
                    [] ->
                        0;
                    _ ->
                        lists:sum(Results) div length(Results)
                end,
            Transport:send(Socket, <<Mean:4/big-signed-integer-unit:8>>),
            loop(Socket, Transport, Prices);
        _ ->
            ok = Transport:close(Socket)
    end.
