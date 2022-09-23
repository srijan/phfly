-module(phfly_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(phfly, tcp_port, 8080),
    {ok, ProtocolMod} = application:get_env(phfly, protocol_mod),
    {ok, _} = ranch:start_listener(
        phfly_tcp, ranch_tcp, #{socket_opts => [{port, Port}]}, ProtocolMod, []
    ),
    phfly_sup:start_link().

stop(_State) ->
    ranch:stop_listener(phfly_tcp),
    ok.

%% internal functions
