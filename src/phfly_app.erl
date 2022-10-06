-module(phfly_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, ProtocolMod} = application:get_env(phfly, protocol_mod),
    Port = application:get_env(phfly, tcp_port, 8080),
    case implements_ranch_protocol(ProtocolMod) of
        true ->
            {ok, _} = ranch:start_listener(
                phfly_tcp, ranch_tcp, #{socket_opts => [{port, Port}]}, ProtocolMod, []
            );
        false ->
            ok
    end,
    phfly_sup:start_link().

stop(_State) ->
    ranch:stop_listener(phfly_tcp),
    ok.

%% internal functions

implements_ranch_protocol(Mod) ->
    lists:member({behaviour, [ranch_protocol]}, Mod:module_info(attributes)).
