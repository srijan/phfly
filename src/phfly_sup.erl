-module(phfly_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    PH4 = #{
        id => ph4,
        start => {ph4, start_link, []},
        restart => permanent,
        type => worker
    },
    ChildSpecs = [PH4],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
