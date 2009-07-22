%% @private
%% @author Jacob Perkins
-module(routy_sup).

-behaviour(supervisor).

-include("routy.hrl").

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	{ok, {{one_for_one, 1, 60}, [
		{?CACHE, {gen_cache, start_link, [?CACHE]},
		 permanent, brutal_kill, worker, [?CACHE]}
	]}}.
