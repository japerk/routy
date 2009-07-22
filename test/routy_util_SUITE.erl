-module(routy_util_SUITE).

-export([all/0]).

-export([stringify/1]).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common_test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> [stringify].

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

stringify(_Config) ->
	"foobar" = routy_util:stringify("foobar"),
	"foobar" = routy_util:stringify(foobar),
	"5" = routy_util:stringify(5),
	"123.456700" = routy_util:stringify(123.456700),
	"-123.456789" = routy_util:stringify(-123.456789),
	ok.
