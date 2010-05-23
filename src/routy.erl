%% @doc Url dispatching module. Complies with yaws appmod interface. Routy
%% clients can throw `forbidden' for `{status, 403}', `notfound' for
%% `{status, 404}' and `badrequest' for `{status, 400}'.
%% @todo Include error messages in response bodies.
%% @author Jacob Perkins
-module(routy).

-behaviour(application).

-include("routy.hrl").
-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([start/2, stop/1, config_change/3, behaviour_info/1]).
-export([auth/2, out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
start(_Type, _Args) ->
	config_change(application:get_all_env(routy), [], []),
	routy_sup:start_link().

%% @private
stop(_State) -> ok.

%% @private
config_change(_Changed, _New, _Removed) ->
	{ok, ID} = application:get_env(routy, id),
	Props = application:get_all_env(routy),
	
	R = fun({Route, Handlers}) ->
			{ok, Compiledroute} = re:compile(Route), 
			{{Route, Compiledroute}, Handlers} 
		end,
	
	P = fun(Mod) -> lists:map(R, Mod:routes()) end,
	
	% TODO: routes need to be queried on demand, not created at start or app
	% change, since routing apis may change in other apps without the routy
	% app needing to be updated
	
	% recreate route paths and full route specs from route modules
	RouteMods = proplists:get_value(route_mods, Props, []),
	AllRoutes = lists:map(P, RouteMods),
	ok = application:set_env(routy, routes, lists:flatten(AllRoutes)),
	
	{ok, Authdirs} = application:get_env(routy, authdirs),
	{ok, Realm} = application:get_env(routy, realm),
	Auths = [#auth{dir=[Dir], realm=Realm, type="OAuth", mod=?MODULE} || Dir <- Authdirs],
	% yaws global config
	G1 = yaws_config:make_default_gconf(false, ID),
	G2 = G1#gconf{logdir=proplists:get_value(logdir, Props)},
	% yaws server config
	S1 = yaws_config:make_default_sconf(),
	S2 = S1#sconf{
		docroot=".", listen={0, 0, 0, 0}, port=proplists:get_value(port, Props),
		servername=proplists:get_value(server_name, Props),
		authdirs=Auths, appmods=[{"/", routy}]
	},
	
	yaws_api:setconf(G2, [[S2]]).

behaviour_info(callbacks) -> [{routes, 0}].

%%%%%%%%%%%%%
%% authmod %%
%%%%%%%%%%%%%

auth(_A, _Auth) ->
	true.

%%%%%%%%%%%%
%% appmod %%
%%%%%%%%%%%%

%% @doc Yaws appmod out function. Returns Yaws compatible results.
%% @spec out(Request) -> Result
out(Request) ->
	try out_routes(Request)
	catch
		Err:Reason ->
			error_logger:error_report([{Err, Reason}]),
			{status, 500}
	end.

out_routes(Request) ->
	{ok, Routes} = application:get_env(routy, routes),

	% Check if the request path match any of the registered routes
	MatchingUrl = lists:dropwhile(
								fun({_, CompiledUrl}) ->
										case re:run(Request#arg.server_path, CompiledUrl) of
													nomatch -> true;
													_ -> false
										end
		
								end,
								proplists:get_keys(Routes)
								),

	case MatchingUrl of
		[] ->
			error_logger:error_report([{not_found, Request#arg.server_path}]),
			{status, 404};
		[{FirstMatchedUrl, FirstMatchedCompiledUrl} | _] ->
			Methods = proplists:get_value({FirstMatchedUrl, FirstMatchedCompiledUrl} , Routes),
			out_method(Request, (Request#arg.req)#http_request.method, Methods)
	end.

out_method(Request, Method, Methods) ->
	case proplists:get_value(Method, Methods) of
		undefined ->
			error_logger:error_report([{not_allowed, Method}, {methods, Methods}]),
			{status, 405};
		ListRequestHandlers ->
			out_vars(Request, Method, ListRequestHandlers)
	end.

out_vars(Request, Method, ListRequestHandlers) when Method =:= 'GET' orelse Method =:= 'POST' ->
	routy_util:try_route(Request, Method, ListRequestHandlers);
out_vars(_, Method, ListRequestHandlers) ->
	error_logger:error_report([{not_implemented, Method}, ListRequestHandlers]),
	{status, 501}.

