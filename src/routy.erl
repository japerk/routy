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
	
	P = fun(Mod, Paths) ->
			Routes = Mod:routes(),
			{Routes, proplists:get_keys(Routes) ++ Paths}
		end,
	
	% TODO: routes need to be queried on demand, not created at start or app
	% change, since routing apis may change in other apps without the routy
	% app needing to be updated
	
	% recreate route paths and full route specs from route modules
	RouteMods = proplists:get_value(route_mods, Props, []),
	{AllRoutes, RoutePaths} = lists:mapfoldl(P, [], RouteMods),
	ok = application:set_env(routy, routes, lists:flatten(AllRoutes)),
	
	{ok, Authdirs} = application:get_env(routy, authdirs),
	{ok, Realm} = application:get_env(routy, realm),
	Auths = [#auth{dir=[Dir], realm=Realm, type="OAuth", mod=?MODULE} || Dir <- Authdirs],
	
	% yaws global config
	G = [{auth_log, false}, {copy_errlog, false}, {id, ID}, {yaws, ID},
		 {logdir, proplists:get_value(logdir, Props)}],
	% yaws server config
	S = [{access_log, false}, {deflate, true}, {dir_listings, false},
		 {listen, {0, 0, 0, 0}}, {port, proplists:get_value(port, Props)},
		 {servername, proplists:get_value(server_name, Props)},
		 {authdirs, Auths},
		 {appmods, [{Path, routy} || Path <- RoutePaths]}],
	
	application:stop(yaws),
	application:unload(yaws),
	yaws:start_embedded(".", S, G).

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
%% @spec out(A) -> Result
out(A) ->
	try authorized(A) of
		true ->
			out_routes(A);
		false ->
			error_logger:warning_report([{notauthorized, A}]),
			{status, 401}
	catch
		Err:Reason ->
			error_logger:error_report([{Err, Reason}]),
			{status, 500}
	end.

out_routes(A) ->
	{ok, Routes} = application:get_env(routy, routes),
	
	case proplists:get_value(A#arg.server_path, Routes) of
		undefined ->
			error_logger:error_report([{not_found, A#arg.server_path}]),
			{status, 404};
		Methods ->
			out_method(A, (A#arg.req)#http_request.method, Methods)
	end.

out_method(A, Method, Methods) ->
	case proplists:get_value(Method, Methods) of
		undefined ->
			error_logger:error_report([{not_allowed, Method}, {methods, Methods}]),
			{status, 405};
		MFA ->
			out_vars(A, Method, MFA)
	end.

out_vars(A, 'GET', MFA) ->
	try_args('GET', MFA, yaws_api:parse_query(A));
out_vars(A, 'POST', {Module, Function, json}) ->
	case json:decode_string(binary_to_list(A#arg.clidata)) of
		{error, Err} -> http_error(400, Err);
		{ok, Json} -> try_route('POST', Module, Function, [Json])
	end;
out_vars(A, 'POST', MFA) ->
	try_args('POST', MFA, yaws_api:parse_query(A) ++ yaws_api:parse_post(A));
out_vars(_, Method, MFA) ->
	error_logger:error_report([{not_implemented, Method}, MFA]),
	{status, 501}.

try_args(Method, {Module, Function, Keys}, Props) ->
	try make_args(Keys, Props) of
		Args -> try_route(Method, Module, Function, Args)
	catch
		throw:badarg ->
			Report = [badarg, {module, Module}, {function, Function},
					  {keys, Keys}, {props, Props}],
			error_logger:error_report(Report),
			http_error(400, badarg)
	end.

%%%%%%%%%%%%%
%% authkey %%
%%%%%%%%%%%%%

%% routy authkey is double base64 encode.
%% step 1: generate digest from Salt + Secret + Client IP
%% step 2: base64 encode digest as Hash
%% step 3: base64 encode Salt:Hash as Authorization
%% set authorization header to "Routy" Authorization

authorized(A) ->
	{ok, Auth} = application:get_env(routy, authkey),
	not Auth orelse verify((A#arg.headers)#headers.other).

verify(Headers) ->
	{value, Authkey} = lists:keysearch("X-Authkey", 3, Headers),
	Authorization = base64:decode_to_string(element(5, Authkey)),
	{Salt, Hash} = estring:splitc(Authorization, $:),
	{value, RealIp} = lists:keysearch("X-Real-Ip", 3, Headers),
	{ok, Secret} = application:get_env(routy, secret),
	Digest = erlang:md5(lists:append([Salt, Secret, element(5, RealIp)])),
	Hash == base64:encode_to_string(Digest).

%%%%%%%%%%%%%
%% routing %%
%%%%%%%%%%%%%

try_route(Method, Module, Function, Args) ->
	Terms = [{method, Method}, {module, Module},
			 {function, Function}, {args, Args}],
	
	try route(Method, Module, Function, Args) of
		ok -> {status, 204}; % no content
		Result -> Result
	catch
		throw:badrequest ->
			error_logger:warning_report([badrequest | Terms]),
			http_error(400); % bad request
		throw:forbidden ->
			error_logger:warning_report([forbidden | Terms]),
			http_error(403); % forbidden
		throw:notfound ->
			error_logger:warning_report([notfound | Terms]),
			http_error(404); % not found
		throw:Reason ->
			error_logger:error_report([{throw, Reason} | Terms]),
			http_error(500);
		exit:Reason ->
			error_logger:error_report([{exit, Reason} | Terms]),
			http_error(500);
		error:undef ->
			error_logger:error_report([{error, undef} | Terms]),
			http_error(501); % not implemented
		error:Reason ->
			error_logger:error_report([{error, Reason} | Terms]),
			http_error(500)
	end.

% only cache if is a get request and caching is enabled
route('GET', Module, Function, Args) ->
	{ok, Cache} = application:get_env(routy, cache),
	
	if
		Cache -> ?recall(Module, Function, Args);
		true -> apply(Module, Function, Args)
	end;
route(_, Module, Function, Args) ->
	apply(Module, Function, Args).

http_error(Code) -> {status, Code}.

http_error(Code, Term) ->
	[{status, Code}, {content, "text/plain", routy_util:stringify(Term)}].

%%%%%%%%%%%%%%%
%% make args %%
%%%%%%%%%%%%%%%

make_args(Keys, Props) ->
	F = fun(Key) ->
			case make_arg(Key, Props) of
				undefined -> throw(badarg);
				Value -> Value
			end
		end,

	lists:map(F, Keys).

make_arg(props, Props) ->
	Props;
make_arg({Key, {Type, Default}}, Props) ->
	case proplists:get_value(Key, Props) of
		undefined -> Default;
		Value -> convert(Value, Type)
	end;
make_arg({Key, Type}, Props) ->
	convert(proplists:get_value(Key, Props), Type);
make_arg(Key, Props) ->
	proplists:get_value(Key, Props).

convert(undefined, _) -> undefined;
convert("true", bool) -> true;
convert("false", bool) -> false;
convert(Value, list) -> Value;
convert(Value, integer) -> list_to_integer(Value);
convert(Value, float) -> list_to_float(Value);
convert(_, _) -> undefined.
