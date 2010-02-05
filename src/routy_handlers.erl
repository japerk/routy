%% @doc Routy request handlers module. 
%% Provide some request handlers
%% @author Bruno Mahe
-module(routy_handlers).


-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([decode_json/2]).
-export([extract_args/1, extract_args/2, extract_all_args/2, echo/1, nop/2, authkey/2]).
-export([redirect_template/2, redirect_template_from_templatedir/2]).
-export([redirect_template_fun/2, redirect_template_from_templatedir_fun/2]).

decode_json(Module, Function) ->
	fun (Req, _) ->
		case json:decode_string(binary_to_list(Req#arg.clidata)) of
			{error, _Err} ->
				throw(badarg);
			{ok, Json} ->
				routy_util:try_route((Req#arg.req)#http_request.method, Module, Function, [Json])
		end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extract_args handler factory %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_args({Module, Function, ListParameters}) ->
	fun (Req, _) ->
		Args = case (Req#arg.req)#http_request.method of
					'GET'  -> yaws_api:parse_query(Req);
					'POST' -> yaws_api:parse_post(Req)
				end,
		ParsedArgs = routy_util:make_args(ListParameters, Args),
		routy_util:try_route((Req#arg.req)#http_request.method, Module, Function, ParsedArgs)
	end.

%% This request handler factory returns a request handler
%% which will extract the required parameters and call the function
%% reference passed as a parameter

extract_args(UrlSpec, {Module, Function, ListParameters}) ->
	fun (Req, _) ->
			Args = case (Req#arg.req)#http_request.method of
							'GET'  -> yaws_api:parse_query(Req);
							'POST' -> yaws_api:parse_post(Req)
					end,
			ParsedArgs = routy_util:make_args(ListParameters, Args ++ routy_util:parse_url(Req#arg.server_path, UrlSpec)),
			routy_util:try_route((Req#arg.req)#http_request.method, Module, Function, ParsedArgs)
	end.


%% This request handler factory returns a request handler
%% which will extract all parameters and call the function
%% reference passed as a parameter

extract_all_args(UrlSpec, {Module, Function}) ->
	fun (Req, _) ->
			Args = case (Req#arg.req)#http_request.method of
							'GET'  -> yaws_api:parse_query(Req);
							'POST' -> yaws_api:parse_post(Req)
					end,

			ParsedArgs = Args ++ routy_util:parse_url(Req#arg.server_path, UrlSpec),
			routy_util:try_route((Req#arg.req)#http_request.method, Module, Function, [ParsedArgs])
	end.



redirect_template_fun(ErlyDTLTemplateFilename, ParamsCreator) ->
	% Compile the template
	Template = list_to_atom(binary_to_list(term_to_binary(erlang:phash2(ErlyDTLTemplateFilename)))),
	erlydtl:compile(ErlyDTLTemplateFilename, Template),
					
	fun (_, _) ->
				{ok, RenderedPage} = Template:render(ParamsCreator()),
				[{status, 200}, {content, "text/html", RenderedPage}]
	end.


redirect_template_from_templatedir_fun(ErlyDTLTemplateName, ParamsCreator) ->
	{ok, TemplateDir} = application:get_env(routy, templatedir),

	redirect_template_fun(TemplateDir ++ ErlyDTLTemplateName, ParamsCreator).



redirect_template(ErlyDTLTemplateFilename, ParamsCreator) ->
	% Compile the template
	Template = list_to_atom(binary_to_list(term_to_binary(erlang:phash2(ErlyDTLTemplateFilename)))),
	ok = erlydtl:compile(ErlyDTLTemplateFilename, Template),
					
	{ok, RenderedPage} = Template:render(ParamsCreator()),

	[{status, 200}, {content, "text/html", RenderedPage}].



redirect_template_from_templatedir(ErlyDTLTemplateName, ParamsCreator) ->
	{ok, TemplateDir} = application:get_env(routy, templatedir),
	redirect_template(TemplateDir ++ ErlyDTLTemplateName, ParamsCreator).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% echo handler factory %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This request handler returns a specific message.


echo(Message) ->
	fun (_, _) ->
		[{status, 200}, {content, "text/plain", io_lib:format("~p~n", [Message]) }]
	end.



%%%%%%%%%%%%%%%%%
%% NOP handler %%
%%%%%%%%%%%%%%%%%

%% This request handler doesn't do anything


nop(Req, [RH| RequestHandlers]) ->
	RH(Req, RequestHandlers).


%%%%%%%%%%%%%%%%%%%%%
%% authkey handler %%
%%%%%%%%%%%%%%%%%%%%%

%% routy authkey is double base64 encode.
%% step 1: generate digest from Salt + Secret + Client IP
%% step 2: base64 encode digest as Hash
%% step 3: base64 encode Salt:Hash as Authorization
%% set authorization header to "Routy" Authorization

authkey(Req, [RH| RequestHandlers]) ->
	case authorized(Req) of
		true ->
			RH(Req, RequestHandlers);
		false ->
			error_logger:warning_report([{notauthorized, Req}]),
			{status, 401}
	end.

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

