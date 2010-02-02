%% @doc Yaws utility functions.
%%
%% @author Jacob Perkins
%% @todo define prop as an edoc type
-module(routy_util).

-include("routy.hrl").

-export([
	encode_props/1,
	encode_props/2,
	encode_prop/1,
	decode/1,
	decode_props/1,
	dejsonify/1,
	stringify/1,
	structify/1,
	render_tmpl/2,
	compile_templates/1,
	extract_named_subpatterns/1,
	parse_url/2,
	make_args/2,
	try_route/3,
	try_route/5,
	http_error/2,
	mime_type/1
]).


%% @doc Returns mime type from a file name.
%% @spec mime_type(string()) -> string()
mime_type(FileName) ->
	yaws_api:mime_type(FileName).

%% @doc Encode url props.
%% @spec encode_props([Prop::term()]) -> string()
%% @see encode_prop/1
encode_props([]) ->
	[];
encode_props([Prop]) ->
	encode_prop(Prop);
encode_props([Prop | Props]) ->
	encode_prop(Prop) ++ "&" ++ encode_props(Props).

%% @doc Encodes props and appends to Url.
%% @spec encode_props(Url, Props) -> string()
%% @todo check if Url ends already has `?'
%% @see encode_props/1
encode_props(Url, []) -> Url;
encode_props(Url, Props) -> Url ++ "?" ++ encode_props(Props).

%% @doc Url encodes property.
%% @spec encode_prop(Prop::term()) -> string()
encode_prop({Key, Val}) ->
	stringify(Key) ++ "=" ++ yaws_api:url_encode(stringify(Val));
encode_prop(Key) ->
	stringify(Key) ++ "=".

%% @doc Decodes Url's props.
%% @spec decode(string()) -> [Prop::term()]
decode(URL) ->
	case estring:splitc(URL, $?) of
		{URL, []} -> [];
		{_, Query} -> decode_props(Query)
	end.

%% @doc Decode url props.
%% @spec decode_props(string()) -> [Prop::term()]
decode_props(Query) ->
	F = fun(Var) -> estring:splitc(Var, $=) end,
	lists:map(F, string:tokens(Query, "&")).

%% @doc Removes Yaws JSON terms and tuple enclosures.
%% @spec dejsonify(JsonTerm::term()) -> term()
dejsonify({struct, Props}) -> [dejsonify(P) || P <- Props];
dejsonify({array, List}) -> [dejsonify(I) || I <- List];
dejsonify({Key, Val}) when is_atom(Key) -> {Key, dejsonify(Val)};
dejsonify(Item) -> Item.

%% @doc Converts term to string. Uses `io_lib:format(~f)' instead of
%% `float_to_list' to get default precision of 6.
%% @spec stringify(term()) -> string()
stringify(Term) when is_atom(Term) -> atom_to_list(Term);
stringify(Term) when is_integer(Term) -> integer_to_list(Term);
stringify(Term) when is_float(Term) -> hd(io_lib:format("~f", [Term]));
stringify(Term) when is_binary(Term) -> binary_to_list(Term);
stringify({{_, _, _}, {_, _, _}}=Term) -> datetime:iso_string(Term);
stringify(Term) when is_tuple(Term) -> tuple_to_list(Term);
stringify(Term) when is_list(Term) -> Term.

%% @doc Creates Yaws JSON compatible terms by enclosing known patterns in JSON
%% tuples.
%% @spec structify(Term::term()) -> JSONTerm::term()
structify([]) ->
	{struct, []};
structify({Key, [{_, _} | _]=Vals}) ->
	{Key, {struct, [structify(V) || V <- Vals]}};
structify({Key, Val}) when is_atom(Val) ->
	{Key, atom_to_list(Val)};
structify({Key, Val}) ->
	{Key, Val};
structify([_ | _]=Vals) ->
	{struct, [structify(V) || V <- Vals]}.

render_tmpl(Tmpl, Vars) ->
	{ok, Html} = Tmpl:render(Vars),
	{html, Html}.

compile_templates(App) when is_atom(App) ->
	% TODO: this is slight different than emod:compile_templates, but we should
	% choose one over the other and delete the other
	application:load(App),
	Out = filename:join(code:lib_dir(App), "ebin"),
	Docroot = filename:join(code:priv_dir(App), "docroot"),
	Opts = [{out_dir, Out}, {custom_tags_dir, Docroot}],
	
	F = fun(Template) ->
			File = filename:join(Docroot, atom_to_list(Template)),
			io:format("compiling ~s to ~p~n", [File, Template]),
			ok = erlydtl_compiler:compile(File, Template, Opts)
		end,
	
	{ok, Templates} = application:get_env(App, templates),
	lists:foreach(F, Templates);
compile_templates(App) ->
	lists:foreach(fun compile_templates/1, App).


extract_named_subpatterns(RegExp) ->
	case re:run(RegExp, "\\?<([A-Za-z_0-9]+)>", [ungreedy, {capture, all_but_first}, global] ) of
			{match, Captured}  -> [ string:substr(RegExp, Start+1, Length) || [{Start, Length}] <- Captured];
			_ -> []
	end.


parse_url(Url, UrlSpec) ->
	SubPatterns = extract_named_subpatterns(UrlSpec),

	case re:run(Url, UrlSpec, [{capture, SubPatterns}, global] ) of
			{match, Captured}  -> 
											Zipped = lists:zip(SubPatterns, lists:flatten(Captured)),
											
											lists:foldl(
															fun({_, {Start, Length}}, AccIn) when Start =:= -1 andalso Length =:= 0 ->
																			AccIn;
																({Pattern, {Start, Length}}, AccIn) ->
																			[{Pattern, string:substr(Url, Start+1, Length)} | AccIn]
															end,
															[],
															Zipped
															);
			_ -> []
	end.
	


%%%%%%%%%%%%%
%% routing %%
%%%%%%%%%%%%%

try_route(Request, Method, Module, Function, ParsedArgs) ->
	Terms = [{method, Method}, {module, Module},
			{function, Function}, {args, ParsedArgs}],
	Fun = fun() ->	apply(Module, Function, ParsedArgs) end,

	route(Fun, Terms).

try_route(Request, Method, [RequestHandler | RemainingRequestHandlers] = ListRequestHandlers) ->
	Terms = [{method, Method}, {request, Request}, {request_handlers, ListRequestHandlers}],
	Fun = fun() ->	RequestHandler(Request, RemainingRequestHandlers) end,

	route(Fun, Terms).


route(Route, Terms) when is_function(Route, 0) ->

	try Route() of
		ok -> {status, 204}; % no content
		Result -> Result
	catch
		throw:badarg ->
			Report = [badarg, Terms],
			error_logger:error_report(Report),
			routy_util:http_error(400, badarg);
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
