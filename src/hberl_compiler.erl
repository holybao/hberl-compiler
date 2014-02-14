%%%-------------------------------------------------------------------
%%% author    libao he (holybao@hotmail.com & blog.holybao.com)
%%%-------------------------------------------------------------------
-module(hberl_compiler).
-import(hbutils, [format2binary/2]).

%% API
-export([compile/1, compile/2]).
-export([compile2forms/1, compile2forms/2]).
-export([compile2file/2, compile2file/3]).
-export([scan_file/1, scan_tokens/1, scan_tokens/2]).
-export([process_file/1, process_file/2]).
-export([process_tokens/1, process_tokens/2]).
-export([parse_file/1, parse_file/2, parse_tokens/1]).
-export([parse_transform/1]).

-record(erl_ctx, {
    out_dir = "./",
    include_dirs = [],
    file = undefined,
    reader = {file, read_file},
    module = "hbtest",
    record = [],
    clause_type = undefined,
    action = get,
    force_recompile = true,
    verbose = false}).

-record(scope, {                 
    names_dict = dict:new(),
    var_counter = 1}).

-record(trav, {
    erl_scopes = [#scope{}],        
    atms_dict = dict:new(),
    atm_counter = 1,
    fun_counter = 1}).

compile(File) -> compile(File, convert2CompilerOptions(File)).

%% Compiler Options: [{out_dir,OutDir},{include_dirs,IncludeDirs},
%% {file,File},{module,Module},...]
compile(File, Options) ->
    case compile2forms(File, transformOptions(File,Options)) of
        {ok, OriginForms} -> 
            Ctx = init_erl_ctx(Options),
            trace(?MODULE, ?LINE, "Origin Forms", OriginForms, Ctx),
            do_compile(OriginForms, Ctx);
        Error -> Error
    end.

compile2forms(File) -> compile2forms(File, convert2CompilerOptions(File)).

%% Preprocessor Options: [{include_dirs,IncludeDirs},{file,File}]
compile2forms(File, Options) -> parse_file(File, transformOptions(File,Options)).

compile2file(SourceFile, DestFile) -> 
    compile2file(SourceFile, DestFile, convert2CompilerOptions(SourceFile)).

%% Compiler Options
compile2file(SourceFile, DestFile, Options) -> 
    case compile2forms(SourceFile, transformOptions(SourceFile,Options)) of
        {ok, OriginForms} ->
            Ctx = init_erl_ctx(Options),
            case catch parse_transform(OriginForms, Ctx) of
                {ok, {Forms, {_Ctx1, Trav}}} -> 
                    {NewForms, ExportMaps} = processforms(Forms, {Ctx, Trav}),
                    trace(?MODULE, ?LINE, "New Forms", NewForms, Ctx),
                    ErlStr = erl_prettypr:format(erl_syntax:form_list(NewForms)),
                    NewStr = format2binary(<<"hbfunmap() -> ~p.\n">>, [ExportMaps]),
                    case file:write_file(DestFile, format2binary(<<"~s\n\n~s">>, 
                        [ErlStr, NewStr])) of
                        ok -> ok;
                        {error, Reason} -> 
                            {error, format2binary(<<"errcode:~p,write file '~s' failed">>,
                                [Reason, DestFile])}
                    end;
                Error -> formatError(Error)
            end;   
        Error -> formatError(Error)
    end.

scan_file(File) ->
    case fetchFileData(File) of
        {ok, Data} -> scan_tokens(binary_to_list(Data));
        {error, _}=Error -> Error
    end.

scan_tokens(Data) -> scan_tokens(Data, {1,1}).

scan_tokens(Data, StartLocation) ->
    case hberl_scan:tokens([], Data, StartLocation) of
        {done, Return, Rest} ->
            case Return of
                {ok, Tokens, EndLocation} ->
                    case scan_tokens(Rest, EndLocation) of
                        {ok, NewTokens} -> {ok, Tokens ++ NewTokens};
                        Error -> Error
                    end;
                {eof, EndLocation} -> {ok, [{eof, EndLocation}]};
                {error, ErrorInfo, _EndLocation} -> {error, ErrorInfo}
            end;
        {more, Continuation1} ->
            {done, Return, eof} = hberl_scan:tokens(Continuation1, eof, eof),
            case Return of
                {ok, Tokens, _EndLocation} -> {ok, Tokens};
                {eof, EndLocation} -> {ok, [{eof, EndLocation}]};
                {error, ErrorInfo, _EndLocation} -> {error, ErrorInfo}
            end
    end.

process_file(File) -> process_file(File, convert2CompilerOptions(File)).

%% Preprocessor Options
process_file(File, Options) ->
    case scan_file(File) of
        {ok, Tokens} -> process_tokens(Tokens, transformOptions(File,Options));
        Error -> Error
    end.

process_tokens(Tokens) -> process_tokens(Tokens, []).

process_tokens(Tokens, Options) -> aleppo:process_tokens(Tokens, Options).

parse_file(File) -> parse_file(File, convert2CompilerOptions(File)).

parse_file(File, Options) ->
    case process_file(File, transformOptions(File,Options)) of
        {ok, Tokens} -> parse_tokens(Tokens);
        Error -> Error
    end.

parse_tokens(Tokens) ->
    NewTokens = proplists:delete(eof, Tokens),
    TokenGroups = grouping_tokens(NewTokens),
    OriginForms = lists:map(fun hberl_parser:parse_form/1,TokenGroups),
    case proplists:get_value(error, OriginForms) of 
        undefined -> {ok, [Form || {ok,Form} <- OriginForms]};
        Reason -> {error, Reason}
    end.

%% **************************************************************
%%                      Internal Functions
%% **************************************************************
convert2CompilerOptions(File) ->
     ModuleName = filename:rootname(filename:basename(File)),
     [{file, File}, {module, ModuleName}].

transformOptions(File, Options) ->
    ModuleName = filename:rootname(filename:basename(File)),
    Options1 = case proplists:get_value(module, Options) of
        undefined -> [{module,ModuleName} | proplists:delete(module,Options)];
        _ -> Options
    end,
    case proplists:get_value(file, Options) of
        undefined -> [{file,File} | Options1];
        _ -> Options1
    end.

fetchFileData(File) -> 
    case file:read_file(File) of
        {ok, Data} -> {ok, Data};
        {error, Reason} -> 
            {error, format2binary(<<"errcode:~p,read file '~s' failed">>, 
                [Reason, File])}
    end.

formatError({error, ErrMsg}) when is_binary(ErrMsg) -> {error, ErrMsg};
formatError({error, {Line,Mod,Desc}}) -> 
    {error, format2binary(<<"Line:~p, Module:~p, Description:~s">>,
        [Line, Mod, Desc])};
formatError(Error) -> Error.

grouping_tokens(Tokens) -> grouping_tokens(Tokens, [], []).

grouping_tokens([{dot,Line} | Ts], Tokens, TokenGroups) ->
    TokenGroup = lists:reverse([{dot,Line} | Tokens]),
    grouping_tokens(Ts, [], [TokenGroup | TokenGroups]);
grouping_tokens([Token | Ts], Tokens, TokenGroups) ->
    grouping_tokens(Ts, [Token | Tokens], TokenGroups);
grouping_tokens([], Tokens, TokenGroups) ->
    NewTokenGroups = case Tokens of
        [] -> TokenGroups;
        _ -> [Tokens | TokenGroups]
    end,
    lists:reverse(NewTokenGroups).

processforms(Forms,{Ctx,Trav}) ->
    NewForms = lists:foldl(fun(X,S) ->
        lists:keydelete(X,3,S)
    end, Forms, [module,export,hbfunmap,file]),
    ModAtm = list_to_atom(Ctx#erl_ctx.module),
    ModForm = {attribute, 1, module, ModAtm},
    ExportMaps = fetch_export_maps(Trav),
    ExportFuns = lists:foldl(fun({{_Fun, Arity}, FunAtm}, S) ->
        [{FunAtm, Arity} | S]
    end, [], ExportMaps),
    ExportForm = {attribute, 2, export, [{hbfunmap, 0} | ExportFuns]},
    {[ModForm, ExportForm | NewForms], ExportMaps}.

parse_transform(ErlParseTree) -> 
    parse_transform(ErlParseTree, #erl_ctx{}, #trav{}).

parse_transform(ErlParseTree, Ctx) -> 
    parse_transform(ErlParseTree, Ctx, #trav{}).
    
parse_transform(ErlParseTree, Ctx, Trav) when is_list(ErlParseTree) ->
    FunArityList = [{Fun, Arity} || {function, _, Fun, Arity, _} <- ErlParseTree],
    Trav1 = lists:foldl(fun ({Fun, Arity}, FTrav) ->
        case name_search({function, Fun}, FTrav#trav.erl_scopes, []) of
            not_found -> 
                {FunAtm, FTrav1} = build_fun_name(Fun, FTrav),
                FTrav2 = wrap_store({function, Fun}, FunAtm, FTrav1),
                wrap_store({function, Fun, Arity}, FunAtm, FTrav2);
            {ok, FunAtm} -> wrap_store({function, Fun, Arity}, FunAtm, FTrav)
        end
    end, Trav, FunArityList),
    {Forms, {Ctx1, Trav2}} = lists:mapfoldl(fun ast/2, {Ctx, Trav1}, ErlParseTree),
    FlatForms = lists:flatten(Forms),
    trace(?MODULE, ?LINE, "parse_transform", FlatForms, Ctx1),
    {ok, {FlatForms, {Ctx1, Trav2}}}.

do_compile(ErlParseTree, Ctx) ->
    case catch parse_transform(ErlParseTree, Ctx) of
        {ok, {Forms,_}} ->  compile_forms(Forms, Ctx);
        Error -> Error
    end.

compile_forms(Forms, Ctx) ->
    CompileOptions = case Ctx#erl_ctx.verbose of
        true -> [debug_info, verbose, report_errors, report_warnings];
        _ -> []
    end, 
    case compile:forms(Forms, CompileOptions) of
        {ok, Mod, Bin} ->
            Path = filename:join([Ctx#erl_ctx.out_dir, atom_to_list(Mod) ++ ".beam"]),
            case file:write_file(Path, Bin) of
                ok ->
                    code:purge(Mod),
                    case code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin) of
                        {module, _} -> ok;
                        _ -> {error, <<"code reload failed">>}
                    end;
                {error, Reason} ->
                    {error, format2binary(<<"errcode:~p,beam generation failed">>, [Reason])}
            end;
        error -> {error, <<"compilation failed">>}
    end.

%% ************************************************
%%      AST
%%-------------------------------------------------
%%  Module declarations and forms
%%-------------------------------------------------
%%  {attribute,Line,module,Module}
%%  {attribute,Line,export,Exports}
%%  {attribute,Line,record,{Name,Inits}}
%%  {attribute,Line,file,{Name,Line}}
%% ************************************************
ast({attribute, Line, module, Module}, {Ctx, Trav}) ->
    ModAtm = list_to_atom(Ctx#erl_ctx.module),
    {{attribute,Line,module,ModAtm},{Ctx,wrap_store({module,Module},ModAtm,Trav)}};
    %{{attribute, Line, module, {ModAtm, [mydb, myfile]}}, 
    %    {Ctx, wrap_store({module, Module}, Module, Trav)}};

ast({attribute, Line, export, Exports}, {Ctx, Trav}) ->
    MetaFun = 
    fun ({Fun, Arity}, {FCtx, FTrav}) ->
        {FunAtm, FTrav1} = 
        case name_search({function, Fun, Arity}, FTrav#trav.erl_scopes, []) of
            not_found ->
                throw({error, format2binary(<<"Line:~p,function '~s/~p' undefined">>, 
                    [Line, Fun, Arity])});
            {ok, Val} ->
                {Val, wrap_store({export, Fun, Arity}, Val, FTrav)}
        end,
        {{FunAtm, Arity}, {FCtx, FTrav1}}
    end,
    {AstExports, {_, Trav1}} = lists:mapfoldl(MetaFun, {Ctx, Trav}, Exports),
    {[{attribute, Line, export, AstExports},
        {attribute, Line, hbfunmap, fetch_export_maps(Trav1)}], {Ctx, Trav1}};

ast({attribute, _Line, file, {_File, _FLine}}, {Ctx, Trav}) ->
    {[], {Ctx,Trav}};
    %%{{attribute, Line, file, {File, FLine}}, {Ctx, Trav}};

ast({attribute, Line, record, {Record, RepInits}}, {Ctx, Trav}) ->
    {RecordAtm, Trav1} = build_atm_name(Record, Trav),
    Trav2 = wrap_store({record, Record}, RecordAtm, Trav1),
    {AstRepInits, {_, Trav3}} = 
        lists:mapfoldl(fun ast/2, {Ctx#erl_ctx{record = Record, action = set}, Trav2}, RepInits),
    {{attribute, Line, record, {RecordAtm, AstRepInits}}, {Ctx, Trav3}};

%% user-defined type attribute support, todo...
ast({attribute, Line, type, {_TypeName,_Type,_Args}}, {_Ctx, _Trav}) ->
    throw({error,format2binary(<<"Line:~p,user-defined type not supported">>, 
        [Line])});

ast({attribute, Line, Attr, _}, {_Ctx, _Trav}) ->
    throw({error, format2binary(<<"Line:~p,user-defined attribute '~w' not supported">>, 
        [Line, Attr])});

ast({typed_record_field, RecordField, _Type}, {Ctx, Trav}) ->
    ast(RecordField, {Ctx, Trav});

ast({record_field, Line, RepF}, {#erl_ctx{record = Record, action = Action} = Ctx, Trav}) ->
    Field = element(3, RepF),
    case Action of
        get ->
            case name_search({record_field, Record, Field}, Trav#trav.erl_scopes, []) of
                not_found ->
                    case Field of
                        "_" -> 
                            {{record_field, Line, {element(1,RepF),
                                element(2,RepF), '_'}}, {Ctx,Trav}};
                        _ -> 
                            throw({error, format2binary(<<"filed '~s' undefined "
                                "in record '~s'">>, [Field, Record])})
                    end;
                {ok, Val} ->
                    AstRepF = {element(1, RepF), element(2, RepF), Val},
                    {{record_field, Line, AstRepF}, {Ctx, Trav}}
            end;
        set ->
            {AstRepF, {_, Trav1}} = ast(RepF, {Ctx, Trav}),
            Trav2 = wrap_store({record_field, Record, Field}, element(3, AstRepF), Trav1),
            {{record_field, Line, AstRepF}, {Ctx, Trav2}}
    end;

ast({record_field, Line, RepF, RepV}, {Ctx, Trav}) ->
    {{record_field, Line, AstRepF}, {_, Trav1}} = ast({record_field, Line, RepF}, {Ctx, Trav}),
    {AstRepV, {_, Trav2}} = ast(RepV, {Ctx, Trav1}),
    {{record_field, Line, AstRepF, AstRepV}, {Ctx, Trav2}};

%% function declaration Name Fc_1 ; ... ; Name Fc_k
ast({function, Line, Fun, Arity, RepFcs}, {Ctx, Trav}) ->
    {FunAtm, Trav1} = 
        case name_search({function, Fun, Arity}, Trav#trav.erl_scopes, []) of 
            not_found ->
                throw({error, format2binary(<<"Line:~p,function '~s/~p' undefined">>, 
                    [Line, Fun, Arity])});
            {ok, Val} ->
                {Val, Trav}
        end,
    {AstRepFcs, {_, Trav2}} = 
        lists:mapfoldl(fun metafc/2, {Ctx, Trav1}, RepFcs),
    {{function, Line, FunAtm, Arity, AstRepFcs}, {Ctx, Trav2}};
            
%% ************************************************
%%          Atomic literals
%% ************************************************
ast({integer, Line, Int}, {Ctx, Trav}) ->
    {{integer, Line, Int}, {Ctx, Trav}};

ast({float, Line, Float}, {Ctx, Trav}) ->
    {{float, Line, Float}, {Ctx, Trav}};

ast({string, Line, String}, {Ctx, Trav}) ->
    {{string, Line, String}, {Ctx, Trav}};

ast({atom, Line, Atom}, {Ctx, Trav}) ->
    {AtomAtm, Trav1} = 
        case atom_search({atom, Atom}, Trav#trav.atms_dict) of
            not_found ->
                build_atm_name(Atom, Trav);
            {ok, Val} ->
                {Val, Trav}
        end,
    {{atom, Line, AtomAtm}, {Ctx, Trav1}};

%% ************************************************
%%          Patterns
%% ************************************************
ast({match, Line, RepLft, RepRgt}, {Ctx, Trav}) ->
    {AstLft, {_, Trav1}} = ast(RepLft, {Ctx#erl_ctx{action=set}, Trav}),
    {AstRgt, {_, Trav2}} = ast(RepRgt, {Ctx, Trav1}),
    {{match, Line, AstLft, AstRgt}, {Ctx, Trav2}};

ast({var, Line, Var}, {#erl_ctx{action = Action} = Ctx, Trav}) ->
    case Action of
        get ->
            case name_search({var, Var}, Trav#trav.erl_scopes, []) of
                not_found ->
                    throw({error, format2binary(<<"Line:~p,variable '~s' unbound">>,
                        [Line, Var])});
                {ok, Val} -> {{var, Line, Val}, {Ctx, Trav}}
            end;
        set ->
            {VarAtm, Trav1} = 
                case name_search({var, Var}, Trav#trav.erl_scopes, []) of
                    not_found -> build_var_name(Var, Trav);
                    {ok, Val} -> {Val, Trav}
                end,
            {{var, Line, VarAtm}, {Ctx, Trav1}}
    end;

ast({tuple, Line, Reps}, {Ctx, Trav}) ->
    {AstReps, {_, Trav1}} = lists:mapfoldl(fun ast/2, {Ctx, Trav}, Reps),
    {{tuple, Line, AstReps}, {Ctx, Trav1}};

ast({nil, Line}, {Ctx, Trav}) ->
    {{nil, Line}, {Ctx, Trav}};

ast({cons, Line, RepH, RepT}, {Ctx, Trav}) ->
    {AstRepH, {_, Trav1}} = ast(RepH, {Ctx, Trav}),
    {AstRepT, {_, Trav2}} = ast(RepT, {Ctx, Trav1}),
    {{cons, Line, AstRepH, AstRepT}, {Ctx, Trav2}};

ast({bin, Line, Reps}, {Ctx, Trav}) ->
    {AstReps, {_, Trav1}} = lists:mapfoldl(fun ast/2, {Ctx, Trav}, Reps),
    {{bin, Line, AstReps}, {Ctx, Trav1}};

%% TODO
ast({bin_element, Line, RepP, RepSize, RepTsl}, {Ctx, Trav}) ->
    {[AstRepP, AstRepSize], {Ctx1, Trav1}} = 
        lists:mapfoldl(fun ast/2, {Ctx, Trav}, [RepP, RepSize]),
    AstRepTsl = 
    case RepTsl of
        default -> default;
        _ -> 
            lists:map(fun(X) -> 
                case lists:member(X, ["integer","float","binary","bytes","bitstring","bits",
                    "utf8","utf16","utf32","signed","unsigned","big","little","native"]) of
                    true -> list_to_atom(X);
                    false ->
                        case X of
                            {"unit",Integer} -> {unit,Integer};
                            {A,Value} -> throw({error,format2binary(<<"Line:~p,bit type "
                                "{~s,~p} undefined">>, [Line,A,Value])});
                            Type -> throw({error,format2binary(<<"Line:~p,bit type ~s "
                                "undefined">>,[Line,Type])})
                        end
                end
            end, RepTsl)
    end,
    {{bin_element, Line, AstRepP, AstRepSize, AstRepTsl}, {Ctx1, Trav1}};

ast({op, Line, Op, RepLft, RepRgt}, {Ctx, Trav}) ->
    {[AstRepLft, AstRepRgt], {_, Trav1}} = 
        lists:mapfoldl(fun ast/2, {Ctx, Trav}, [RepLft, RepRgt]),
    {{op, Line, Op, AstRepLft, AstRepRgt}, {Ctx, Trav1}};

ast({op, Line, Op, Rep}, {Ctx, Trav}) ->
    {AstRep, {_, Trav1}} = ast(Rep, {Ctx, Trav}),
    {{op, Line, Op, AstRep}, {Ctx, Trav1}};

ast({record, Line, Record, RepFields}, {Ctx, Trav}) ->
    case name_search({record, Record}, Trav#trav.erl_scopes, []) of
        not_found ->
            throw({error, format2binary(<<"Line:~p,record '~s' undefined">>, 
                [Line, Record])});
        {ok, RecordAtm} ->
            {AstRepFields, {_, Trav1}} = 
            ast(RepFields, {Ctx#erl_ctx{record = Record}, Trav}),
            {{record, Line, RecordAtm, AstRepFields}, {Ctx, Trav1}}
    end;

%% #Record.Field, not supported in erlang vm???
ast({record_index, Line, Record, RepField}, {Ctx, Trav}) ->
    case name_search({record, Record}, Trav#trav.erl_scopes, []) of
        not_found ->
            throw({error, format2binary(<<"Line:~p,record '~s' undefined">>, [Line, Record])});
        {ok, RecordAtm} ->
            {AstRepField, {_, Trav1}} = 
                ast(RepField, {Ctx#erl_ctx{record = Record, action = get}, Trav}),
            {{record_index, Line, RecordAtm, AstRepField}, {Ctx, Trav1}}
    end;

%% ************************************************
%%          Expressions
%% ************************************************
ast(ExprList, {Ctx, Trav}) when is_list(ExprList) -> 
    {AstExprList, {_, Trav1}} = 
         lists:mapfoldl(fun ast/2, {Ctx, Trav}, ExprList),
    {AstExprList, {Ctx, Trav1}};

%% E_0#Record{Fileld_1 = E_1, ..., Field_k = E_k}
ast({record, Line, RepE, Record, RepFields}, {Ctx, Trav}) ->
    {AstRepE, {Ctx1, Trav1}} = ast(RepE, {Ctx#erl_ctx{action = get}, Trav}),
    {{record, _, RecordAtm, AstRepFields}, {_, Trav2}} =
        ast({record, Line, Record, RepFields}, {Ctx1, Trav1}),
    {{record, Line, AstRepE, RecordAtm, AstRepFields}, {Ctx, Trav2}};

%% E_0#Record.Field
ast({record_field, Line, RepE, Record, RepField}, {Ctx, Trav}) ->
    {AstRepE, {Ctx1, Trav1}} = ast(RepE, {Ctx#erl_ctx{action = get}, Trav}),
    {{record_index, _, RecordAtm, AstRepField}, {_, Trav2}} = 
        ast({record_index, Line, Record, RepField}, {Ctx1, Trav1}),
    {{record_field, Line, AstRepE, RecordAtm, AstRepField}, {Ctx, Trav2}};
    
%% catch E_0
ast({'catch', Line, Rep}, {Ctx, Trav}) ->
    {AstRep, {_, Trav1}} = ast(Rep, {Ctx, Trav}),
    {{'catch', Line, AstRep}, {Ctx, Trav1}};

%% E_m:E_0(E_1,E_2, ..., E_k)
ast({call, Line, {remote, RLine, RepM, RepF}, RepParams}, {Ctx, Trav}) ->
    {AstRepM, AstRepF, {_, Trav1}} = 
        check_remote_fun(Line, RepM, RepF, length(RepParams), {Ctx, Trav}),
    {AstRepParams, {_, Trav2}} = lists:mapfoldl(fun ast/2, {Ctx, Trav1}, RepParams),
    {{call, Line, {remote, RLine, AstRepM, AstRepF}, AstRepParams}, {Ctx, Trav2}};

%% E_0(E_1,E_2, ..., E_k)
ast({call, Line, RepF, RepParams}, {Ctx, Trav}) ->
    {AstRepF, {_, Trav1}} = 
    case RepF of
        {atom, FLine, Fun} ->
            Arity = length(RepParams),
            case name_search({function, Fun, Arity}, Trav#trav.erl_scopes, []) of
                 not_found ->
                    case get_mod_func("erlang", Fun, Arity) of
                        true ->
                            {{atom, FLine, list_to_atom(Fun)}, {Ctx, Trav}};
                        false ->
                            throw({error, format2binary(<<"Line:~p,function '~s/~p' undefined">>, 
                                [Line, Fun, Arity])})
                    end;
                {ok, Val} -> {{atom, FLine, Val}, {Ctx, Trav}}
            end;
        _ -> ast(RepF, {Ctx, Trav})
    end,
    {AstRepParams, {_, Trav2}} = lists:mapfoldl(fun ast/2, {Ctx, Trav1}, RepParams),
    {{call, Line, AstRepF, AstRepParams}, {Ctx, Trav2}};

%% [E_0 || W_1, ..., W_k]
ast({lc, Line, RepE, RepGFs}, {Ctx, Trav}) ->
    Trav1 = wrap_add_scope(Trav),
    {AstRepE, {_, Trav2}} = ast(RepE, {Ctx#erl_ctx{action = set}, Trav1}),
    {AstRepGFs, {_, Trav3}} = lists:mapfoldl(fun ast/2, {Ctx, Trav2}, RepGFs),
    {{lc, Line, AstRepE, AstRepGFs}, {Ctx, wrap_del_scope(Trav3)}};

%% <<E_0 || W_1, ..., W_k>>
ast({bc, Line, RepE, RepGFs}, {Ctx, Trav}) ->
    Trav1 = wrap_add_scope(Trav),
    {AstRepE, {_, Trav2}} = ast(RepE, {Ctx#erl_ctx{action = set}, Trav1}),
    {AstRepGFs, {_, Trav3}} = lists:mapfoldl(fun ast/2, {Ctx, Trav2}, RepGFs),
    {{bc, Line, AstRepE, AstRepGFs}, {Ctx, wrap_del_scope(Trav3)}};

%% begin B end
ast({block, Line, RepB}, {Ctx, Trav}) ->
    {AstRepB, {_, Trav1}} = ast(RepB, {Ctx, Trav}),
    {{block, Line, AstRepB}, {Ctx, Trav1}};

%% if Ic_1 ; ... ; Ic_k end
ast({'if', Line, RepIcs}, {Ctx, Trav}) ->
    {AstRepIcs, {_, Trav1}} = ast(RepIcs, {Ctx#erl_ctx{clause_type=if_clause}, Trav}),
    {{'if', Line, AstRepIcs}, {Ctx, Trav1}};

%% case E_0 of Cc_1 ; ... ; Cc_k end,
ast({'case', Line, RepE, RepCcs}, {Ctx, Trav}) ->
    {AstRepE, {_, Trav1}} = ast(RepE, {Ctx, Trav}),
    {AstRepCcs, {_, Trav2}} = ast(RepCcs, {Ctx#erl_ctx{clause_type=case_clause}, Trav1}),
    {{'case', Line, AstRepE, AstRepCcs}, {Ctx, Trav2}};
    %{{'case', Line, AstRepE, AstRepCcs}, {Ctx, Trav2}};

%% try B catch Tc_1 ; ... ; Tc_k end
ast({'try', Line, RepB, [], RepTcs}, {Ctx, Trav}) ->
    {AstRepB, {_, Trav1}} = ast(RepB, {Ctx, Trav}),
    {AstRepTcs, {_, Trav2}} = ast(RepTcs, 
        {Ctx#erl_ctx{clause_type=catch_clause}, Trav1}),
    {{'try', Line, AstRepB, [], AstRepTcs}, {Ctx, Trav2}};

%% try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n end,
ast({'try', Line, RepB, RepCcs, RepTcs}, {Ctx, Trav}) ->
    {AstRepB, {_, Trav1}} = ast(RepB, {Ctx, Trav}),
    {AstRepCcs, {_, Trav2}} = ast(RepCcs,
        {Ctx#erl_ctx{clause_type=case_clause}, Trav1}),
    {AstRepTcs, {_, Trav3}} = ast(RepTcs,
        {Ctx#erl_ctx{clause_type=catch_clause}, Trav2}),
    {{'try', Line, AstRepB, AstRepCcs, AstRepTcs}, {Ctx, Trav3}};

%% try B after A end
ast({'try', Line, RepB, [], [], RepA}, {Ctx, Trav}) ->
    {[AstRepB, AstRepA], {_, Trav1}} = lists:mapfoldl(fun ast/2, {Ctx, Trav}, [RepA, RepB]),
    {{'try', Line, AstRepB, [], [], AstRepA}, {Ctx, Trav1}};

%% try B of Cc_1 ; ... ; Cc_k after A end,
ast({'try', Line, RepB, RepCcs, [], RepA}, {Ctx, Trav}) ->
    {AstRepB, {_, Trav1}} = ast(RepB, {Ctx, Trav}),
    {AstRepCcs, {_, Trav2}} = lists:mapfoldl(fun ast/2, 
        {Ctx#erl_ctx{clause_type=case_clause}, Trav1}, RepCcs),
    {AstRepA, {_, Trav3}} = ast(RepA, {Ctx, Trav2}),
    {{'try', Line, AstRepB, AstRepCcs, [], AstRepA}, {Ctx, Trav3}};

%% try B catch Tc_1 ; ... ; Tc_k after A end,
ast({'try', Line, RepB, [], RepTcs, RepA}, {Ctx, Trav}) ->
    {AstRepB, {_, Trav1}} = ast(RepB, {Ctx, Trav}),
    {AstRepTcs, {_, Trav2}} = lists:mapfoldl(fun ast/2, 
        {Ctx#erl_ctx{clause_type=case_clause}, Trav1}, RepTcs),
    {AstRepA, {_, Trav3}} = ast(RepA, {Ctx, Trav2}),
    {{'try', Line, AstRepB, [], AstRepTcs, AstRepA}, {Ctx, Trav3}};

%%  try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n after A end,
ast({'try', Line, RepB, RepCcs, RepTcs, RepA}, {Ctx, Trav}) ->
    {AstRepB, {_, Trav1}} = ast(RepB, {Ctx, Trav}),
    {AstRepCcs, {_, Trav2}} = lists:mapfoldl(fun ast/2, 
        {Ctx#erl_ctx{clause_type=case_clause}, Trav1}, RepCcs),
    {AstRepTcs, {_, Trav3}} = lists:mapfoldl(fun ast/2, 
        {Ctx#erl_ctx{clause_type=catch_clause}, Trav2}, RepTcs),
    {AstRepA, {_, Trav4}} = ast(RepA, {Ctx, Trav3}),
    {{'try', Line, AstRepB, AstRepCcs, AstRepTcs, AstRepA}, {Ctx, Trav4}};

%%TODO, receive Cc_1 ; ... ; Cc_k end
ast({'receive', Line, _RepCcs}, {_Ctx, _Trav}) ->
    throw({error, format2binary(<<"Line:~p,receive clause not supported">>,[Line])});

%%TODO, receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end
ast({'receive', Line, _RepCcs, _RepE, _RepB}, {_Ctx, _Trav}) ->
    throw({error, format2binary(<<"Line:~p,receive clause not supported">>,[Line])});

%% fun Fun/Arity
ast({'fun', Line, {function, Fun, Arity}}, {Ctx, Trav}) ->
    case name_search({function, Fun, Arity}, Trav#trav.erl_scopes, []) of
        not_found ->
            case get_mod_func("erlang", Fun, Arity) of
                false -> 
                    throw({error,format2binary(<<"Line:~p,function ~s/~p not supported">>,
                        [Line, Fun, Arity])});
                true ->
                    {{'fun', Line, {function, list_to_atom(Fun), Arity}}, {Ctx, Trav}}
            end;
        {ok, Val} ->
            {{'fun', Line, {function, Val, Arity}}, {Ctx, Trav}}
    end;

%% fun Mod:Fun/Arity
ast({'fun', Line, {function, RepMod, RepFun, Arity}}, {Ctx, Trav}) ->
    {AstRepMod, AstRepFun, {_, Trav1}} = 
        check_remote_fun(Line, RepMod, RepFun, Arity, {Ctx, Trav}),
    {{'fun', Line, {function, AstRepMod, AstRepFun, Arity}}, {Ctx, Trav1}};
            
%% fun Fc_1 ; ... ; Fc_k end
ast({'fun', Line, {clauses, RepFcs}}, {Ctx, Trav}) ->
    {AstRepFcs, {_, Trav1}} = 
        lists:mapfoldl(fun metafc/2, {Ctx, Trav}, RepFcs),
    {{'fun', Line, {clauses, AstRepFcs}}, {Ctx, Trav1}};

%%TODO, query [E_0 || W_1, ..., W_k] end
ast({'query', Line, {lc, _LLine, _RepE, _RepWs}}, {_Ctx, _Trav}) ->
    throw({error,format2binary(<<"Line:~p,query clause not supported">>,[Line])});

%%TODO, E_0.Field
ast({record_field, Line, RepE, RepField}, {Ctx, Trav}) ->
    {AstRepE, {_, Trav1}} = ast(RepE, {Ctx, Trav}),
    {AstRepField, {_, Trav2}} = ast(RepField, {Ctx, Trav1}),
    {{record_field, Line, AstRepE, AstRepField}, {Ctx, Trav2}};

%% ************************************************
%%      Generators and filters
%% ************************************************
%% generator P <- E
ast({generate, Line, RepP, RepE}, {Ctx, Trav}) ->
    {[AstRepP, AstRepE], {Ctx1, Trav1}} =
        lists:mapfoldl(fun ast/2, {Ctx, Trav}, [RepP, RepE]),
    {{generate, Line, AstRepP, AstRepE}, {Ctx1, Trav1}};

%% generator P <= E
ast({b_generate, Line, RepP, RepE}, {Ctx, Trav}) ->
    {[AstRepP, AstRepE], {_, Trav1}} =
        lists:mapfoldl(fun ast/2, {Ctx, Trav}, [RepP, RepE]),
    {{b_generate, Line, AstRepP, AstRepE}, {Ctx, Trav1}};

%% ************************************************
%%      Clauses
%%-------------------------------------------------
%% function clause ( Ps ) -> B : {clause, Line, RepPs, [], RepB}
%% function clause ( Ps ) when Gs -> B : {clause, Line, RepPs, RepGs, RepB}
%% if clause Gs -> B : {clause, Line, [], RepGs, RepB}
%% case clause P -> B : {clause, Line, [RepP], [], RepB}
%% case clause P when Gs -> B : clause, Line, [RepP], RepGs, RepB}
%% catch clause P -> B : {clause, Line, [Rep({throw,P,_})],[],RepB}
%% catch clause X:P -> B : R{clause, Line, [Rep({X,P,_})], [], RepB}
%% catch clause P when Gs -> B : {clause, Line, [Rep({throw,P,_})], RepGs, RepB}
%% catch clause X:P when Gs -> B : {clause, Line, [Rep({X,P,_})], RepGs, RepB}
%% ************************************************
ast({clause, Line, RepPs, RepGs, RepB}, {#erl_ctx{clause_type = ClauseType} = Ctx, Trav}) ->
    {AstRepPs, {_, Trav1}} = 
        case ClauseType of
            CT when CT =:= function_clause orelse 
                CT =:= if_clause orelse
                CT =:= case_clause orelse 
                CT =:= catch_clause -> 
                lists:mapfoldl(fun ast/2, {Ctx#erl_ctx{action = set}, Trav}, RepPs);
            undefined -> 
                lists:mapfoldl(fun ast/2, {Ctx#erl_ctx{action = get}, Trav}, RepPs)
        end,
    {[AstRepGs, AstRepB], {_, Trav2}} =
        lists:mapfoldl(fun ast/2, {Ctx, Trav1}, [RepGs, RepB]),
    {{clause, Line, AstRepPs, AstRepGs, AstRepB}, {Ctx, Trav2}};

%% ************************************************
%%      Guards
%% ************************************************   
ast(P, {Ctx, Trav}) ->
    {P, {Ctx, Trav}}.

check_remote_fun(Line, RepM, RepF, Arity, {Ctx, Trav}) ->
     {AstRepM, AstRepF, {_, Trav1}} = 
     case {RepM, RepF} of
        {{atom, MLine, Mod}, {atom, FLine, Fun}} ->
            case name_search({module, Mod}, Trav#trav.erl_scopes, []) of
                not_found ->
                    case get_mod_func(Mod, Fun, Arity) of
                        true ->
                            {{atom, MLine, list_to_atom(Mod)}, 
                            {atom, FLine, list_to_atom(Fun)}, {Ctx, Trav}};
                        false ->
                            throw({error,format2binary(<<"function ~s:~s/~p not supported">>,
                                [Mod, Fun, Arity])})
                        end;
                {ok, ModVal} ->
                    case name_search({function, Fun, Arity}, Trav#trav.erl_scopes, []) of
                        not_found ->
                             throw({error,format2binary(<<"function ~s:~s/~p undefined">>,
                                [Mod, Fun, Arity])});
                        {ok, FunVal} ->
                            {{atom, MLine, ModVal}, {atom, FLine, FunVal}, {Ctx, Trav}}
                    end
            end;
        _ ->
            throw({error,format2binary(<<"Line:~p,dynamically call remote function "
                "not allowed">>,[Line])})
    end,
    {AstRepM, AstRepF, {Ctx, Trav1}}.

metafc(RepFc, {Ctx, Trav}) ->
    {AstRepFc, {_, Trav1}} = ast(RepFc, {Ctx#erl_ctx{clause_type = function_clause}, wrap_add_scope(Trav)}),
    {AstRepFc, {Ctx, wrap_del_scope(Trav1)}}.

%% ************************************************
%%  init_erl_ctx/1
%% ************************************************
init_erl_ctx(Options) ->
    Ctx = #erl_ctx{},
    #erl_ctx{
        out_dir = proplists:get_value(out_dir, Options,  Ctx#erl_ctx.out_dir),
        include_dirs = proplists:get_value(include_dirs, Options, Ctx#erl_ctx.include_dirs),
        verbose = proplists:get_value(verbose, Options, Ctx#erl_ctx.verbose),
        module = proplists:get_value(module, Options, Ctx#erl_ctx.module),
        reader = proplists:get_value(reader, Options, Ctx#erl_ctx.reader),
        force_recompile = proplists:get_value(force_recompile, Options, Ctx#erl_ctx.force_recompile)}.

%% ********************************************************************
%%  wrap_add_scope/1, wrap_del_scope/1, name_search/3 & atom_search/2
%% ********************************************************************
wrap_add_scope(Trav) -> 
    Trav#trav{erl_scopes = [#scope{} | Trav#trav.erl_scopes]}.

wrap_del_scope(Trav) ->
    Trav#trav{erl_scopes = tl(Trav#trav.erl_scopes)}.

wrap_store(Key, Val, Trav) ->
    [HeadScope | RestScopes] = Trav#trav.erl_scopes,
    Scope = HeadScope#scope{names_dict = dict:store(Key, Val, HeadScope#scope.names_dict)},
    Trav#trav{erl_scopes = [Scope | RestScopes]}.

fetch_export_maps(Trav) ->
    HeadScope = hd(Trav#trav.erl_scopes),
    Dict = dict:filter(fun (X, _Y) -> 
        case X of 
            {export, _, _} -> true;
            _ -> false
        end
    end, HeadScope#scope.names_dict),
    [{{list_to_binary(Fun), Arity}, FunAtm} ||
        {{export, Fun, Arity}, FunAtm} <- dict:to_list(Dict)].

%fetch_func_maps(Trav) ->
%    HeadScope = hd(Trav#trav.erl_scopes),
%    Dict = dict:filter(fun (X, _Y) -> 
%        case X of 
%            {function, _, _} -> true;
%            _ -> false
%        end
%    end, HeadScope#scope.names_dict),
%    [{{list_to_binary(Fun), Arity}, FunAtm} || 
%        {{function, Fun, Arity}, FunAtm} <- dict:to_list(Dict)].

%% TODO...
%fetch_module_map(Trav) ->
%    HeadScope = hd(Trav#trav.erl_scopes),
%    Dict = dict:filter(fun (X, _Y) -> 
%        case X of
%            {module,_} -> true;
%            _ -> false
%        end
%    end, HeadScope#scope.names_dict),
%    dict:to_list(Dict).
name_search(_, [], _) -> not_found;
name_search(Key, [H | T], Tmp) ->
    case dict:find(Key, H#scope.names_dict) of
        {ok, Value} -> {ok, Value};
        error -> name_search(Key, T, [H | Tmp]) 
    end.

atom_search(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Value} -> {ok, Value};
        error -> not_found
    end.

%% ************************************************
%%      Build Functions
%%-------------------------------------------------
%%  build_var_name/2
%%  build_fun_name/2
%% ************************************************
build_atm_name(Atom, Trav) when is_atom(Atom) -> {Atom, Trav};
build_atm_name(Atom, Trav) ->
    case catch list_to_existing_atom(Atom) of
        AtomAtm when is_atom(AtomAtm) ->
            {AtomAtm, Trav};
        _ ->
            AtomAtm = list_to_atom(lists:concat(["a", Trav#trav.atm_counter])), 
            {AtomAtm, Trav#trav{atms_dict = dict:store({atom, Atom}, AtomAtm,
                Trav#trav.atms_dict), atm_counter = Trav#trav.atm_counter + 1}}
    end.

build_var_name(Var, Trav) when is_atom(Var) -> {Var, Trav};       
build_var_name(Var, Trav) ->
    VarAtm = case catch list_to_existing_atom(Var) of
        Val when is_atom(Val) -> Val;
        _ -> 
            case Var of
                [$_ | []] -> '_';
                [$_ | _TVar] ->
                    Head = hd(Trav#trav.erl_scopes),
                    list_to_atom(lists:concat(["_Var", Head#scope.var_counter]));
                _ ->
                    Head = hd(Trav#trav.erl_scopes),
                    list_to_atom(lists:concat(["Var", Head#scope.var_counter]))
            end
        end,
    [HeadScope | RestScopes] = Trav#trav.erl_scopes,
    Scope = HeadScope#scope{names_dict = dict:store({var, Var}, VarAtm, 
                  HeadScope#scope.names_dict), var_counter = HeadScope#scope.var_counter + 1},
    {VarAtm, Trav#trav{erl_scopes = [Scope | RestScopes]}}.
            
build_fun_name(Fun, Trav) ->
    case catch list_to_existing_atom(Fun) of
        FunAtm when is_atom(FunAtm) ->
            {FunAtm, Trav};
        _ ->
            FunAtm = list_to_atom(lists:concat(["hbfun", Trav#trav.fun_counter])),
            Trav1 = Trav#trav{fun_counter = Trav#trav.fun_counter + 1},
            {FunAtm, wrap_store({function, Fun}, FunAtm, Trav1)}
    end.   

%% ************************************************
%%  get_mod_func/3
%% ************************************************
get_mod_func(Mod,Func,Arity) -> hberl_allowedfuns:get_mod_func(Mod,Func,Arity).

%% ************************************************
%%  trace/5, show debug information 
%% ************************************************
trace(Module, Line, Title, Content, Ctx) ->
    case Ctx#erl_ctx.verbose of
        true ->
            io:format("TRACE ~p:~p ~p: ~p~n",[Module, Line, Title, Content]);
        _ -> ok
    end.
