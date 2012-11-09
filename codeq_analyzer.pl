:- prolog_flag(compiling,_,debugcode).
:- prolog_flag(source_info,_,on).
:- prolog_flag(profiling,_,on).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(file_systems)).

:- op(300, fy, ~~).

% set ProB namespaces

% some pathes may be set by environment variables during compilation,
:- dynamic compile_time_env_path/2.
lookup_env_path(Pathname, Varname) :-
    ( environ(Varname,Value) ->
        print('Hard-wired path for alias '),print(Pathname),print(': '),print(Value),nl,
        assert(compile_time_env_path(Pathname,Value))
    ; otherwise ->
        true).
% compile-time pathes
:- lookup_env_path(prob_comp_home,'PROB_COMP_HOME'). % hard-wired version of the runtime_application_path
                                                     % This is used on systems where it absolutely clear where the
                                                     % ProB's application directory will be, namely Debian/Ubuntu packages
% run-time pathes
:- lookup_env_path(prob_home,'PROB_HOME').
:- lookup_env_path(examples,'PROB_EXAMPLES').        % hard-wired pathes, see above (about Debian/Ubuntu packages)
:- lookup_env_path(prob_lib,'PROB_LIB').             % hard-wired pathes, see above (about Debian/Ubuntu packages)
:- lookup_env_path(prob_tcl,'PROB_TCL').

% removes an (optional) trailing /src directory from the path,
% the result will not end with a slash
remove_src_dir(Orig,Dir) :-
    name(Orig,COrig),
    ( append(CDir,"/src/",COrig) -> true
    ; append(CDir,"/src",COrig)  -> true
    ; append(CDir,"/src/proz",COrig) -> true
    ; append(CDir,"/src/proz/",COrig) -> true
    ; append(CDir,"/",COrig)     -> true
    ; otherwise -> COrig=CDir),
    name(Dir,CDir).

% returns the path to the application directory (at run-time)
runtime_application_path(Dir) :-
   ( environ('PROB_HOME',Dir) -> % the user has set the environment variable PROB_HOME and thus overrides any other setting
       true                      % the Rodin plugin used this mechanism 
   ; application_path2(Dir),is_correct_prob_home_path(Dir) -> 
       true
   ; application_path2(D),absolute_file_name(D,D1),get_parent_directory(D1,Dir),
       is_correct_prob_home_path(Dir) ->
       true
   ; compile_time_env_path(prob_comp_home,Dir) ->  % e.g. Debian/Ubuntu systems: the path was hard-wired at compile-time
       true
   ; otherwise ->  % usually a run from source code, where we the current directory is prob/src
       current_directory(Current), remove_src_dir(Current,Dir)).
application_path2(Dir) :-
   \+(prolog_flag(system_type,development)),
   environ('SP_APP_DIR',Dir).  /* /usr/local/bin/sicstus on development systems */

check_if_hard_wired(Alias, _Prefix, _Dir, Full) :-
    compile_time_env_path(Alias,Full),!.
check_if_hard_wired(_Alias, Prefix, Dir, Full) :-
    atom_concat(Prefix,Dir,Full).

set_search_path(Alias, Prefix, Dir) :-
    check_if_hard_wired(Alias, Prefix, Dir, Full),
    %%  print(setting_path(Alias,Full)),nl, %%
    ( catch( user:file_search_path(Alias,Full), _, fail) -> true
    ; otherwise ->
        assertz(user:file_search_path(Alias,Full))).
   
compiletime_application_path(Dir) :-
    compile_time_env_path(prob_comp_home,Dir),!.
compiletime_application_path(Dir) :-
    runtime_application_path(Dir).
set_compile_time_search_pathes :-
    compiletime_application_path(App),
    set_search_path(extension, App, '/extensions'),
    set_search_path(probsrc, App, '/src'),
    set_search_path(probcspsrc, App, '/src/cia'),
    set_search_path(bparser, App, '/src/bparser'),
    set_search_path(plugins, App, '/plugins'),
    set_search_path(abstract_domains, App, '/plugins/absint/abstract_domains'),
    set_search_path(tclsrc, App, '/tcl').
:- set_compile_time_search_pathes.

:- dynamic exports/3, imports/3, imports/1, predicates/7, dynamics/1, metas/1, in_module/1.

in_module('user').

flatten(List,FlatList) :- flatten1(List,[],FlatList).
flatten1([],L,L) :- !.
flatten1([H|T],Tail,List) :- !, flatten1(H,FlatList,List), flatten1(T,Tail,FlatList).
flatten1(NonList,Tail,[NonList|Tail]).

write_exports :-
    exports(Module,Name,Arity),
    format('["~w" "~w" ~w]', [Module,Name,Arity]),
    fail.
write_exports.

write_import1 :-
    imports(Name),
    format('"~w "',[Name]),
    fail.
write_import1.
    
write_import3 :-
    imports(Module,Name,Arity),
    format('["~w" "~w" ~w]', [Module,Name,Arity]),
    fail.
write_import3.

write_predicates :-
    findall(pr(Name,Ar), predicates(Name,Ar,_,_,_,_,_), ListOfNames),
    remove_dups(ListOfNames,ListOfNames2),
    write_predicates(ListOfNames2).
write_predicates([]).
write_predicates([pr(Name,Ar)|Names]) :-
    write_predicates2(Name,Ar,[],[],[],[],0),
    write_predicates(Names).

bind_args(Args,VC,VCN) :-
    term_variables(Args,Variables),
    bind_args2(Variables,VC,VCN).
bind_args2([],X,X).
bind_args2([V|Vs],VC,VCN) :-
    number_codes(VC,CodesVC),
    append("v",CodesVC,Codes),
    atom_codes(V,Codes),
    VCNT is VC + 1,
    bind_args(Vs,VCNT,VCN).

is_dynamic(Name,Ar,':dynamic true') :- dynamics(Name/Ar), !.
is_dynamic(_Name,_Ar,':dynamic false').
is_meta(Name,Ar,':meta true') :- metas(Name/Ar), !.
is_meta(_Name,_Ar,':meta false').

write_predicates2(Name,Ar,Code,Calls,StartLines,EndLines,VC) :-
    retract(predicates(Name,Ar,Args1,Body1,Calls1,StartLine,EndLine)),
    bind_args(Args1,VC,VCN),
    bind_args(Body1,VCN,VCN2),
    NewCode = [Args1,Body1|Code],
    append(Calls,Calls1,NewCalls),
    write_predicates2(Name,Ar,NewCode,NewCalls,[StartLine|StartLines],[EndLine|EndLines],VCN2).
write_predicates2(Name,Ar,Code,Calls,StartLines,EndLines,_VNC) :-
    is_dynamic(Name,Ar,Dynamic), is_meta(Name,Ar,Meta),
    format('{ :name "~w" :arity ~w :code "~w" :startlines ~w :endlines ~w ~w ~w :calls [',[Name,Ar,Code,StartLines,EndLines,Dynamic,Meta]),
    write_calls(Calls),
    write(']}'),nl.
	    
write_calls([]).
write_calls([call(Module,Name,Ar)|Calls]) :-
    format('["~w" "~w" ~w]', [Module,Name,Ar]),
    write_calls(Calls).

write_clj_representation :-
    update_calls_all_preds,
    write('{'), nl,
    in_module(Module),
    format(':module "~w"\n', [Module]),
    write(':exports ['), write_exports, write(']'), nl,
    write(':predicates ['), write_predicates, write(']'), nl,
    write(':import_module ['), write_import1, write(']'), nl,
    write(':import_predicates ['), write_import3, write(']'), nl,
    write('}').

update_calls_all_preds :-
    findall(pred(Name,Ar,Arguments,Body,Calls,Start,End),
	    predicates(Name,Ar,Arguments,Body,Calls,Start,End),
	    ListOfAssertedPreds),
    maplist(update_calls,ListOfAssertedPreds).

update_calls(pred(Name,Ar,Arguments,Body,Calls,Start,End)) :-
    maplist(update_call,Calls,UpdatedCalls),
    retract(predicates(Name,Ar,Arguments,Body,Calls,Start,End)),
    assert(predicates(Name,Ar,Arguments,Body,UpdatedCalls,Start,End)).

update_call(call(Module,Call,Arity),call(Module2,Call,Arity)) :-
    Module = nil
    -> update_module(Call,Arity,Module2)
    ;  Module2 = Module.

update_module(Call,Arity,Module2) :-
    in_module(X), functor(CallAndVar,Call,Arity),
    (predicate_property(X:CallAndVar,built_in) -> Module2 = built_in ;
     predicate_property(X:CallAndVar,imported_from(From)) -> Module2 = From ;
     predicates(Call,Arity,_,_,_,_,_) -> Module2 = X ;
     imports(ModuleI,Name,Arity) -> Module2 = ModuleI ;
     otherwise -> Module2 = foo_error).

layout_sub_term([],_,[]).
layout_sub_term([H|T],N,Res) :-
    (N=<1 -> Res=H ; N1 is N-1, layout_sub_term(T,N1,Res)).

analyze_body(X,_Layout,[call('built_in', 'call', 1)]) :- var(X), !.
analyze_body(\+(X),Layout,[call('built_in','not',1)|Calls]) :-
    !, analyze_body(X,Layout,Calls).
%analyze_body('~~'(X),Layout) :-
%    !, analyze_body(X,Layout).
analyze_body((A -> B),Layout,[call('built_in', '->' , 2)|Calls]) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    append(CallsA,CallsB,Calls).
analyze_body((A -> B ; C),Layout,[call('built_in', '->', 3)|Calls]) :-
    !, layout_sub_term(Layout,2,LayoutAB),
    layout_sub_term(LayoutAB,2,LayoutA),
    layout_sub_term(LayoutAB,3,LayoutB),
    layout_sub_term(Layout,3,LayoutC),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    analyze_body(C,LayoutC,CallsC),
    append(CallsA,CallsB,CallsT), append(CallsT,CallsC,Calls).
analyze_body(if(A,B,C),Layout,[call('built_in', 'if', 3)|Calls]) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    layout_sub_term(Layout,4,LayoutC),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    analyze_body(C,LayoutC,CallsC),
    append(CallsA,CallsB,CallsT), append(CallsT,CallsC,Calls).
analyze_body((A,B),Layout,Calls) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    append(CallsA,CallsB,Calls).
analyze_body((A;B),Layout,Calls) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    append(CallsA,CallsB,Calls).
analyze_body(M:X,Layout,[call(M, Fun, Ar)]) :- !, functor(X,Fun,Ar).
analyze_body(X,Layout,[call(nil, Fun, Ar)]) :- !, functor(X,Fun,Ar).

assert_exports(Name,N/A) :-
    !, assert(exports(Name,N,A)).
assert_imports(Name,N/A) :-
    !, assert(imports(Name,N,A)).
assert_dynamics((X,Y)) :-
    !, assert(dynamics(X)), assert_dynamics(Y).
assert_dynamics(X) :-
    !, assert(dynamics(X)).
assert_metas((X,Y)) :-
    !, assert_metas(X), assert_metas(Y).
assert_metas(Term) :-
    !, functor(Term,Fun,Arg),
    (metas(Fun/Arg) -> true ; assert(metas(Fun/Arg))).

analyze((:- module(Name, ListOfExported)), _Layout, (:- module(Name,ListOfExported))) :-
    !, retract(in_module(_)), assert(in_module(Name)),maplist(assert_exports(Name),ListOfExported).
analyze((:- use_module(Name, ListOfImported)), Layout, (:- true)) :-
    !, maplist(assert_imports(Name),ListOfImported).
analyze((:- use_module(Name)), _Layout, (:- true)) :-
    !, assert(imports(Name)).
analyze((:- dynamic(X)), _Layout, (:- dynamic(X))) :-
    !, assert_dynamics(X).
analyze((:- meta_predicate(X)), _Layout, (:- true)) :-
    !, assert_metas(X).
analyze((:- _),_Layout,(:- true)) :- !.
analyze((?- X),_Layout,(?- X)) :- !.
analyze(end_of_file,_Layout,end_of_file) :- !.

analyze((Head :- Body), [LayoutHead | LayoutSub], (Head :- Body)) :-
    !, layout_sub_term([LayoutHead|LayoutSub],3,SubLay),
    analyze_body(Body,SubLay,Calls),
    functor(Head,Fun,Ar),
    Head =.. [Fun|Args],
    flatten([LayoutHead|LayoutSub],[StartLine|FlatLayout]),
    (FlatLayout = [] -> EndLine = StartLine ; last(FlatLayout,EndLine)),
    assert(predicates(Fun,Ar,Args,Body,Calls,StartLine,EndLine)).
analyze(Fact, Layout, Fact) :-
    !, functor(Fact,Fun,Ar),
    Fact =.. [Fun|Args],
    flatten(Layout,[StartLine|FlatLayout]),
    (FlatLayout = [] -> EndLine = StartLine ; last(FlatLayout,EndLine)),
    assert(predicates(Fun,Ar,Args,'',[],StartLine,EndLine)).

user:term_expansion(Term1, Lay1, Tokens1, Term2, [], [codeq | Tokens1]) :-
    nonmember(codeq, Tokens1), % do not expand if already expanded
    analyze(Term1, Lay1, Term2),
    %write(Term1),nl,
    %write(Term2),nl,
    !.