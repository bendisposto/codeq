:- prolog_flag(compiling,_,debugcode).
:- prolog_flag(source_info,_,on).
:- prolog_flag(profiling,_,on).

:- use_module(library(lists)).
:- use_module(library(terms)).

:- op(300, fy, ~~).

:- dynamic exports/3, predicates/5, dynamics/1, in_module/1.

write_exports :-
    exports(Module,Name,Arity),
    format('["~w" "~w" ~w]', [Module,Name,Arity]),
    fail.
write_exports.

write_predicates :-
    findall(pr(Name,Ar), predicates(Name,Ar,_,_,_), ListOfNames),
    remove_dups(ListOfNames,ListOfNames2),
    write_predicates(ListOfNames2).
write_predicates([]).
write_predicates([pr(Name,Ar)|Names]) :-
    write_predicates2(Name,Ar,[],[],0),
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

write_predicates2(Name,Ar,Code,Calls,VC) :-
    retract(predicates(Name,Ar,Args1,Body1,Calls1)),
    bind_args(Args1,VC,VCN),
    bind_args(Body1,VCN,VCN2),
    NewCode = [Args1,Body1|Code],
    append(Calls,Calls1,NewCalls),
    write_predicates2(Name,Ar,NewCode,NewCalls,VCN2).
write_predicates2(Name,Ar,Code,Calls,_VNC) :-
    (dynamics(Name/Ar)
    -> format('{ :name "~w" :arity ~w :code "~w" :dynamic true :calls [',[Name,Ar,Code])
    ;  format('{ :name "~w" :arity ~w :code "~w" :calls [',[Name,Ar,Code])),
    write_calls(Calls),
    write(']}'),nl.
	    
write_calls([]).
write_calls([call(Module,Name,Ar)|Calls]) :-
    format('["~w" "~w" ~w]', [Module,Name,Ar]),
    write_calls(Calls).

write_clj_representation :-
    write('{'), nl,
    in_module(Module),
    format(':module "~w"\n', [Module]),
    write(':exports ['), write_exports, write(']'), nl,
    write(':predicates ['), write_predicates, write(']'), nl,
    write('}').

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
assert_dynamics((X,Y)) :-
    !, assert(dynamics(X)), assert_dynamics(Y).
assert_dynamics(X) :-
    !, assert(dynamics(X)).

analyze((:- module(Name, ListOfExported)), Layout, (:- module(Name,ListOfExported))) :-
    !, assert(in_module(Name)),maplist(assert_exports(Name),ListOfExported).
analyze((:- dynamic(X)), Layout, (:- dynamic(X))) :-
    !, assert_dynamics(X).
%analyze((:- meta(X)), Layout, (:- dynamic(X))) :-
%    !, assert_metas(X).
analyze((:- _),_Layout,(:- true)) :- !.
analyze((?- X),_Layout,(?- X)) :- !.
analyze(end_of_file,_Layout,end_of_file) :- !.

analyze((Head :- Body), [LayoutHead | LayoutSub], (Head :- Body)) :-
    !, layout_sub_term([LayoutHead|LayoutSub],3,SubLay),
    analyze_body(Body,SubLay,Calls),
    functor(Head,Fun,Ar),
    Head =.. [Fun|Args],
    assert(predicates(Fun,Ar,Args,Body,Calls)).
analyze(Fact, _Layout, Fact) :-
    !, functor(Fact,Fun,Ar),
    Fact =.. [Fun|Args],
    assert(predicates(Fun,Ar,Args,'',[])).

user:term_expansion(Term1, Lay1, Tokens1, Term2, [], [codeq | Tokens1]) :-
    nonmember(codeq, Tokens1), % do not expand if already expanded
    analyze(Term1, Lay1, Term2),
    %write(Term1),nl,
    %write(Term2),nl,
    !.