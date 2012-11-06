:- write('{:victory true}'),nl.

layout_sub_term([],_,[]).
layout_sub_term([H|T],N,Res) :-
    (N=<1 -> Res=H ; N1 is N-1, layout_sub_term(T,N1,Res)).

analyze_body(X,_Layout) :- var(X), !.
analyze_body(\+(X),Layout) :-
    !, analyze_body(X,Layout).
analyze_body((_A -> B),Layout) :-
    !, layout_sub_term(Layout,3,Layout2),
    analyze_body(B,Layout2).
analyze_body((A -> B ; C),Layout) :-
    !, layout_sub_term(Layout,2,LayoutAB),
    layout_sub_term(LayoutAB,2,LayoutA),
    layout_sub_term(LayoutAB,3,LayoutB),
    layout_sub_term(Layout,3,LayoutC),
    analyze_body(A,LayoutA),
    analyze_body(B,LayoutB),
    analyze_body(C,LayoutC).
analyze_body(if(A,B,C),Layout) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    layout_sub_term(Layout,4,LayoutC),
    analyze_body(A,LayoutA),
    analyze_body(B,LayoutB),
    analyze_body(C,LayoutC,).
analyze_body((A,B),Layout) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA),
    analyze_body(B,LayoutB).
coverage_counter_body((A;B),Layout) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    coverage_counter_body(A,LayoutA),
    coverage_counter_body(B,LayoutB).
coverage_counter_body(X,Layout) :-
    !.

analyze((:- _),_Layout) :- !.
analyze(end_of_file,_Layout) :- !.

analyze((Head :- Body), [LayoutHead | LayoutSub]) :-
    !, layout_sub_term([LayoutHead|LayoutSub],3,SubLay),
    analyze_body(Body,SubLay),
analyze(Fact, _Layout) :-
    !.

user:term_expansion(Term1, Lay1, Tokens1, Term1, Lay1, [codeq | Tokens1]) :-
    nonmember(codeq, Tokens1), % do not expand if already expanded
    analyze(Term1, Lay1),
    !.