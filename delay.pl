% (c) 2009-2012 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
% Heinrich Heine Universitaet Duesseldorf
% This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 

:- module(delay,[delay_findall/4,  delay_setof/5, delay_setof_check/6,
  delay_setof_list/4,
   delay_setof_with_explicit_waitvars/4,
  %delay_once_wait_vars/2, %not used
  delay_call/2, delay_call/3, delay_not/2,
  compute_wait_variables/3]).

:- use_module(tools).

:- use_module(module_information,[module_info/2]).
:- module_info(group,kernel).
:- module_info(description,'Utilities to delay calls until sufficiently instantiated.').
:- module_info(revision,'$Rev: 12181 $').
:- module_info(lastchanged,'$LastChangedDate: 2012-10-17 11:44:34 +0200 (Mi, 17 Okt 2012) $').

:- use_module(self_check).

:- use_module(debug). /* so that calls can call unqualified debug_prints */

:- use_module(error_manager).


:- use_module(tools_printing,[print_term_summary/1]).
:- meta_predicate my_findall(-,0,-,-).
:- if(debug:global_debug_flag).
my_findall(X,P,L,_) :-
   statistics(runtime,[Start,_]),
   findall(X,P,L),
   statistics(runtime,[End,_]),
   Tot is End-Start,
   (Tot>50 -> nl, %nl,print(Call),nl,
                 print('*** FINDALL: '),print_term_summary(P),
                 print('*** exceeded limit: '), print(Tot), print(' ms'),nl,
                 length(L,Len),
                 print('*** SOLUTIONS: '), print(Len), nl
		 ; true).
:- else.
my_findall(X,P,L,ExpectedFinalResult) :- 
 (ExpectedFinalResult==[]
    -> \+(P),L=[]  % we know the end result to be empty; not necessary to try and find all solutions
    ; findall(X,P,L)).
    %; findall(X,(P,print(sol(X)),nl,nl,trace),L)).
:- endif.


my_findall_check(X,P,L,ExpectedFinalResult,TimeOutCode) :- 
  statistics(runtime,[Start,_]),
  call_cleanup((my_findall(X,P,L,ExpectedFinalResult),Ok=true),
      (Ok==true -> true
        ; /* failure or time-out; --> time-out as findall should never fail */
          statistics(runtime,[End,_]), Tot is End-Start,
          (Tot>1000 -> call(TimeOutCode), print('Runtime: '),print(Tot),nl
           ; true) % check if the findall is probably responsible for the time-out
     )).

/* ----------------------------------------- */

/* Below is a findall that delays until all
   non-output variables have become ground.
 Ideally, one would want a delay_findall that
  figures out by itself when it has all the answers
  (by looking at call_residue) and that may
  even progressively instantiate the output list
  such as in:
     delay_findall(X,delay_member(X,[a|T]),R), delay_member(Z,R)
 But this will require more involved machinery.
*/


:- use_module(tools,[remove_variables/3]).
  
:- use_module(library(terms),[term_variables/2]).

:- use_module(kernel_objects,[equal_object/2]).

:- meta_predicate delay_findall(-,0,-,-).
delay_findall(V,G,R,OutputVars) :-
	  term_variables(G,Vars),
	  term_variables(OutputVars,RealOutputVars),
	  remove_variables(Vars,RealOutputVars,WVars),
	  debug_print(9,delay_findall_wait(WVars,Vars,OutputVars)),debug_nl(9),
	  when(ground(WVars), (%print_quoted(findall(V,G,RRes)),
	                      my_findall(V,G,RRes,R), %print_message(findall_res(RRes)),
	                       kernel_objects:equal_object(RRes,R))).
 /* overly restrictive; but may work in some cases */
 /* needs to be improved so that we can detect when G can be executed
  without residue !!!!!!!!!!!!!!!!!!!!! */

:- use_module(store,[l_normalise_values/2]).

:- use_module(b_interpreter,[convert_list_of_expressions_into_set/2]).

% used by probvm
delay_setof_with_explicit_waitvars(V,G,FullSetResult,WaitVariables) :- %print(delay_set_of(V)),nl,
    when(ground(WaitVariables),
         (   my_findall(V,G,RRes,FullSetResult),
             b_interpreter:convert_list_of_expressions_into_set(RRes,SetRes),
             kernel_objects:equal_object(SetRes,FullSetResult))).

:- meta_predicate delay_setof(-,0,-,-,-).
delay_setof(V,G,FullSetResult,WVars,Done) :- %print(delay_set_of(V)),nl,
	  /* term_variables(G,Vars),
	  term_variables(OutputVars,RealOutputVars),
	  ~~mnf(remove_variables(Vars,RealOutputVars,WVars)), */
	  when(ground(WVars), ( my_findall(V,G,RRes,FullSetResult),
	              b_interpreter:convert_list_of_expressions_into_set(RRes,SetRes),
	              kernel_objects:equal_object(SetRes,FullSetResult),
	                       Done=true)).

:- meta_predicate delay_setof_check(-,0,-,-,-,0).
delay_setof_check(V,G,FullSetResult,WVars,Done,TimeOutCode) :- %print(delay_set_of(V)),nl,
	  /* term_variables(G,Vars),
	  term_variables(OutputVars,RealOutputVars),
	  ~~mnf(remove_variables(Vars,RealOutputVars,WVars)), */
	   %% print(wait_vars(WVars,G)),nl, %%
	  when(ground(WVars), ( %print_term_summary(findall(V,G,RRes)),
	                      my_findall_check(V,G,RRes,FullSetResult,TimeOutCode),
	              % print(findall_res(RRes)),nl, %%
	              b_interpreter:convert_list_of_expressions_into_set(RRes,SetRes),
	              % will generate AVL tre
	              % print(delay_setof_result(SetRes,FullSetResult)),nl,  translate:print_bvalue(SetRes),nl,
	                       kernel_objects:equal_object(SetRes,FullSetResult),
	                       Done=true)).

:- meta_predicate delay_setof_list(-,0,-,-).
 /* same as above but Result is a list of elements; */
 /*   the list should not be interpreted as a set, but each element is a value for a parameter */
delay_setof_list(V,G,FullSetResult,OutputVars) :-
	  term_variables(G,Vars),
	  term_variables(OutputVars,RealOutputVars),
	  remove_variables(Vars,RealOutputVars,WVars),
	  when(ground(WVars), ( %print_quoted(findall(V,G,RRes)),nl,
	                      my_findall(V,G,RRes,FullSetResult),
	                %%  print_message(delay_seotof_list_rres(RRes)), %%
	                      ll_norm(RRes,NormRRes),
	                       sort(NormRRes,SNormRRes),
	                %%  print_message(SNormRRes), %%
	                       delay:remove_object_duplicates(SNormRRes,SetRes),
	                 %% print_message(fullres(SetRes)),nl, %%
	                       kernel_objects:equal_object(SetRes,FullSetResult)
	                       )).
	                       
 :- assert_must_succeed(( delay:remove_object_duplicates([[int(1),fd(2,setY)],[int(2),fd(1,setY)]],_Res) )).
 :- assert_must_succeed(( delay:remove_object_duplicates([fd(1,setY),fd(1,setY)],_Res) )).
% should not generate type error !!

ll_norm([],[]) :- !.
ll_norm([El1|T1],[El2|T2]) :- !,store:l_normalise_values(El1,El2), ll_norm(T1,T2).

%norm([],Y) :- !,Y=[].
%norm([H|T],Y) :- !, store:l_normalise_values([H|T],Y).
%norm(X,Y) :- !, print(norm(X,Y)),nl,store:normalise_value(X,Y).

remove_object_duplicates([],[]).
remove_object_duplicates([H|T],Res) :-
   (check_whether_solution_exists(H,T) -> Res = RT ; Res = [H|RT]),
   remove_object_duplicates(T,RT).
   

check_whether_solution_exists(Sol,[H|_T]) :-  is_same_solution(Sol,H).
%%  (is_same_solution(Sol,H) -> true ; check_whether_solution_exists(Sol,T)).
  
  /* Note: solutions are lists of values; one element per variable of the setof */
is_same_solution([],[]) :- !.
is_same_solution([El1|T1],[El2|T2]) :- !,equal_object(El1,El2), is_same_solution(T1,T2).
is_same_solution(X,Y) :- equal_object(X,Y).
      %add_error(is_same_solution,'Illegal args: ',(X,Y)),fail.



compute_wait_variables(WaitTerm,OutputVars,WaitVars) :-
      term_variables(WaitTerm,Vars),
	  term_variables(OutputVars,RealOutputVars),
	  remove_variables(Vars,RealOutputVars,WaitVars).
	  
:- meta_predicate delay_call(0,-,-).
delay_call(Call,WaitTerm,OutputVars) :-
	  compute_wait_variables(WaitTerm,OutputVars,WaitVars),
	  %print_message(delay(Call,WaitVars)),
	  when(ground(WaitVars),Call).

:- meta_predicate delay_call(0,-).
delay_call(Call,OutputVars) :-
	  delay_call(Call,Call,OutputVars).

:- meta_predicate delay_not(0,-).
delay_not(Call,OutputVars) :-  %print_message(informational,delay_not(Call,OutputVars)),
   delay_call(      %% ( print(neg_call(Call)),nl,
             \+(Call) ,
                    %%  print(done_neg_call(Call)),nl),
               OutputVars).

/*
:- meta_predicate delay_once_wait_vars(0,-).
delay_once_wait_vars(Call,WaitVars) :-
   when(ground(WaitVars),(Call -> true)).
*/



   /*
s(X) :- p(X,_Y).
p(X,Y) :- when(ground(X),(print(call(q(X,Y))),nl,q(X,Y))).
q(a,c).
q(b,d).
q(b,e).

test(X,Res) :- delay_findall(f(X),s(X),Res,[]).
test2(X,Res) :- delay_findall(f(X,Y),p(X,Y),Res,[Y]).

t(X,Y,R,R2) :-  print(test),nl,
	            test(X,R),test(Y,R2),
				print(instantiateX),nl,
				X=a,print(instantiateY),nl,
				Y=b.
*/


