:- module(escaper,[escaping_format/2]).

:- use_module(library(lists)).
:- use_module(library(codesio)).

escaping_format(FormatString,Arguments) :-
    format_to_codes(FormatString,Arguments,Codes),
    escape_codes_list(Codes,EscapedCodes),
    atom_codes(Atom,EscapedCodes),
    write(Atom).

escape_codes_list([],[]).
escape_codes_list([92|T],[92,92|T2]) :-
    escape_codes_list(T,T2).
escape_codes_list([H|T],[H|T2]) :-
    escape_codes_list(T,T2).