#!/bin/bash
sicstus -l ../codeq_analyzer.pl --goal "prolog_flag(redefine_warnings, _, off),on_exception(X,(use_module('$1'),write_clj_representation,halt),(print(X),nl,halt(1)))."