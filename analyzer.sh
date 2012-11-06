#!/bin/bash
sicstus -l ../codeq_analyzer.pl --goal "use_module('$1'),write_clj_representation,halt."