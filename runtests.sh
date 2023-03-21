#!/bin/bash
q=/home/yuri/prog/compilers/nora/coyc/coyc.sh
stage=$1
# ./test_compiler.sh ../coyc/coyc_full.sh 1
cd ../write_a_c_compiler
./test_compiler.sh $q $stage
