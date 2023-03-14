#!/bin/bash
input=$1
coyc=/home/yuri/prog/compilers/nora/coyc/_build/install/default/bin/coyc
preprocessed="$input.i"
gcc -E -P $input -o $preprocessed
$coyc $preprocessed
gcc out.s -o a.out
