#!/usr/bin/env bash

./gracec < $1 > arxeio.ll 2> /dev/null
llc -o arxeio.s arxeio.ll
clang -o a.out arxeio.s libgrace.a
