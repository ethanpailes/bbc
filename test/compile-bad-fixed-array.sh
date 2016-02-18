#!/bin/sh

BBCC=$(stack exec which bbc)

${BBCC} -o fixed-array-type-error fixed-array-type-error.bb

exit $?
