#!/bin/sh

WHICH=$(which which)
BBCC=$(stack exec ${WHICH} bbc)

${BBCC} -o fixed-array-type-error fixed-array-type-error.bb

exit $?
