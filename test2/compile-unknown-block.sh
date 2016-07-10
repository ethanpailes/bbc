#!/bin/sh

WHICH=$(which which)
BBCC=$(stack exec ${WHICH} bbc)

${BBCC} -o unknown-block unknown-block.bb

exit $?
