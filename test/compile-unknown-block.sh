#!/bin/sh

BBCC=$(stack exec which bbc)

${BBCC} -o unknown-block unknown-block.bb

exit $?
