#!/bin/sh

#
# A script to run tests. Takes four arguments
#
# 1) the binary or script to be run
# 2) the expected exit status
# 3) (FILE) the expected output (of both stdout & stderr)
# 4) test name

if [ x$# != x4 ] ; then
  echo "$# args provided when I want 4"
  exit 1
fi

BIN=$1
EXPECTED_EXIT_STAT=$2
EXPECTED_OUT_FILE=$3
TEST_NAME=$4

echo "Running test ${TEST_NAME}..."

TMP_OUT_FILE="/tmp/bbc-test-$$-$(id -u -n)-${TEST_NAME}.txt"
echo "Writing test output to ${TMP_OUT_FILE}"

./${BIN} > ${TMP_OUT_FILE} 2>&1
EXIT_STAT=$?

if [ ${EXIT_STAT} != ${EXPECTED_EXIT_STAT} ] ; then
  echo -n "Test exited with status ${EXIT_STAT},"
  echo " should be ${EXPECTED_EXIT_STAT} [ FAIL ]"
  exit 1
fi

echo "diff ${TMP_OUT_FILE} ${EXPECTED_OUT_FILE}"
diff ${TMP_OUT_FILE} ${EXPECTED_OUT_FILE}
DIFF_EXIT_STAT=$?
if [ ${DIFF_EXIT_STAT} != 0 ] ; then
  echo "Diff of outputs uncovered differences. [ FAIL ]"
  exit 1
fi

########## MEMORY CHECK

NOLEAKS=$(valgrind ${BIN} 2>&1 | grep "no leaks are possible" | wc -l)
if [ x${NOLEAKS} == x1 ] ; then
  echo "Test assertions all held, but a memory leak was detected."
  echo "For more details run: valgrind ${BIN}"
  echo "[ FAIL ]"
  exit 1
fi



rm -f ${TMP_OUT_FILE}

echo "[ OK ]"
