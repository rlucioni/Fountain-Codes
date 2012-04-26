#!/bin/sh

make operations
echo "CREATING TEST FILE..."
dd if=/dev/random of=test_file bs=1000 count=1
echo "RUNNING LT CODE OPERATIONS..."
./io_operations test_file test_dump 60 10
echo "GENERATING MD5 HASHES..."
TEST_FILE_HASH=`md5 -q test_file`
TEST_DUMP_HASH=`md5 -q test_dump`
echo "COMPARING HASHES..."
if [[ "$TEST_FILE_HASH" = "$TEST_DUMP_HASH" ]]; then 
	echo "PASSED."
else 
	echo "FAILED. $TEST_FILE_HASH vs $TEST_DUMP_HASH"
fi
