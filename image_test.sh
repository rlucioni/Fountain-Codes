#!/bin/sh

touch test_dump
echo "CLEANING..."
make clean
make operations
echo "RUNNING LT CODE OPERATIONS ON lenna.jpg..."
./operations lenna.jpg test_dump 300 10
echo "GENERATING MD5 HASHES..."
TEST_FILE_HASH=`md5 -q lenna.jpg`
TEST_DUMP_HASH=`md5 -q test_dump`
echo "COMPARING HASHES..."
if [[ "$TEST_FILE_HASH" = "$TEST_DUMP_HASH" ]]; then 
	echo "OPERATIONS SUCCESSFUL. SEE test_dump FOR THE PROCESSED FILE."
else 
	echo "FAILED. $TEST_FILE_HASH vs $TEST_DUMP_HASH"
fi
