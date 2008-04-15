#!/bin/sh

SRC=src/
GDATA=src/com/google/gdata
YOUTUBE=src/com/google/gdata/youtube
CALENDAR=src/com/google/gdata/calendar
TESTS=tests/

#
# Return the number of lines of code for all scala sources
# under the directory given as argument.
#
function countLoc {
    find $1 -name "*.scala" | xargs cat | wc -l
}

echo "LOC in the youtube package:" `countLoc $YOUTUBE`
echo "LOC in the calendar package:" `countLoc $CALENDAR`
echo "LOC in the gdata package: " `countLoc $GDATA`
echo "LOC in the whole source tree: " `countLoc $SRC`
echo "LOC in tests: " `countLoc $TESTS`
