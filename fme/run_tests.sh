#!/bin/sh
./fmebuild.sh
BISECT_FILE=_build/coverage find tests -iname '*.py' -exec '{}' ./fme.byte \;
cd _build
bisect-report -html ../report coverage*.out
cd ..
echo

