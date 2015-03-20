#!/bin/sh
corebuild fme.byte && find tests -iname '*.py' -exec '{}' ./fme.byte \;
echo

