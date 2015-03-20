#!/usr/bin/python

import sys
from subprocess import Popen, PIPE

def _unidiff_output(expected, actual):
    """
    Helper function. Returns a string containing the unified diff of two multiline strings.
    """

    import difflib
    expected=expected.splitlines(1)
    actual=actual.splitlines(1)

    diff=difflib.unified_diff(expected, actual)

    return ''.join(diff)

def run(script, expected):
    if len(sys.argv) < 2:
        print sys.argv[0], "Specify FME executable to be tested"
        return

    exe = sys.argv[1]
    p = Popen([exe, "-"], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate(script)
    if expected[1:] != stdout:
        print sys.argv[0], "Wrong test output. See diff"
        print _unidiff_output(expected[1:], stdout)

