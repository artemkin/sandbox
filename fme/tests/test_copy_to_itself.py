#!/usr/bin/python

script = r"""
MD Dir1
MD Dir1\Dir2
MF Dir1\readme.txt
COPY Dir1 Dir1
"""

expected = r"""
C:
|_DIR1
    |_DIR1
    |   |_DIR2
    |   |
    |   |_readme.txt
    |
    |_DIR2
    |
    |_readme.txt
"""

import test

test.run(script, expected)

