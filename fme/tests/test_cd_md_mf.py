#!/usr/bin/python

script = r"""
MD Dir1
MD Dir1\Dir2
CD Dir1
MD EDir4
MD GDir5
MD Dir2\Dir3
MF Dir2\Dir3\readme.txt
CD EDir4
MF temp.dat
"""

expected = r"""
C:
|_DIR1
    |_DIR2
    |   |_DIR3
    |       |_readme.txt
    |
    |_EDIR4
    |   |_temp.dat
    |
    |_GDIR5
"""

import test

test.run(script, expected)

