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
MHL temp.dat c:\dir1\dir2\dir3
DELTREE C:\DIR1\DIR2\DIR3
DELTREE C:\DIR1\EDIR4
"""

expected = r"""
C:
|_DIR1
    |_DIR2
    |
    |_EDIR4
    |
    |_GDIR5
"""

import test

test.run(script, expected)

