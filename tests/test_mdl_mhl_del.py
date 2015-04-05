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
MHL C:\DIR1\EDIR4 c:\dir1
MD C:\DIR1\EDIR4\TEST
MHL C:\DIR1\EDIR4\temp.dat c:\dir1\EDIR4
MHL C:\DIR1\EDIR4\temp.dat c:\dir1\EDIR4\TEST
MDL C:\DIR1\EDIR4\temp.dat c:\dir1
MDL C:\DIR1\EDIR4\temp.dat c:\dir1\GDIR5
DELTREE C:\DIR1\EDIR4
cd c:\dir1\dir2\dir3
DELTREE C:\DIR1\DIR2
"""

expected = r"""
C:
|_DIR1
    |_hlink[C:\DIR1\EDIR4]
    |_DIR2
    |   |_DIR3
    |
    |_EDIR4
    |
    |_GDIR5
"""

import test

test.run(script, expected)

