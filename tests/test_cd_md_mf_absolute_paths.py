#!/usr/bin/python

script = r"""
MD C:\Dir1
MD Dir1\Dir2
CD Dir1
MD C:\DIR1\EDir4
MD C:\dir1\GDir5
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
    |       |_README.TXT
    |
    |_EDIR4
    |   |_TEMP.DAT
    |
    |_GDIR5
"""

import test

test.run(script, expected)

