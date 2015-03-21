#!/usr/bin/python

script = r"""
MD Dir1
MD Dir1\Dir2
CD Dir1
MD Dir4
MD Dir5
MD Dir2\Dir3
MF Dir2\Dir3\readme.txt
MF Dir2\Dir3\file1.txt
CD Dir4
MF temp.dat
DEL C:\Dir1\Dir2\Dir3\readme.txt
DEL C:\Dir1\Dir2\Dir3\file1.txt
RD C:\Dir1\Dir2\Dir3
"""

expected = r"""
C:
|_DIR1
    |_DIR2
    |
    |_DIR4
    |   |_temp.dat
    |
    |_DIR5
"""

import test

test.run(script, expected)

