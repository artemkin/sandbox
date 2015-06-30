#!/usr/bin/python

script = r"""
MD Dir
MD Dir1
MD Dir1\Dir2
CD Dir1\Dir2
MF file2.dat
MD Dir3
CD Dir3
MF file3.dat
MD Dir4
CD Dir4
MF file4.dat
MD Dir5
CD Dir5
MF file5.dat
MHL C:\Dir1\Dir2\Dir3\Dir4\Dir5\file5.dat C:\Dir
CD C:
DELTREE Dir1
MD Dir2
CD Dir2
MF a.txt
MF b.txt
CD C:
MD Dir3
COPY Dir2 Dir3
"""


expected = r"""
C:
|_DIR
|   |_hlink[C:\DIR1\DIR2\DIR3\DIR4\DIR5\file5.dat]
|
|_DIR1
|   |_DIR2
|       |_DIR3
|           |_DIR4
|               |_DIR5
|                   |_file5.dat
|
|_DIR2
|   |_a.txt
|   |_b.txt
|
|_DIR3
    |_DIR2
        |_a.txt
        |_b.txt
"""

import test

test.run(script, expected)

