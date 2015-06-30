#!/usr/bin/python

script = r"""
MD Dir1
MD Dir1\Dir2
MF Dir1\readme.txt
MHL c:\dir1\readme.txt dir1\dir2
MD Dir3
COPY dir1 Dir3
DEL dir3\dir1\readme.txt
"""

expected = r"""
C:
|_DIR1
|   |_DIR2
|   |   |_hlink[C:\DIR1\readme.txt]
|   |
|   |_readme.txt
|
|_DIR3
    |_DIR1
        |_DIR2
            |_hlink[C:\DIR1\readme.txt]
"""

import test

test.run(script, expected)

