#!/usr/bin/python

script = r"""
MD Dir1
MD Dir1\Dir2
MF Dir1\readme.txt
MDL c:\dir1\readme.txt dir1\dir2
MD Dir3
COPY dir1\dir2 Dir3
"""

expected = r"""
C:
|_DIR1
|   |_DIR2
|   |   |_dlink[C:\DIR1\readme.txt]
|   |
|   |_readme.txt
|
|_DIR3
    |_DIR2
        |_dlink[C:\DIR1\readme.txt]
"""

import test

test.run(script, expected)

