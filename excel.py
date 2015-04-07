
import sys
sys.path += '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/OpenEye_python2.7_osx_10.8_x64-2014.2.2-py2.7.egg', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyBrain-0.3.1-py2.7.egg', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/Peach-0.3.1-py2.7.egg', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/bitarray-0.8.1-py2.7-macosx-10.8-x86_64.egg', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/fann2-1.0.0-py2.7-macosx-10.8-x86_64.egg', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python27.zip', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-darwin', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-mac', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/plat-mac/lib-scriptpackages', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/lib-tk', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/lib-old', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/readline', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/lib-dynload', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyObjC', '/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/gtk-2.0'

from xlrd import open_workbook
from xlutils.copy import copy

class Rwb():
    wb = None
    file = None

    def __init__(self,file):
        rb = open_workbook(file)
        self.file = file
        self.wb = copy(rb)



    def changecells(self,sheets,cells,vals):
        # for i, set cell[i] on sheets[i] to value vals[i]
        # cell[i] is (row,col) pair.
        # sheets contains sheet index
        if type(sheets) is int:           # test for single change
            cells = [cells]               # because of way single vals get passed R->python
            sheets = [sheets]
            vals = [vals]
        elif (len(sheets) != len(cells)) or (len(cells)!=len(vals)) or (len(sheets) != len(vals)):
            print 'changecells:  incompatible length args.'
            return False

        for i in range(len(sheets)):      # note -1 for change to python 0-based arrays
            sh = self.wb.get_sheet(sheets[i]-1)
            row = cells[i][0] -1
            col = cells[i][1] -1
            val = vals[i]
            sh.write(row,col,val)
        self.wb.save(self.file)
