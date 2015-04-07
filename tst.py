#!/usr/bin/env  python

from xlrd import open_workbook
from xlutils.copy import copy

rb = open_workbook('foo.xls',formatting_info=True,on_demand=True)
sh = rb.sheet_by_name('Verification')
print 'cell c26 = ',sh.cell(25,2)

wb = copy(rb)
wb.get_sheet(1).write(25,2,50)
wb.save('fofo.xls')



