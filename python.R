
library(rPython)



pythoninit = function(wbfile){
    python.exec("execfile('excel.py')")
    python.assign('wbfile',wbfile)
    python.exec('wb = Rwb(wbfile)')
}

pythonchange = function(sheetidx,     # sheet indices (1-based from R)
             cells,        # 2d array rbind(celladdr[1],...celladr[N])
             vals){        # values for cells to be set to
# broken?
#    python.method.call('wb','changecells',sheedidx,cells,vals)

    python.exec('wb.
}

