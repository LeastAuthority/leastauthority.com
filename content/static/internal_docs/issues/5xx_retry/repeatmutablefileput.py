#! /usr/bin/env python

import subprocess, time

putstring = '/home/arc/tahoe-lafs/bin/tahoe put %(localfile)s URI:SSK:%(capability)s' % \
{'localfile':'mutabletestfilecontents', 
 'capability':'ge56tmsdk5wk3dxpiznhgfwxty:w2ax2u5m7mwzq4o5jqob7y5lif46tbthb6td7lhw3nnng2cyn4ra'
}
putcommandlist = putstring.split()

#XXXgetstring = '/home/arc/tahoe-lafs/bin/tahoe put %(localfile)s URI:SSK:%(capability)s' % \
#{'localfile':'mutabletestfilecontents', 
# 'capability':'ge56tmsdk5wk3dxpiznhgfwxty:w2ax2u5m7mwzq4o5jqob7y5lif46tbthb6td7lhw3nnng2cyn4ra'
#}XXX
#XXXgetputcommandlist = putstring.split()XXX


def main():
    HTTPCODE = '200'
    counter = 0
    HTTPNot200Count = 0
    fh = open('mutabletestfilecontents','w')
    fh.write('')
    fh.close()
    trialtime = time.time()
    errorout = open('%s.txt'%trialtime,'a')
    while counter < 10000:
        fh = open('mutabletestfilecontents','a')
        counter = counter + 1
        scount = str(counter)
        fh.write(scount+ '\n')
        fh.close()
        SubProcObj = subprocess.Popen(putcommandlist, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        SubProcObj.wait()
        SubProcComm = SubProcObj.communicate()
        HTTPCODE = SubProcComm[1].split()[0]
        if HTTPCODE != '200':
            HTTPNot200Count = HTTPNot200Count + 1
            thetime = time.time()
            errorstring = "At %s, on the %sth 'put' HTTPCODE is:  %s .\nComm Tuple is %s.\n" % (thetime, counter, HTTPCODE, SubProcComm)
            errorout.write(errorstring)
    
    errorfrequencystr = str((HTTPNot200Count*1.0) / (counter*1.0))
    errorcountstring = "The number of responses to the put request, not headed by HTTP 200 Codes in %s attempts is: %s\n" % (counter, HTTPNot200Count) 
    errorout.write(errorcountstring)
    errorout.close()

if __name__ == '__main__':
    main()
