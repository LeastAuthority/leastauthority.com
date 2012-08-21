#! /usr/bin/env python

import subprocess, time, os

def main():
    numrequests = 10000
    width = str(len(str(numrequests)))
    prestring = "%."+width+"d start: %s\tstop: %s\tdelta: %.4s \n"
    HTTPCODE = '200'
    counter = 0
    HTTPNot200Count = 0
    trialtime = time.time()
    os.mkdir(str(trialtime))
    datafilename = '%s/testfile'%trialtime 
    fh = open(datafilename,'w')
    fh.write('')
    fh.close()
    errorout = open('%s/errors.txt'%trialtime,'a')
    trialtimes = open('%s/trialtimes.txt'%trialtime,'a')
    putstring = '/home/arc/tahoe-lafs/bin/tahoe put %(localfile)s' % \
        {'localfile':datafilename}
    putcommandlist = putstring.split()
    while counter < numrequests:
        fh = open('%s/testfile'%trialtime,'w')
        counter = counter + 1
        writedata = 'a'*55 + str(counter%10)
        fh.write(writedata)
        fh.close()
        startt = time.time()
        SubProcObj = subprocess.Popen(putcommandlist, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        SubProcObj.wait()
        stopt = time.time()
        deltat = stopt - startt
        deltatout = prestring % (counter, startt, stopt, deltat)
        trialtimes.write(deltatout)
        SubProcComm = SubProcObj.communicate()
        HTTPCODE = SubProcComm[1].split()[0]
        if HTTPCODE != '200':
            HTTPNot200Count = HTTPNot200Count + 1
            thetime = time.time()
            errorstring = "At %s, on the %sth 'put' HTTPCODE is:  %s .\nComm Tuple is %s.\n" % (thetime, counter, HTTPCODE, SubProcComm)
            errorout.write(errorstring)
    
    trialstop = time.time()
    totaltime = (trialstop - trialtime)/3600.
    #errorfrequencystr = str((HTTPNot200Count*1.0) / (counter*1.0))
    errorcountstring = "The number of responses to the put request, not headed by HTTP 200 Codes in %s attempts is: %s\n" % (counter, HTTPNot200Count) 
    errorout.write(errorcountstring)
    errorout.close()
    trialtimes.close()
    print "The %s request trial took a total of %.5s hours." % (numrequests, totaltime)

if __name__ == '__main__':
    main()
