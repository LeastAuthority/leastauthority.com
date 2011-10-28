import time


ISO_TIME_FMT = '%Y-%m-%dT%H:%M:%SZ'

def now():
    return time.strftime(ISO_TIME_FMT, time.gmtime())

def format_iso_time(secs):
    return time.strftime(ISO_TIME_FMT, time.gmtime(secs))


