import time


ISO_TIME_FMT = '%Y-%m-%dT%H:%M:%S%z'

def now():
    return time.strftime(ISO_TIME_FMT)

def format_iso_time(t):
    return time.strftime(ISO_TIME_FMT, t)


