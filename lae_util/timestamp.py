import time, calendar
from dateutil.parser import parse


ISO_TIME_FMT = '%Y-%m-%dT%H:%M:%SZ'

def format_iso_time(secs):
    return time.strftime(ISO_TIME_FMT, time.gmtime(secs))

def parse_iso_time(timestamp):
    dt = parse(timestamp)
    return calendar.timegm(dt.timetuple())
