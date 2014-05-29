import sys, time, calendar
from dateutil.parser import parse


ISO_TIME_FMT = '%Y-%m-%dT%H:%M:%SZ'

def format_iso_time(secs):
    return time.strftime(ISO_TIME_FMT, time.gmtime(secs))

def parse_iso_time(timestamp):
    try:
        dt = parse(timestamp)
    except TypeError, e:
        # work around for <https://bugs.launchpad.net/dateutil/+bug/1279680>
        raise ValueError, e, sys.exc_info()[2]

    return calendar.timegm(dt.timetuple())
