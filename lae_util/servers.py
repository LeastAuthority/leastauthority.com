
import time
from lae_util.timestamp import format_iso_time


def append_record(fp, *args):
    f = fp.open("a")
    try:
        f.write(",".join([format_iso_time(time.time()),] + map(str, args)) + "\n")
    finally:
        f.close()
