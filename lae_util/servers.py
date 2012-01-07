import time
from lae_util.timestamp import format_iso_time

def append_record(filename, *args):
    f = open(filename, "a+")
    try:
        f.write(",".join((format_iso_time(time.time()),) + args) + "\n")
    finally:
        f.close()
