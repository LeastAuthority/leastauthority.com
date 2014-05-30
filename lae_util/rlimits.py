
from resource import getrlimit, setrlimit, RLIMIT_NOFILE


def increase_rlimits():
    (soft, hard) = getrlimit(RLIMIT_NOFILE)
    if soft >= 1024:
        pass
    elif hard >= 1024 and hard < 1000000:
        setrlimit(RLIMIT_NOFILE, (hard, hard))
    else:
        # This one works on OS-X, but it doesn't work on Linux.
        setrlimit(RLIMIT_NOFILE, (-1, -1))
        (new_soft, new_hard) = getrlimit(RLIMIT_NOFILE)
        if new_soft == soft:
            # Probably Cygwin, which ignores -1. Use a real value.
            setrlimit(RLIMIT_NOFILE, (3200, -1))
