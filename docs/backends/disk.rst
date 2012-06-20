====================================
Storing Shares on a Local Filesystem
====================================

The "disk" backend stores shares on the local filesystem. Versions of
Tahoe-LAFS <= 1.9.0 always stored shares in this way.

``[storage]``

``backend = disk``

    This enables use of the disk backend, and is the default.

``readonly = (boolean, optional)``

    If ``True``, the node will run a storage server but will not accept any
    shares, making it effectively read-only. Use this for storage servers
    that are being decommissioned: the ``storage/`` directory could be
    mounted read-only, while shares are moved to other servers. Note that
    this currently only affects immutable shares. Mutable shares (used for
    directories) will be written and modified anyway. See ticket `#390
    <http://tahoe-lafs.org/trac/tahoe-lafs/ticket/390>`_ for the current
    status of this bug. The default value is ``False``.

``reserved_space = (quantity of space, optional)``

    If provided, this value defines how much disk space is reserved: the
    storage server will not accept any share that causes the amount of free
    disk space to drop below this value. (The free space is measured by a
    call to statvfs(2) on Unix, or GetDiskFreeSpaceEx on Windows, and is the
    space available to the user account under which the storage server runs.)

    This string contains a number, with an optional case-insensitive scale
    suffix like "K" or "M" or "G", and an optional "B" or "iB" suffix. So
    "100MB", "100M", "100000000B", "100000000", and "100000kb" all mean the
    same thing. Likewise, "1MiB", "1024KiB", and "1048576B" all mean the same
    thing.

    "``tahoe create-node``" generates a tahoe.cfg with
    "``reserved_space=1G``", but you may wish to raise, lower, or remove the
    reservation to suit your needs.

``expire.enabled =``

``expire.mode =``

``expire.override_lease_duration =``

``expire.cutoff_date =``

``expire.immutable =``

``expire.mutable =``

    These settings control garbage collection, causing the server to
    delete shares that no longer have an up-to-date lease on them. Please
    see `<garbage-collection.rst>`_ for full details.
