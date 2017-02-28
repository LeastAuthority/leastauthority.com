"""
The grid router accepts connections on many different ports destined for
the introducer or storage server of many different grids and proxies those
connections on to the correct Pod based on port number.
"""

__all__ = ["Options", "makeService"]


from ._router import Options, makeService
