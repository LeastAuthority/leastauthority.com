from time import clock
import cProfile

_Profile = cProfile.Profile

def Profile(*a, **kw):
    return _Profile(clock, *a, **kw)

cProfile.Profile = Profile

if __name__ == '__main__':
    cProfile.main()
