from io import BytesIO
from errno import (
    ENOTDIR,
    ENOENT,
    EEXIST,
)

from zope.interface import implementer

from twisted.python.filepath import IFilePath, FilePath

DIRECTORY = "directory"
NORMAL = "normal"



class NoCloseBytesIO(BytesIO):
    def close(self):
        pass



class _State(object):
    def __init__(self, type, contents=None):
        self.type = type
        self.contents = contents



class _PathState(object):
    def __init__(self):
        self.paths = {
            MemoryPath(FilePath("/"), self): _State(type=DIRECTORY),
        }


    def get(self, path):
        try:
            return self.paths[path]
        except KeyError:
            raise OSError(ENOENT, path.path)


    def add(self, path, state):
        if self.exists(path):
            raise OSError(EEXIST, path.path)
        else:
            for p in path.parents():
                if not p.isdir():
                    raise OSError(ENOTDIR, path.path)
            self.paths[path] = state


    def exists(self, path):
        return path in self.paths


    def isdir(self, path):
        return self.get(path).type == DIRECTORY


    def children(self, path):
        return list(p for p in self.paths if p.path.startswith(path.path + u"/"))



@implementer(IFilePath)
class MemoryPath(object):
    @property
    def path(self):
        return self._path.path


    def __hash__(self):
        return hash(self.path)


    def __eq__(self, other):
        if isinstance(other, MemoryPath):
            return self.path == other.path
        return NotImplementedError


    def __ne__(self, other):
        if isinstance(other, MemoryPath):
            return self.path != other.path
        return NotImplementedError


    def __repr__(self):
        return "<MemoryPath {}>".format(self.path)


    def __init__(self, path, _state=None):
        self._path = path
        if _state is None:
            _state = _PathState()
        self._state = _state


    def basename(self):
        return self._path.basename()


    def child(self, name):
        return MemoryPath(self._path.child(name), self._state)


    def children(self):
        return self._state.children(self)


    def exists(self):
        return self._state.exists(self)


    def isdir(self):
        return self._state.isdir(self)


    def makedirs(self):
        parents = list(self.parents())
        parents.reverse()
        parents.append(self)
        for path in parents:
            if path.exists():
                if not path.isdir():
                    raise OSError(ENOTDIR, path.path)
            else:
                path.mkdir()


    def parents(self):
        for parent in self._path.parents():
            yield MemoryPath(parent, self._state)


    def mkdir(self):
        self._state.add(self, _State(type=DIRECTORY))


    def create(self):
        contents = NoCloseBytesIO()
        self._state.add(self, _State(type=NORMAL, contents=contents))
        return contents


    def getContent(self):
        return self._state.get(self).contents.getvalue()
