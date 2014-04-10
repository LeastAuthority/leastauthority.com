
from twisted.python.filepath import FilePath


class TestUtilitiesMixin:
    def create_workdir(self):
        tempdirfp = (FilePath(self.__class__.__module__)
                       .child(self.__class__.__name__)
                       .child(self.shortDescription()))
        tempdirfp.makedirs()
        return tempdirfp

