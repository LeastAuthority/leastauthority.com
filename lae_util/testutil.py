
import os

from twisted.python.filepath import FilePath


class TestUtilitiesMixin:
    def create_workdir(self):
        tempdir = os.path.join(self.__class__.__module__, self.__class__.__name__,
                               self.shortDescription())
        filepath_of_tempdir = FilePath(tempdir)
        filepath_of_tempdir.makedirs()
        return filepath_of_tempdir

