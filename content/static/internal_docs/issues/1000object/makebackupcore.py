#! /usr/bin/env python

from twisted.python.filepath import FilePath

def main():
    directoryfp = FilePath('filestoprepopulatewith')
    directoryfp.makedirs()
    for filenumber in range(2000):
        fpobj = directoryfp.child(str(filenumber))
        contentstring = 'a'*55 + str(filenumber)
        fpobj.setContent(contentstring)
        base8_664 = 436
        fpobj.chmod(base8_664)

if __name__ == '__main__':
    main()
