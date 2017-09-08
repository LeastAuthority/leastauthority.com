import runpy

def main():
    from sys import argv
    assert argv[1] == b"-o"
    outpath = argv[2]

    del argv[:3]

    from theseus import Tracer
    t = Tracer()
    t.install()
    try:
        runpy.run_path(argv[0], run_name="__main__")
    finally:
        with open(outpath, "wb") as trace_file:
            t.write_data(trace_file)

if __name__ == '__main__':
    main()
