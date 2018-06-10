#!/usr/bin/env python3

# Simple enough script: just runs ./run-test.py on all tests in the WASM test
# suite. It might get more intricate at some point.
import os
import os.path
import pathlib
import subprocess

WASM_TEST_DIR = os.path.abspath("../external/testsuite")
STDOUT_NAME = "test_stdout.txt"
STDERR_NAME = "test_stderr.txt"


disabled = [
    # Relies on floats for loop bounds in one test and thus infinite loops.
    # Remove when floats are implemented.
    "loop.wast",

    # Br_table unimplemented in CMM, so skipping this until it is
    "br_table.wast",

    # f32 and f64 take ages and also fail at the moment as they're unimplemented
    "f32.wast",
    "f64.wast"
]


def write_file(filename, contents):
    with open(filename, 'a') as f:
        f.write(contents)

def run_test(test):
    print("Running test", test)
    path = os.path.join(WASM_TEST_DIR, test)
    res = subprocess.run(["./run-test.py", path],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)

    clean_test_logs()
    write_file(STDOUT_NAME, res.stdout.decode("utf-8"))
    write_file(STDERR_NAME, res.stderr.decode("utf-8"))

def main():
    script_files = [
            f for f in os.listdir(WASM_TEST_DIR) if
            pathlib.Path(f).suffix == ".wast"]
    for f in script_files:
        if f not in disabled:
            run_test(f)
        else:
            print("Skipping", f)


if __name__ == "__main__":
    main()
