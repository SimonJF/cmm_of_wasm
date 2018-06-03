#!/usr/bin/env python3

# Simple enough script: just runs ./run-test.py on all tests in the WASM test
# suite. It might get more intricate at some point.
import os
import os.path
import pathlib
import subprocess

WASM_TEST_DIR = os.path.abspath("../external/testsuite")

def run_test(test):
    print("Running test", test)
    path = os.path.join(WASM_TEST_DIR, test)
    res = subprocess.run(["./run-test.py", path],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
    print("stdout:", res.stdout)
    print("stderr:", res.stderr)



def main():
    script_files = [
            f for f in os.listdir(WASM_TEST_DIR) if
            pathlib.Path(f).suffix == ".wast"]
    for f in script_files:

        run_test(f)


if __name__ == "__main__":
    main()
