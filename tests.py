#!/usr/bin/env python3

import argparse
import os
import subprocess
from sys import version_info, platform, argv
from time import time
from multiprocessing import Pool, cpu_count

# Type hinting tuple and list for all python3 versions
if version_info < (3, 9):
    from typing import Tuple as tuple, List as list

TEST_SOURCE_DIR = 'tests'
INDENT = '    '

if platform == "win32":
    # Windows
    BINARY_EXTENSION = '.exe'
else:
    # Linux
    BINARY_EXTENSION = '.bin'


def run_test(source_path: str) -> tuple[bool, str]:
    '''
    Compile and execute the given source file.

    Then, check if exit code is the same as the expected one located '*.code' or equal to 0 if no file is found.
    Check if the output is the same as the expected output located in '*.out' or equal to '' if no file is found.
    Same for the error output with '*.err' files.

    Return True if all tests are passed, False otherwise with output.
    '''

    # redirect print to a string to print in right order
    output = ''

    def custom_print(value):
        nonlocal output
        if type(value) == str:
            output += value + '\n'
        else:
            output += str(value) + '\n'
    print = custom_print

    # store start time to compute execution time
    start_time = time()

    source_path_without_extension = source_path[:-4]

    binary_path = source_path_without_extension + BINARY_EXTENSION
    exit_code_path = source_path_without_extension + '.code'
    output_path = source_path_without_extension + '.out'
    error_path = source_path_without_extension + '.err'

    try:
        # compile the source
        process = subprocess.Popen(['stack', 'run', '--', '-o', binary_path,
                                   source_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        _, p_stderr = process.communicate()

        if source_path.split('/')[-1].startswith('error_'):
            # if compilation is expected to fail
            if process.returncode == 0:
                # fail
                raise Exception('Error: compilation succeed but expected to fail')
            else:
                # success
                execution_time = time() - start_time
                print(f'[OK] {source_path.split("/")[-1]} ({execution_time:.2f}s)')
                return True, output
        else:
            # else check if no compilation error occurred
            if process.returncode != 0:
                # fail
                p_stderr = p_stderr.decode('utf-8').strip()
                raise Exception('Error: compilation failed\n' + p_stderr)

        # execute the binary
        process = subprocess.Popen(
            binary_path, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        # get stdout, stderr and exit code
        got_stdout, got_stderr = process.communicate()
        got_stdout = got_stdout.decode('utf-8').strip()
        got_stderr = got_stderr.decode('utf-8').strip()
        got_exit_code = process.returncode

        # retrieve expected exit code
        exp_exit_code = 0
        if os.path.exists(exit_code_path):
            with open(exit_code_path, 'r') as f:
                exp_exit_code = int(f.read()) & 0xFF

        # check exit code
        if got_exit_code != exp_exit_code:
            err = f'Got exit code: {got_exit_code}\nBut expected: {exp_exit_code}'
            raise Exception(err)

        # retrieve expected output
        exp_stdout = ''
        if os.path.exists(output_path):
            with open(output_path, 'r') as f:
                exp_stdout = f.read()

        # check stdout
        if got_stdout != exp_stdout:
            err = f'Got stdout: {got_stdout}\nBut expected: {exp_stdout}'
            raise Exception(err)

        # retrieve expected error output
        exp_stderr = ''
        if os.path.exists(error_path):
            with open(error_path, 'r') as f:
                exp_stderr = f.read()

        # check stderr
        if got_stderr != exp_stderr:
            err = f'Got stderr: {got_stderr}\nBut expected: {exp_stderr}'
            raise Exception(err)

        # all tests are passed
        execution_time = time() - start_time
        print(f'[OK] {source_path.split("/")[-1]} ({execution_time:.2f}s)')
        has_succeeded = True

    except Exception as e:
        # if an error occurred, then the test has failed
        print('[KO] ' + source_path)
        print(e)
        has_succeeded = False

    # delete the binary
    if os.path.exists(binary_path):
        os.remove(binary_path)

    # return True if all tests are passed, False otherwise
    return has_succeeded, output


def build_project() -> None:
    '''
    Build the project to avoid waiting for compilation during the tests.
    '''
    print('Build koak...')
    ret_code: int = subprocess.call(['stack', 'build'])
    if ret_code != 0:
        raise Exception('Build failed')
    print('Build done\n')


def main(test_paths: list[str], jobs_count: int) -> None:
    # build the project
    build_project()

    # initialize counters
    success_count = 0
    failure_count = 0

    # iterate on each test path
    for test_path in test_paths:
        # remove trailing '/'
        test_path = test_path.rstrip('/')

        if os.path.isdir(test_path):
            # iterate on each subdirectory
            for (dir, _, files) in os.walk(test_path):
                test_suite_name = dir.split('/')[-1]
                indent = dir[len(test_path):].count('/')
                print(indent * INDENT + test_suite_name + ':')

                # retrieve each file in the current directory ending with .koa
                sources = [f'{dir}/{f}' for f in files if f.endswith('.koa')]

                with Pool(processes=jobs_count) as pool:
                    # run tests in parallel
                    for res, output in pool.imap(run_test, sources):
                        if res:
                            # if test has succeeded
                            success_count += 1
                            print((indent + 1) * INDENT + output, end='')
                        else:
                            # if test has failed
                            failure_count += 1
                            print('>' * 80)
                            print((indent + 1) * INDENT + output, end='')
                            print('<' * 80)
        else:
            # if the path is a file, then run the test
            res, output = run_test(test_path)
            if res:
                # if test has succeeded
                success_count += 1
                print(output, end='')
            else:
                # if test has failed
                failure_count += 1
                print('>' * 80)
                print(output, end='')
                print('<' * 80)

    # print summary and exit
    print()

    if failure_count == 0:
        print(f'{success_count} tests passed')
        exit(0)

    print(f'{failure_count} tests failed out of {success_count + failure_count}')
    exit(1)


def arg_check_positive(value):
    v = int(value)
    if v <= 0:
        raise argparse.ArgumentTypeError(
            f'{value} isn\'t a valid positive integer value')
    return v


if __name__ == '__main__':
    # parse arguments
    parser = argparse.ArgumentParser(description='Run integration tests')
    parser.add_argument('-j', '--jobs', type=arg_check_positive, default=cpu_count(),
                        help='run N tests in parallel (default: number of CPU cores)')
    parser.add_argument('TEST_PATH', nargs='*', default=[TEST_SOURCE_DIR],
                        help=f'path to the test directory (default: {TEST_SOURCE_DIR})')
    args = parser.parse_args()

    # run tests
    main(args.TEST_PATH, args.jobs)
