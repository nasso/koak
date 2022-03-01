#!/usr/bin/env python3

import os
import subprocess
from sys import version_info, platform

# Type hinting tuple for all python3 versions
if version_info < (3, 9):
    from typing import Tuple as tuple

TEST_SOURCE_DIR = 'tests'
INDENT = '    '

if platform == "win32":
    # Windows
    BINARY_EXTENSION = '.exe'
else:
    # Linux
    BINARY_EXTENSION = '.bin'


def build_project() -> None:
    '''
    Build the project to avoid waiting for compilation during the tests
    '''
    print('Build koak...')
    subprocess.call(['stack', 'build'], stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE)
    print('Build done\n')


def run_test(source_path: str, indent: int) -> bool:
    '''
    Compile and execute the given source file

    Then, check if exit code is the same as the expected one located '*.code' or equal to 0 if no file is found
    Check if the output is the same as the expected output located in '*.out' or equal to '' if no file is found
    Same for the error output with '*.err' files

    Return True if all tests are passed, False otherwise
    '''

    source_path_without_extension = source_path[:-4]

    binary_path = source_path_without_extension + BINARY_EXTENSION
    exit_code_path = source_path_without_extension + '.code'
    output_path = source_path_without_extension + '.out'
    error_path = source_path_without_extension + '.err'

    try:
        # compile the source
        subprocess.call(['stack', 'run', '--', '-o', binary_path,
                        source_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

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
                exp_exit_code = int(f.read())

        # check exit code
        if got_exit_code != exp_exit_code:
            err = (indent + 1) * INDENT + f'Got exit code: {got_exit_code}\n'
            err += (indent + 1) * INDENT + f'But expected: {exp_exit_code}'
            raise Exception(err)

        # retrieve expected output
        exp_stdout = ''
        if os.path.exists(output_path):
            with open(output_path, 'r') as f:
                exp_stdout = f.read()

        # check stdout
        if got_stdout != exp_stdout:
            err = (indent + 1) * INDENT + f'Got stdout: {got_stdout}\n'
            err += (indent + 1) * INDENT + f'But expected: {exp_stdout}'
            raise Exception(err)

        # retrieve expected error output
        exp_stderr = ''
        if os.path.exists(error_path):
            with open(error_path, 'r') as f:
                exp_stderr = f.read()

        # check stderr
        if got_stderr != exp_stderr:
            err = (indent + 1) * INDENT + f'Got stderr: {got_stderr}\n'
            err += (indent + 1) * INDENT + f'But expected: {exp_stderr}'
            raise Exception(err)

        # all tests are passed
        print(indent * INDENT + '[OK] ' + source_path)
        has_succeeded = True

    except Exception as e:
        # if an error occurred, then the test has failed
        print(indent * INDENT + '[KO] ' + source_path)
        print(e)
        has_succeeded = False

    # delete the binary
    if os.path.exists(binary_path):
        os.remove(binary_path)

    # return True if all tests are passed, False otherwise
    return has_succeeded


def run_tests(dir: str, files: list) -> tuple[int, int]:
    '''
    For each files like '*.koa' in the given directory, call run_test()

    One '*.koa' file is one test
    Return the number of success and failure tests as tuple
    '''
    success_count = 0
    failure_count = 0

    name = os.path.basename(dir)
    indent = dir.count('/')

    # print test suite name
    print(indent * INDENT + name + ':')

    # run tests for each files end with .koa
    for source in files:
        if source.endswith('.koa'):
            source_path = dir + '/' + source
            # run the test
            if run_test(source_path, indent + 1):
                success_count += 1
            else:
                failure_count += 1

    return success_count, failure_count


def print_summary(success_count: int, failure_count: int) -> None:
    '''
    Print the summary of the test
    Exit with 1 if there is at least one failure, 0 otherwise
    '''
    print('\n')

    if failure_count == 0:
        print(f'{success_count} tests passed')
        exit(0)

    print(f'{failure_count} tests failed out of {success_count + failure_count}')
    exit(1)


if __name__ == '__main__':

    # build the project
    build_project()

    # initialize counters
    success_count = 0
    failure_count = 0

    # iterate on the tests directory
    for (dir, _, files) in os.walk(TEST_SOURCE_DIR):
        success_count_, failure_count_ = run_tests(dir, files)
        success_count += success_count_
        failure_count += failure_count_

    # print summary
    print_summary(success_count, failure_count)
