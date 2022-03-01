#!/usr/bin/env python3

import os
import subprocess

# Type hinting tuple for all python3 versions
import sys
if sys.version_info < (3, 9):
    from typing import Tuple as tuple


TEST_SOURCE_DIR = 'tests'

INDENT = '    '

COLOR_GREEN = '\033[92m'
COLOR_RED = '\033[91m'
COLOR_RESET = '\033[0m'

OK = COLOR_GREEN + '[OK]' + COLOR_RESET
KO = COLOR_RED + '[KO]' + COLOR_RESET


def build_project() -> None:
    '''
    Build the project to avoid waiting for compilation during the tests
    '''
    print('Build koak...')
    subprocess.call(['stack', 'build'], stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE)
    print('Build done\n')


def test_run(dir: str, files: list) -> tuple[int, int]:
    '''
    For each files like '*.koa' in the given directory, run the test
    Then, check if exit code is the same as the expected one located '*.code' or equal to 0 if no file is found
    Check if the output is the same as the expected output located in '*.out' or equal to '' if no file is found
    Same for the error output with '*.err' files

    One '*.koa' file is one test
    Return the number of success and failure tests as tuple
    '''
    success_count = 0
    failure_count = 0

    name = os.path.basename(dir)
    indent = INDENT * dir.count('/')

    # Print test suite name
    print(indent + name + ':')

    # for each files end with .koa
    for source in files:
        if source.endswith('.koa'):

            try:
                source = dir + '/' + source
                # compile the source
                binary_path = source[:-4] + '.bin'
                subprocess.call(['stack', 'run', '--', '-o', binary_path,
                                source], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

                # execute the binary and get the stdout, stderr and return code
                process = subprocess.Popen(
                    binary_path, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                stdout, stderr = process.communicate()
                stdout = stdout.decode('utf-8').strip()
                stderr = stderr.decode('utf-8').strip()
                code = process.returncode

                # check return code
                try:
                    with open((source[:-4] + '.code'), 'r') as f:
                        expected_code = int(f.read())
                except FileNotFoundError:
                    expected_code = 0

                if code != expected_code:
                    err = indent + INDENT + INDENT + \
                        'Expected code: ' + str(expected_code) + '\n'
                    err += indent + INDENT + INDENT + 'Got code: ' + str(code)
                    raise Exception(err)

                # check stdout
                try:
                    with open((source[:-4] + '.out'), 'r') as f:
                        expected_stdout = f.read().strip()
                except FileNotFoundError:
                    expected_stdout = ''

                if stdout != expected_stdout:
                    err = indent + INDENT + INDENT + 'Expected stdout: ' + expected_stdout + '\n'
                    err += indent + INDENT + INDENT + 'Got stdout: ' + stdout
                    raise Exception(err)

                # check stderr
                try:
                    with open((source[:-4] + '.err'), 'r') as f:
                        expected_stderr = f.read().strip()
                except FileNotFoundError:
                    expected_stderr = ''

                if stderr != expected_stderr:
                    err = indent + INDENT + INDENT + 'Expected stderr: ' + expected_stderr + '\n'
                    err += indent + INDENT + INDENT + 'Got stderr: ' + stderr
                    raise Exception(err)

                print(indent + INDENT + OK + ' ' + source)
                success_count += 1

            except Exception as e:
                print(indent + INDENT + KO + ' ' + source)
                print(e)
                failure_count += 1

    return success_count, failure_count


def clean() -> None:
    '''
    Delete all generated files
    '''
    os.system('find ' + TEST_SOURCE_DIR + ' -name "*.bin" -type f -delete')


def resume(success_count: int, failure_count: int) -> None:
    '''
    Print the resume of the test
    Exit with 1 if there is at least one failure, 0 otherwise
    '''
    print('\n')

    if failure_count == 0:
        print(COLOR_GREEN + str(success_count) + ' tests passed' + COLOR_RESET)
        exit(0)

    print(COLOR_RED + str(failure_count) + ' tests failed out of ' +
          str(success_count + failure_count) + COLOR_RESET)
    exit(1)


if __name__ == '__main__':

    # build the project
    build_project()

    # initialize counters
    success_count = 0
    failure_count = 0

    # iterate on the tests directory
    for (dir, _, files) in os.walk(TEST_SOURCE_DIR):
        success_count_, failure_count_ = test_run(dir, files)
        success_count += success_count_
        failure_count += failure_count_

    # clean generated files
    clean()

    # resume
    resume(success_count, failure_count)
