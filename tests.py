#!/usr/bin/env python3

import os
import subprocess

TEST_SOURCE_DIR = 'tests'

INDENT = '    '

COLOR_GREEN = '\033[92m'
COLOR_RED = '\033[91m'
COLOR_RESET = '\033[0m'

OK = COLOR_GREEN + '[OK]' + COLOR_RESET
KO = COLOR_RED + '[KO]' + COLOR_RESET


def test_run(dir: str, files: list):
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


if __name__ == '__main__':

    # build the project
    print('Build koak...')
    subprocess.call(['stack', 'build'], stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE)

    total_success = 0
    total_failure = 0

    # iterate on the tests directory
    for (dir, _, files) in os.walk(TEST_SOURCE_DIR):
        success_count, failure_count = test_run(dir, files)
        total_success += success_count
        total_failure += failure_count

    # clean generated files
    os.system('find ' + TEST_SOURCE_DIR + ' -name "*.bin" -type f -delete')

    # resume
    print()
    print()
    if total_failure == 0:
        print(COLOR_GREEN + str(total_success) + ' tests passed' + COLOR_RESET)
        exit(0)

    print(COLOR_RED + str(total_failure) + ' tests failed out of ' +
          str(total_success + total_failure) + COLOR_RESET)
    exit(1)
