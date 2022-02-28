#!/usr/bin/env python3.10

import os
import tempfile
from unittest import TestCase
import subprocess


def run_test(source : str, expected_exit_code : int, expected_stdout : str, expected_stderr : str) -> None:
    tmp_path = tempfile.mktemp()
    source_path = tmp_path + "_source.koa"
    binary_path = tmp_path + "_binary"

    f = open(source_path, "w")
    f.write(source)
    f.close()

    cmd = ["./koak", "-o", binary_path, source_path]
    process = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    exit_code = process.returncode
    stdout = process.stdout.decode("utf-8")
    stderr = process.stderr.decode("utf-8")

    assert exit_code == expected_exit_code
    assert stdout == expected_stdout
    assert stderr == expected_stderr
    
    os.unlink(source_path)
    os.unlink(binary_path)


class Return(TestCase):
    def test_return_void(self):
        run_test(
            '''
            fn main(): () {
                ()
            }'''
            , 0, '', '')


    def test_return_0_as_i32(self):
        run_test(
            '''
            fn main(): i32 {
                0
            }'''
            , 0, '', '')
