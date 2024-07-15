#!/bin/python
# encoding: utf-8
import os
import sys
import random
import time

log_path = "./CSR_tests"

std_out_files = os.popen('ls ' + log_path).read().strip().split('\n')
logs = ''
num = 0
num_err = 0

with open(log_path + '/log_testCase_abort.log','w') as file_testcase:
    for std_out_log in std_out_files:
        # print(std_out_log)
        with open(log_path + '/' +std_out_log) as file:
            content = file.read()
            if 'ABORT' in content:
                logs += content+'\n'
                num_err += 1
                print(std_out_log+'is ABORT')
                file_testcase.write(std_out_log + "\n")
            # elif content=='':
                # print(std_out_log+' is empty, not finished or check std_err_log')
            else:
                num += 1

if (logs == ''):
    print(str(num)+' logs are right')
else:
    print(str(num_err)+' error logs, please check it in'+ log_path +'/logs.log')
    with open(log_path + '/logs.log','w') as file:
        file.write(logs)

