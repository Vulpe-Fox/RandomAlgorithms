# -*- coding: utf-8 -*-
"""
Created on Sun Mar 12 11:32:15 2023

@author: Vulpefox
"""

import csv
import os

# ALTERNATE SCRIPT FOR TEXT FILE INPUT FILE NAMES
# This script will create a text document with a script for
#  banning all the new bot users for Twelve
###

def read_config(path):
    print("Reading from config")
    with open(config_path, 'r') as f:
        line_count = 0
        csv_file_path=""
        for line in f:
            if line_count == 0:
                valuesinline = line.split(": ", 2)
                csv_file_path = valuesinline[1].strip()
                print("CSV File path recorded as: " + csv_file_path)
        print("Complete!")
        return csv_file_path

def read_csv(path):
    print("Reading names from CSV")
    with open(csv_file_path, 'r') as f:
        csv_reader = csv.reader(f, delimiter=',')
        line_count = 0
        output=""
        for row in csv_reader:
            if line_count == 0:
                line_count += 1
            else:
                output += "/ban " + row[0] + "\n"
                print("Command added: "+"/ban "+row[0]+"\n")
                line_count += 1
    print("Complete!")
    return output

def write_output(output):
    print("Writing commands to output")
    with open("output.txt", 'w') as f:
        f.write(output)
    print("Complete!")
            
current_dir = os.getcwd()

config_path = current_dir+"\config.txt"
csv_file_path=read_config(config_path)
output=read_csv(csv_file_path)
write_output(output)
