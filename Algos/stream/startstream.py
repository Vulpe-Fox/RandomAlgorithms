# -*- coding: utf-8 -*-
"""
Created on Sun Mar 12 11:32:15 2023

@author: Vulpefox
"""

import os
import time

# Current working directory
current_dir = os.getcwd()

# Select type
file_selection = input("Select (1) for vr stream, (2) for desktop stream: ")

file_path = ""

if file_selection == "1":

    file_path = current_dir+"\startstreamscript.txt"
elif file_selection == "2": 

    file_path = current_dir+"\startstreamscript - Copy.txt"
else:
    exit()

with open(file_path, "r") as f:
    for line in f:
        valuesinline = line.split(": ", 2)
        print("Now opening " + valuesinline[0] + " from directory: " + valuesinline[1])
        filepath = valuesinline[1].strip()
        os.startfile(filepath)
print("Complete!")
