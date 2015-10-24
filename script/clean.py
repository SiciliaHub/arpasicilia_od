#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import re
import sys

directory_to_explore = sys.argv[1]
new_directory = sys.argv[2]

if not os.path.exists(new_directory):
    os.makedirs(new_directory)

for dirname, dirnames, filenames in os.walk(directory_to_explore):
    for subdirname in dirnames:
        if not os.path.exists(os.path.join(new_directory, subdirname)):
            os.makedirs(os.path.join(new_directory, subdirname))

    for originalFilename in filenames:
        filename = originalFilename.replace(' ', '_')
        if filename.find("_"):
            first_parts = filename.split('_')
            if len(first_parts) == 2:
                zona = first_parts[0]
                second_parts = first_parts[1].split(".")
            
            if len(first_parts) == 3:
                zona = first_parts[0]+ "_" + first_parts[1]
                second_parts = first_parts[2].split(".")

            date = second_parts[0]
            
            if len(date) == 8:
                date = date[-4:] + date[2:4] + date[0:2]
            elif len(date) == 6:
                date = date[-4:] + date[0:2]
                
            new_fullpath = os.path.join(dirname,date + "_" + zona + ".csv")
            new_fullpath = new_fullpath.replace(" ","_")
            new_fullpath = new_fullpath.replace(directory_to_explore, new_directory)
                    
            fullpath = os.path.join(dirname, originalFilename)
            
            original_file_handle = open(fullpath, "r")
            new_file_handle = open(new_fullpath, "w")
            
            for line in original_file_handle:
                if re.match("^\"\d{2}\/\d{2}\/\d{4}(.*)", line):
                    print >> new_file_handle, line,
