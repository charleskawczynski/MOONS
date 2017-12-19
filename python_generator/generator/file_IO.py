from os import listdir
from os.path import isfile, join
import os
import sys
import shutil

def get_all_files_in_path(file_path):
    return [k for k in listdir(file_path) if isfile(join(file_path, k))]

def get_file_contents(file_path):
    with open(file_path, 'r') as content_file: contents = content_file.read()
    return contents

def read_file_to_list_old(file_name,caller):
    f = open(file_name,'r+')
    L = f.read().split('\n')
    try:
        f = open(file_name,'r+')
        L = f.read().split('\n')
    except:
        L = []
        print(' ********* READ ERROR ********* '+file_name)
    return L

def read_file_to_list(file_name,caller):
    with open(file_name,encoding='utf8') as f: # Accounts for encoded characters
        L = f.read().splitlines() # list of lines
    return L

def read_file_to_string(file_name):
    return '\n'.join(read_file_to_list(file_name,'read_file_to_string'))

def write_string_to_file(file_name,s):
    try:
        f = open(file_name,'w+')
        f.write(s)
        f.close()
    except:
        print(' ********* WRITE ERROR ********* write_string_to_list ')
        print(' ********* WRITE ERROR ********* write_string_to_list ')
        print(' ********* WRITE ERROR ********* write_string_to_list ')
    return

def write_list_to_file(file_name,L):
    f = open(file_name,'w+')
    f.write('\n'.join(L))
    f.close()
    return

def make_path(new_path):
    if not os.path.exists(new_path):
        os.makedirs(new_path)

def delete_entire_tree_safe(d):
    if os.path.exists(d):
        shutil.rmtree(d)

