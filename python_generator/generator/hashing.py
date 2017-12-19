import GOOFPY_directory as GD
import hashlib
import file_IO as IO
from collections import deque
from os import listdir
from os.path import isfile, join
import os
import copy
import sys
import shutil
import pandas as pd
import glob
from collections import OrderedDict
import re

def hash_dict(d):
    d_new = OrderedDict()
    for key in d:
        FN = hashlib.md5(b''+FN)
        MNs = hashlib.md5(b''+d[key])
        d_new[FN] = MNs
    return d

def hash_dict_str_to_int(d):
    d = {int(k):int(v) for k,v in d.items()}
    d_new = OrderedDict()
    for key in d:
        FN = key
        MNs = d[key]
        print(FN)
        print(MNs)
        FN =   abs(hash(FN)) % (10 ** 8)
        MNs = [abs(hash(x)) % (10 ** 8) for x in MNs]
        d_new[FN] = MNs
    d = d_new
    for key in d:
        print('-------------')
        print(key)
        print(d[key])
    print('**************************************')
    raise NameError('Done in hash_dict_str_to_int')
    return d

def hash_dict_int_to_str(d):
    d_new = OrderedDict()
    for key in d:
        FN = hashlib.md5(b''+FN)
        MNs = hashlib.md5(b''+d[key])
        d_new[FN] = MNs
        hash_object = hashlib.md5(b'Hello World')
        print(hash_object.hexdigest())
    return d
