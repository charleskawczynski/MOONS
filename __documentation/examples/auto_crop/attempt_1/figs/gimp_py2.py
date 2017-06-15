import os
clear = lambda: os.system('cls')
clear()

import sys
sys.path.append('C:\Program Files\GIMP 2\lib\gimp\2.0\plug-ins')
from gimpfu import pdb, main, register, PF_STRING
from gimpenums import ORIENTATION_HORIZONTAL

def flip(file):
    image = pdb.gimp_file_load(file, file)
    drawable = pdb.gimp_image_get_active_layer(image)
    pdb.gimp_image_flip(image, ORIENTATION_HORIZONTAL)
    pdb.gimp_file_save(image, drawable, file, file)
    pdb.gimp_image_delete(image)

args = [(PF_STRING, 'file', 'GlobPattern', '*.*')]
register('python-flip', '', '', '', '', '', '', '', args, [], flip)

main()