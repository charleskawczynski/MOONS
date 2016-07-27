import os,sys
import main as m
import file_IO as IO
clear = lambda: os.system('cls'); clear()

PS = '\\'
DT.root = os.getcwd()
DT.rel = []
DT.rel.append(os.getcwd()+'IO')

m.run(DT)

# Delete compiled python files
for k in DT.rel:
	DT.append()

IO.delete_pyc_files()
