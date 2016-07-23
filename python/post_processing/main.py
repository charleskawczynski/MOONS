import os
import funcs as f
clear = lambda: os.system('cls')
clear()


file_path = 'C:\Users\Charlie\Documents\MOONS\Numerical Experiment Matrix'
f.plot_all_files_in_path(file_path,'TKE','Time')
print 'Done'
