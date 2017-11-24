import sys
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, '../lib/')
import convert_N_to_t as CNT
import file_IO as IO
import funcs_1D as f1
import funcs_2D as f2
import funcs_3D as f3
import MOONS_directory as MD

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root = 'D:'+PS+'CHARLIE'+PS+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = 'D:'+PS+'CHARLIE'+PS+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS
print('root = '+root)
print('target = '+target)

R = range(0,10+1)
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in R]
source_files = [k.set_dir(root+'M'+str(r)+LDC_path,PS) for (r,k) in zip(R,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(R,source_files)]


source_files_selected = [source_files[0],source_files[1],source_files[5],source_files[10]]

# print(source_files)
for k in source_files: print(k.root.replace(root,''))

print('################### 1D PRIMITIVE VARIABLES PLOTS ############')
# for i in range(0,11):
	# f1.extract_save_line_from_file(root,source_U[i],target+'U_1D_Rm'+str(i)+'.dat',1,PS)
	# f1.extract_save_line_from_file(root,source_U[i],target+'U_1D_vs_y_Rm'+str(i)+'.dat',2,PS)
	# f1.extract_save_line_from_file(root,source_J[i],target+'J_1D_vs_y_Rm'+str(i)+'.dat',2,PS)
	# f1.extract_save_line_from_file(root,source_U[i],target+'U_1D_vs_z_Rm'+str(i)+'.dat',3,PS)

# for sf in source_files:
	# f1.extract_save_line_from_file(root,sf.Jfield_m,target+'J_1D_vs_z_Rm'+str(sf.ID)+'.dat',3,PS)
	# f1.extract_save_line_from_file(root,sf.Jfield_m,target+'J_1D_vs_x_Rm'+str(sf.ID)+'.dat',1,PS)

print('################### 2D PRIMITIVE VARIABLES PLOTS ############')
for sf in source_files_selected:
	# f2.extract_save_plane_from_file(root,sf.Ufield_m,target+sf.Ufield_m_f,1,PS)
	# f2.extract_save_plane_from_file(root,sf.Bfield_m,target+sf.Bfield_m_f,1,PS)
	# f2.extract_save_plane_from_file(root,sf.Jfield_m,target+sf.Jfield_m_f,1,PS)
	f2.extract_save_plane_from_file(root,sf.Jfield_m,target+'J_plane_x.dat',1,PS)
	f2.extract_save_plane_from_file(root,sf.Jfield_m,target+'J_plane_y.dat',2,PS)

print('################### COMPUTE DIFFERENCE BETWEEN Rm ############')
# f3.export_difference_overlapping_grid(source_B[1],source_B[2],target+'B_Rm100-B_Rm200.dat',1.0/100,1.0/200)
# f3.export_difference_overlapping_grid(source_B[2],source_B[3],target+'B_Rm200-B_Rm300.dat',1.0/200,1.0/300)

# for i in range(1,10):
# 	i1=i; i2=i+1;
# 	s1 = 1.0/(i1*100); s2 = 1.0/(i2*100);
# 	f3.export_difference_overlapping_grid(source_B[i1],source_B[i2],target+'B_Rm'+str(i1)+'00-B_Rm'+str(i2)+'00.dat',s1,s2)

# print('################### GENERATE ENERGY TABLE ############')
# for sf in source_files:
# 	print('source = '+sf.root)
# 	(t,KE           ) = IO.get_SS_coordinate(sf.KE           ); print('KE              = '+str(KE           ))
# 	(t,ME1_fluid    ) = IO.get_SS_coordinate(sf.ME1_fluid    ); print('ME1_fluid       = '+str(ME1_fluid    ))
# 	(t,ME1_conductor) = IO.get_SS_coordinate(sf.ME1_conductor); print('ME1_conductor   = '+str(ME1_conductor))
# 	(t,ME1          ) = IO.get_SS_coordinate(sf.ME1          ); print('ME1             = '+str(ME1          ))
# 	ME1_vacuum = ME1-ME1_conductor;                             print('ME1_vacuum      = '+str(ME1_vacuum   ))
# 	ME1_walls = ME1_conductor-ME1_fluid;                        print('ME1_walls       = '+str(ME1_walls    ))
# 	ME1_fluid_p = ME1_fluid/ME1*100;                            print('ME1_fluid_p     = '+str(ME1_fluid_p  ))
# 	ME1_conductor_p = ME1_conductor/ME1*100;                    print('ME1_conductor_p = '+str(ME1_conductor))
# 	ME1_vacuum_p = ME1_vacuum/ME1*100;                          print('ME1_vacuum_p    = '+str(ME1_vacuum_p ))
# 	ME1_walls_p = ME1_walls/ME1*100;                            print('ME1_walls_p     = '+str(ME1_walls_p  ))
# 	(t,JE_fluid     ) = IO.get_SS_coordinate(sf.JE_fluid     ); print('JE_fluid        = '+str(JE_fluid     ))
# 	(t,JE           ) = IO.get_SS_coordinate(sf.JE           ); print('JE              = '+str(JE           ))
# 	JE_fluid_p = JE_fluid/JE*100;                               print('JE_fluid_p      = '+str(JE_fluid_p   ))


print('\n Finished')

IO.delete_pyc_files()