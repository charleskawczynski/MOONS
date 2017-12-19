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
import numpy as np
import write_energies_to_file as WEF
import get_vals as GV
import mesh_class as mesh

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS
print('root = '+root)
print('target = '+target)

Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000,1500,2000]
# Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [0,100]

Re_m_s = [str(x) for x in Re_m]
Re_m_L = [len(str(x)) for x in Re_m]
Re_m_s_LZ = [(max(Re_m_L)-LZ)*'0'+x for LZ,x in zip(Re_m_L,Re_m_s)]
Re_m_s_name = ['Rem'+x for x in Re_m_s]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+r+LDC_path,PS) for (r,k) in zip(Re_m_s_name,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]

# source_files_selected = [source_files[0],source_files[1],source_files[5],source_files[10]]
# source_files_selected = [source_files[2]]
source_files_selected = source_files
# source_files_selected = [source_files[1]]

# print(source_files)
for k in source_files: print(k.root.replace(root,''))

print('################### 1D PRIMITIVE VARIABLES PLOTS ############')
print('')

ME = [0.0]
ME1 = [0.0]
ME0 = [0.0]
B1_over_B0 = [0.0]
B1_over_B = [0.0]
B1x_over_B0 = [0.0]
B1y_over_B0 = [0.0]
B1z_over_B0 = [0.0]
B1x_over_B = [0.0]
B1y_over_B = [0.0]
B1z_over_B = [0.0]

for sf,Rm_LZ,Rm,Rm_val in zip(source_files_selected,Re_m_s_LZ,Re_m_s,Re_m):
	if Rm_val>0.5:
		m_ind = mesh.get_mesh_from_file(sf.mesh_ind)
		m_mom = mesh.get_mesh_from_file(sf.mesh_mom)
		print('Rem = '+Rm)
		Al = 100.0**2.0/(2000.0*Rm_val)
		coeff = 2.0*Al # includes x2 for mirror

		# i_B0 = f3.compute_scalar_field_L2_norm_interior(sf.B0field_raw_z,m_ind,m_mom)
		# i_B1  = f3.compute_vector_field_L2_norm_interior(sf.Bfield_raw_x,sf.Bfield_raw_y,sf.Bfield_raw_z,m_ind,m_mom)
		# i_B1x = f3.compute_scalar_field_L2_norm_interior(sf.Bfield_raw_x,m_ind,m_mom)
		# i_B1y = f3.compute_scalar_field_L2_norm_interior(sf.Bfield_raw_y,m_ind,m_mom)
		# i_B1z = f3.compute_scalar_field_L2_norm_interior(sf.Bfield_raw_z,m_ind,m_mom)

		# ME1_ratio_estimate = coeff*i_B1
		# ME0_ratio.append(i_B0)
		# ME1_ratio_x.append(i_B1x/i_B0)
		# ME1_ratio_y.append(i_B1y/i_B0)
		# ME1_ratio_z.append(i_B1z/i_B0)
		# ME1_ratio.append(i_B1/i_B0)
		# print('')

		i_B0 = f3.compute_scalar_field_L2_norm_interior(sf.B0field_raw_z,m_ind,m_mom)
		i_B  = f3.compute_vector_field_L2_norm_interior_total_field(sf.Bfield_raw_x,sf.Bfield_raw_y,sf.Bfield_raw_z,m_ind,m_mom,[],[],sf.B0field_raw_z)
		i_B1  = f3.compute_vector_field_L2_norm_interior(sf.Bfield_raw_x,sf.Bfield_raw_y,sf.Bfield_raw_z,m_ind,m_mom)
		i_B1x = f3.compute_scalar_field_L2_norm_interior(sf.Bfield_raw_x,m_ind,m_mom)
		i_B1y = f3.compute_scalar_field_L2_norm_interior(sf.Bfield_raw_y,m_ind,m_mom)
		i_B1z = f3.compute_scalar_field_L2_norm_interior(sf.Bfield_raw_z,m_ind,m_mom)

		ME.append(i_B)
		ME1.append(i_B1)
		ME0.append(i_B0)
		B1_over_B0.append(i_B1/i_B0)
		B1_over_B.append(i_B1/i_B)
		B1x_over_B0.append(i_B1x/i_B0)
		B1y_over_B0.append(i_B1y/i_B0)
		B1z_over_B0.append(i_B1z/i_B0)
		B1x_over_B.append(i_B1x/i_B)
		B1y_over_B.append(i_B1y/i_B)
		B1z_over_B.append(i_B1z/i_B)
		print('')

	# print('i_B  = '+str(i_B))
	# print('i_B0 = '+str(i_B0))
	# print('i_B/i_B0 = '+str(i_B/i_B0))
	# i_Bx = f3.compute_scalar_field_L2_norm(sf.Bfield_raw_x,m)
	# i_By = f3.compute_scalar_field_L2_norm(sf.Bfield_raw_y,m)
	# i_Bz = f3.compute_scalar_field_L2_norm(sf.Bfield_raw_z,m)
	# print('i_Bx = '+str(i_Bx))
	# print('i_By = '+str(i_By))
	# print('i_Bz = '+str(i_Bz))
	# print('i_Bx_interior = '+str(i_Bx_interior))
	# print('i_By_interior = '+str(i_By_interior))
	# print('i_Bz_interior = '+str(i_Bz_interior))


file_name = target+'SS_B_energy_components_in_fluid.dat'
var_name_list = ['Re<sub>m</sub>',
'ME',
'ME<sup>1</sup>',
'ME<sup>0</sup>',
'|B<sup>1</sup>|/|B<sup>0</sup>|',
'|B<sup>1</sup>|/|B|',
'|B<sup>1</sup><sub>x</sub>|/|B<sup>0</sup>|',
'|B<sup>1</sup><sub>y</sub>|/|B<sup>0</sup>|',
'|B<sup>1</sup><sub>z</sub>|/|B<sup>0</sup>|',
'|B<sup>1</sup><sub>x</sub>|/|B|',
'|B<sup>1</sup><sub>y</sub>|/|B|',
'|B<sup>1</sup><sub>z</sub>|/|B|',
]
V = [Re_m,
ME,
ME1,
ME0,
B1_over_B0,
B1_over_B,
B1x_over_B0,
B1y_over_B0,
B1z_over_B0,
B1x_over_B,
B1y_over_B,
B1z_over_B]
print('data written to '+file_name)
WEF.write_energies_to_file(file_name,V,var_name_list)


print('\n Finished')

IO.delete_pyc_files()