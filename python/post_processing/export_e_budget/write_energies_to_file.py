import tec_header_funcs as THF
import file_IO as IO

def write_energies_to_file(file_name,V,var_name_list):
	title = file_name.replace('.dat','')
	title = IO.get_file_from_path(title)
	V = list(map(list, zip(*V)))
	n_points = len(V)
	L = THF.create_header_0D(title,var_name_list,n_points)
	for x in V:
		L.append('\t'.join([str(y) for y in x]))
	IO.set_file_contents(file_name,'\n'.join(L))
	return L

