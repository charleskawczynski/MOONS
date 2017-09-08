import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import os
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real,abstract_interfaces_path):

	m_name = 'apply_face_BC_op'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules([''])
	g.module[m_name].read_raw_lines(abstract_interfaces_path+'apply_face_BC_op.f90')

	m_name = 'plane_op'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules([''])
	g.module[m_name].read_raw_lines(abstract_interfaces_path+'plane_op.f90')

	return g