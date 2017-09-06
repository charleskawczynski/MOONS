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
	g.module[m_name].add_raw_lines(['module apply_face_BC_op_mod'])
	g.module[m_name].add_raw_lines(['use current_precision_mod'])
	g.module[m_name].add_raw_lines(['use grid_field_mod'])
	g.module[m_name].add_raw_lines(['use face_SD_mod'])
	g.module[m_name].add_raw_lines(['implicit none'])
	g.module[m_name].add_raw_lines(['private'])
	g.module[m_name].add_raw_lines(['public :: apply_face_BC_op'])
	g.module[m_name].add_raw_lines(['abstract interface'])
	g.module[m_name].add_raw_lines(['  subroutine apply_face_BC_op(GF,surf,FSD,face)'])
	g.module[m_name].add_raw_lines(['    import grid_field,face_SD'])
	g.module[m_name].add_raw_lines(['    implicit none'])
	g.module[m_name].add_raw_lines(['    type(grid_field),intent(inout) :: GF'])
	g.module[m_name].add_raw_lines(['    type(grid_field),intent(in) :: surf'])
	g.module[m_name].add_raw_lines(['    type(face_SD),intent(in) :: FSD'])
	g.module[m_name].add_raw_lines(['    integer,intent(in) :: face'])
	g.module[m_name].add_raw_lines(['  end subroutine'])
	g.module[m_name].add_raw_lines(['end interface'])
	g.module[m_name].add_raw_lines(['end module'])

	m_name = 'plane_op'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules([''])
	g.module[m_name].add_raw_lines(['module plane_op_mod'])
	g.module[m_name].add_raw_lines(['use current_precision_mod'])
	g.module[m_name].add_raw_lines(['use grid_field_mod'])
	g.module[m_name].add_raw_lines(['implicit none'])
	g.module[m_name].add_raw_lines(['private'])
	g.module[m_name].add_raw_lines(['public :: plane_op'])
	g.module[m_name].add_raw_lines(['abstract interface'])
	g.module[m_name].add_raw_lines(['  subroutine plane_op(GF,val)'])
	g.module[m_name].add_raw_lines(['    import grid_field,cp'])
	g.module[m_name].add_raw_lines(['    implicit none'])
	g.module[m_name].add_raw_lines(['    type(grid_field),intent(inout) :: GF'])
	g.module[m_name].add_raw_lines(['    real(cp),intent(in) :: val'])
	g.module[m_name].add_raw_lines(['  end subroutine'])
	g.module[m_name].add_raw_lines(['end interface'])
	g.module[m_name].add_raw_lines(['end module'])

	return g