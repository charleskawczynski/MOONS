import os
if os.name == 'posix':
    pass # this is the operating system name
elif os.name == 'nt':
    clear = lambda: os.system('cls')
    clear()
import inspect; import copy
from collections import OrderedDict
import sys
from pathlib import Path
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = '..'+PS+'generator'
sys.path.append(generatorPath)
import GOOFPY_directory as GD
import generator as g
import fortran_property as FP

import modules.temp as temp
import modules.sim_params as sim_params
import modules.var_set as var_set
import modules.stop_clock as stop_clock
import modules.grid as grid
import modules.block as block
import modules.grid_field as grid_field
import modules.mesh_params as mesh_params
import modules.data_location as data_location
import modules.boundary_conditions as boundary_conditions
import modules.block_field as block_field
import modules.mesh as mesh
import modules.fields as fields
import modules.probe as probe
import modules.time_statistics as time_statistics
import modules.sub_domain as sub_domain
import modules.physical_domain as physical_domain
import modules.mesh_domain as mesh_domain
import modules.apply_face_BC_op as apply_face_BC_op
import modules.procedure_array as procedure_array
import modules.dir_tree as dir_tree
import modules.stitches as stitches
import modules.PCG as PCG
import modules.FFT_solver as FFT_solver
import modules.governing_equations as governing_equations
import modules.MOONS as MOONS

# add_prop parameters:
#     1) name (string)
#     2) data type (integer,real etc.) (string)
#     3) Object / allocatable / Primitive / Parameter (string)
#     4) dimension (int)

# init(var,type,privacy,allocatable,rank,dimension,procedure)
# init(var,type,privacy) # defaults to allocatable = False, rank = 1, dimension = 1, procedure = False

def up_dir(d,n): return str(Path(d).parents[n])
def dir_name(d): return os.path.dirname(d)
def full_path(f): return os.path.abspath(f)

MOONS_dir = up_dir(dir_name(full_path(__file__)),1)+PS
MOONS_dir = MOONS_dir.replace('/',PS).replace('\\',PS)
dir_makefile = MOONS_dir+'makefiles'+PS
makefile_main = dir_makefile+'makefile'

src_pre_generated = dir_makefile+'src_pre_generated.make'
src_generated     = dir_makefile+'src_generated.make'
src_handwritten   = dir_makefile+'src_handwritten.make'
src_all           = dir_makefile+'src_all.make'

vpath_pre_generated = dir_makefile+'vpath_pre_generated.make'
vpath_handwritten   = dir_makefile+'vpath_handwritten.make'
vpath_generated     = dir_makefile+'vpath_generated.make'
vpath_all           = dir_makefile+'vpath_all.make'

PS_make_file     = '$(PS)'
PS_file_sys     = PS
f_ext     = '.f90'

file_main = full_path(__file__)
dir_app = dir_name(file_main)+PS
dir_bin = dir_app+'bin'+PS
dir_code = up_dir(dir_app,1)+PS+'code'+PS
dir_code_handwritten = dir_code+'handwritten'+PS
dir_code_generated = dir_code+'generated_code'+PS
dir_code_pre_generated = dir_code+'pre_generated'+PS
dir_GOOFPY = up_dir(dir_app,0)+PS+'generator'+PS
dir_interface = dir_app+'abstract_interfaces'+PS
abstract_interfaces_path = dir_code_handwritten+'abstract_interfaces'+PS

d = GD.GOOFPY_directory(PS)
d.set_f_ext(f_ext)
d.set_file_main(file_main)
d.set_dir_app(dir_app)
d.set_dir_bin(dir_bin)
d.set_dir_interface(dir_interface)
d.set_dir_code_pre_generated(dir_code_pre_generated)
d.set_dir_code_generated(dir_code_generated)
d.set_dir_code_handwritten(dir_code_handwritten)
d.set_dir_code(dir_code)
d.set_dir_GOOFPY(dir_GOOFPY)
d.set_dir_makefile(dir_makefile)
d.set_makefile_main(makefile_main)
d.set_src_generated(src_generated)
d.set_src_pre_generated(src_pre_generated)
d.set_src_handwritten(src_handwritten)
d.set_src_all(src_all)
d.set_vpath_pre_generated(vpath_pre_generated)
d.set_vpath_handwritten(vpath_handwritten)
d.set_vpath_generated(vpath_generated)
d.set_vpath_all(vpath_all)
d.set_PS_make_file(PS_make_file)
d.set_PS_file_sys(PS_file_sys)

d.unify_path_separator()

make_file_vpaths = []
make_file_vpaths.append((d.dir_code_pre_generated,'$(SRC_DIR_PRE_GENERATED)'+d.PS_make_file))
make_file_vpaths.append((d.dir_code_generated,'$(SRC_DIR_GENERATED)'+d.PS_make_file))
make_file_vpaths.append((d.dir_code_handwritten,'$(SRC_DIR)'+d.PS_make_file))
d.set_make_file_vpaths(make_file_vpaths)

d.compute_common_root()
d.print_local()

g = g.generator()
g.set_directories(d)
g.add_base_files(['precision'+PS+'current_precision.f90'])
g.add_base_files(['IO'+PS+'inquire_funcs.f90'])
g.add_base_files(['IO'+PS+'IO_check.f90'])
g.add_base_files(['IO'+PS+'IO_tools.f90'])
g.add_base_files(['string'+PS+'string.f90'])
g.add_base_files(['string'+PS+'string_aux.f90'])
g.add_base_modules(['string'])
g.add_base_modules(['dir_manip'])
g.add_base_modules(['datatype_conversion'])
# g.print()
priv = 'public'
log = 'logical'
real = 'real(cp)' # ! Double precision (default)
T = True
F = False
g.set_default_real('0.0_cp')

g = temp.add_modules(g,T,F,priv,real)
# --------------- Small data structures
g = stitches.add_modules(g,T,F,priv,real)
g = dir_tree.add_modules(g,T,F,priv,real)

g = probe.add_modules(g,T,F,priv,real)
g = mesh_params.add_modules(g,T,F,priv,real)
g = grid.add_modules(g,T,F,priv,real)
g = var_set.add_modules(g,T,F,priv,real)
g = stop_clock.add_modules(g,T,F,priv,real)
g = sim_params.add_modules(g,T,F,priv,real)

# --------------- Medium data structures
g = grid_field.add_modules(g,T,F,priv,real) # small
# g = data_location.add_modules(g,T,F,priv,real) # Handwritten since fully private
g = sub_domain.add_modules(g,T,F,priv,real)
g = physical_domain.add_modules(g,T,F,priv,real)
g = apply_face_BC_op.add_modules(g,T,F,priv,real,abstract_interfaces_path) # Contains interfaces
g = procedure_array.add_modules(g,T,F,priv,real)
g = boundary_conditions.add_modules(g,T,F,priv,real)

# --------------- Large data structures
g = block.add_modules(g,T,F,priv,real)
g = block_field.add_modules(g,T,F,priv,real)
g = mesh.add_modules(g,T,F,priv,real)
g = fields.add_modules(g,T,F,priv,real)
g = mesh_domain.add_modules(g,T,F,priv,real)
g = time_statistics.add_modules(g,T,F,priv,real)

# --------------- Very large data structures
g = PCG.add_modules(g,T,F,priv,real,abstract_interfaces_path) # Contains interfaces
g = FFT_solver.add_modules(g,T,F,priv,real)
g = governing_equations.add_modules(g,T,F,priv,real)
g = MOONS.add_modules(g,T,F,priv,real)

g.generate_code()
