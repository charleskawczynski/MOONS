	module gridFun3D_mod

	type gridFun3D
	private
	real(dpn),dimension(:,:,:),allocatable :: f,dfdh
	type(grid3D) :: grid
	type(grid_node) :: node
	type(grid_cellcenter) :: cellcenter

	type(CentralDiff) :: CD2
	type(upwind_scheme) :: upwind

	type(assign_op) :: assign
	type(addTo_op) :: addTo
	type(subtract_op) :: subtract
	type(multiply_op) :: multiply
	type(divide_op) :: divide
	end type

	call del(gf,gf%subtractFrom,gf%upwind,gf%n,1,2)

	subroutine del(gf,upwind,assign,cc,n,dir)
		implicit none
		type(gridFun3D),intent(inout) :: gf
		type(assign_op),intent(in) :: assign
		type(upwind_scheme),intent(in) :: upwind
		type(grid_cellcenter),intent(in) :: cellcenter
		integer,intent(in) :: n,dir
		dfdh(i,j,k) = 
