       module boundary_conditions_mod
       ! Making BCs is a 3 step process:
       !
       !       1) Set the block
       !             call init(BCs,B)
       !       2) Set type (can use grid information)
       !             call init_Dirichlet(BCs); call init_Dirichlet(BCs,face)
       !             call init_Neumann(BCs);   call init_Neumann(BCs,face)
       !             call init_periodic(BCs);  call init_periodic(BCs,face)
       !       3) Set values
       !             call init(BCs,0.0_cp)       (default)
       !             call init(BCs,0.0_cp,face)
       !             call init(BCs,vals,face)
       !
       ! The convention for the faces is:
       !   face = {1:6} = {x_min,x_max,y_min,y_max,z_min,z_max}
       !
       !
       !
       !
       !              ^
       !              |
       !              |
       !              |-------------/
       !             /             / |
       !            /             /  |
       !           /             /   |
       !          |--------------    |____________>
       !          |             |   /
       !          |             |  /
       !          |             | /
       !          |_____________|/
       !         /
       !        /
       !       /
       !

       use current_precision_mod
       use face_edge_corner_indexing_mod
       use data_location_mod
       use grid_mod
       use block_mod
       use face_SD_mod
       use GF_mod
       use bctype_mod
       use BC_logicals_mod
       use IO_tools_mod
       use table_mod
       use boundary_mod
       use procedure_array_mod
       use apply_BCs_faces_bridge_mod
       use apply_BCs_faces_bridge_implicit_mod
       implicit none

       private
       public :: boundary_conditions
       public :: init,delete,display,print,export,import ! Essentials

       ! Setters for type
       public :: init_Dirichlet
       public :: init_Neumann
       public :: init_Robin
       public :: init_periodic
       public :: init_symmetric
       public :: init_antisymmetric

       public :: init_PA_face

       public :: get_all_Dirichlet
       public :: get_all_Neumann
       public :: get_all_Robin
       public :: get_any_Dirichlet
       public :: get_any_Neumann
       public :: get_any_Robin

       public :: init_props
       public :: defined

       public :: restrict
       public :: prolongate

       type boundary_conditions
         ! GLOBAL LOGICALS (check here to see if BCs are defined)
         type(BC_logicals) :: BCL
         ! DATA LOCATION
         type(data_location) :: DL                       ! data location (C,N,F,E)
         ! FACES
         type(boundary) :: face                          ! BC values and type
         type(procedure_array) :: PA_face_BCs            ! procedure array for face BCs
         type(procedure_array) :: PA_face_implicit_BCs   ! procedure array for face BCs
         type(face_SD) :: f_BCs                          ! stores indexes for slicing
         ! EDGES
         type(boundary) :: edge                          ! BC values and type
         type(procedure_array) :: PA_edges_BCs           ! procedure array for face BCs
         type(procedure_array) :: PA_edges_implicit_BCs  ! procedure array for face BCs
         ! type(edge_SD) :: e_BCs                        ! Not yet developed
         ! CORNERS
         type(boundary) :: corner                        ! BC values and type
         type(procedure_array) :: PA_corners_BCs         ! procedure array for face BCs
         type(procedure_array) :: PA_corners_implicit_BCs! procedure array for face BCs
         ! type(corner_SD) :: c_BCs                      ! Not yet developed
         integer,dimension(6) :: apply_BC_order = (/1,2,3,4,5,6/)
       end type

       interface init;               module procedure init_GFs_BCs_DL;         end interface
       interface init;               module procedure init_BCs_copy;           end interface

       interface init;               module procedure init_vals_all_S;         end interface
       interface init;               module procedure init_vals_face_GF;       end interface
       interface init;               module procedure init_val_face_S;         end interface

       interface delete;             module procedure delete_BCs;              end interface
       interface display;            module procedure display_BCs;             end interface
       interface print;              module procedure print_BCs;               end interface
       interface export;             module procedure export_BCs;              end interface
       interface import;             module procedure import_BCs;              end interface

       interface export;             module procedure export_BCs_wrapper;      end interface
       interface import;             module procedure import_BCs_wrapper;      end interface

       interface defined;            module procedure defined_BCs;             end interface
       interface init_Dirichlet;     module procedure init_Dirichlet_all;      end interface
       interface init_Dirichlet;     module procedure init_Dirichlet_face;     end interface
       interface init_Neumann;       module procedure init_Neumann_all;        end interface
       interface init_Neumann;       module procedure init_Neumann_face;       end interface
       interface init_Robin;         module procedure init_Robin_all;          end interface
       interface init_Robin;         module procedure init_Robin_face;         end interface
       interface init_periodic;      module procedure init_periodic_all;       end interface
       interface init_periodic;      module procedure init_periodic_face;      end interface
       interface init_symmetric;     module procedure init_symmetric_all;      end interface
       interface init_symmetric;     module procedure init_symmetric_face;     end interface
       interface init_antisymmetric; module procedure init_antisymmetric_all;  end interface
       interface init_antisymmetric; module procedure init_antisymmetric_face; end interface
       interface init_PA_face;       module procedure init_PA_face_BC;         end interface

       interface get_all_Dirichlet;  module procedure get_all_Dirichlet_BCs;   end interface
       interface get_all_Neumann;    module procedure get_all_Neumann_BCs;     end interface
       interface get_all_Robin;      module procedure get_all_Robin_BCs;       end interface
       interface get_any_Dirichlet;  module procedure get_any_Dirichlet_BCs;   end interface
       interface get_any_Neumann;    module procedure get_any_Neumann_BCs;     end interface
       interface get_any_Robin;      module procedure get_any_Robin_BCs;       end interface

       interface define_logicals;    module procedure define_logicals_BCs;     end interface
       interface insist_allocated;   module procedure insist_allocated_BCs;    end interface

       interface init_props;         module procedure init_props_BCs;          end interface

       interface restrict;           module procedure restrict_BCs;            end interface
       interface prolongate;         module procedure prolongate_BCs;          end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_GFs_BCs_DL(BC,B,DL)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         type(data_location),intent(in) :: DL
         call delete(BC)
         call init(BC%face,B,DL,6,'face')
         ! call init(BC%edge,B,DL,12,'edge')
         ! call init(BC%corner,B,DL,8,'corner')
         BC%apply_BC_order = B%apply_BC_order
         call init(BC%DL,DL)
         call init(BC%f_BCs,B%g,B%f)
         ! call init(BC%e_BCs,B%g,B%e)
         ! call init(BC%c_BCs,B%g,B%c)
         call init_mixed(BC%f_BCs,DL)
         call init_vals_all_S(BC,0.0_cp)
         BC%BCL%GFs_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_BCs_copy(BC,BC_in)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(boundary_conditions),intent(in) :: BC_in
#ifdef _DEBUG_BOUNDARY_CONDITIONS_
         call insist_allocated(BC_in,'init_BCs_copy')
#endif
         call delete(BC)
         call init(BC%BCL,BC_in%BCL)
         call init(BC%DL,BC_in%DL)
         BC%apply_BC_order = BC_in%apply_BC_order

         call init(BC%face,BC_in%face)
         call init(BC%PA_face_BCs,BC_in%PA_face_BCs)
         call init(BC%PA_face_implicit_BCs,BC_in%PA_face_implicit_BCs)
         call init(BC%f_BCs,BC_in%f_BCs)

         ! call init(BC%edge,BC_in%edge)
         ! call init(BC%PA_edges_BCs,BC_in%PA_edges_BCs)
         ! call init(BC%PA_edges_implicit_BCs,BC_in%PA_edges_implicit_BCs)
         ! call init(BC%e_BCs,BC_in%e_BCs)
         ! call init(BC%corner,BC_in%corner)
         ! call init(BC%PA_corners_BCs,BC_in%PA_corners_BCs)
         ! call init(BC%PA_corners_implicit_BCs,BC_in%PA_corners_implicit_BCs)
         ! call init(BC%c_BCs,BC_in%c_BCs)
       end subroutine

       subroutine delete_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         call delete(BC%DL)
         call delete(BC%BCL)
         call delete(BC%face)
         call delete(BC%PA_face_BCs)
         call delete(BC%PA_face_implicit_BCs)
         call delete(BC%f_BCs)
         call delete(BC%edge)
         call delete(BC%PA_edges_BCs)
         call delete(BC%PA_edges_implicit_BCs)
         call delete(BC%corner)
         call delete(BC%PA_corners_BCs)
         call delete(BC%PA_corners_implicit_BCs)
         BC%apply_BC_order = (/1,2,3,4,5,6/)
         ! call delete(BC%e_BCs)
         ! call delete(BC%c_BCs)
       end subroutine

       subroutine init_vals_all_S(BC,val)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in) :: val
         call init(BC%face,val)
         call init(BC%edge,val)
         call init(BC%corner,val)
         call define_logicals(BC)
         BC%BCL%vals_defined = .true.
       end subroutine

       subroutine init_vals_face_GF(BC,vals,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(grid_field),intent(in) :: vals
         integer,intent(in) :: face
         ! call assign(BC%face(ID)%b,vals)
         call assign(BC%face%SB(face)%b,vals)
         call assign(BC%face%SB(face)%b_total,vals)
         call assign(BC%face%SB(face)%b_modified,0.0_cp)
         BC%BCL%vals_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_val_face_S(BC,val,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in) :: val
         integer,intent(in) :: face
         ! call assign(BC%face(ID)%b,val)
         call assign(BC%face%SB(face)%b,val)
         call assign(BC%face%SB(face)%b_total,val)
         call assign(BC%face%SB(face)%b_modified,0.0_cp)
         BC%BCL%vals_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine display_BCs(BC,un)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         integer,intent(in) :: un
         if (BC%BCL%defined) then
           call display(BC%face,un)
           ! call display(BC%edge)
           ! call display(BC%corner)
         endif
       end subroutine

       subroutine print_BCs(BC)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         call display(BC,6)
       end subroutine

       subroutine export_BCs(BC,un)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         integer,intent(in) :: un
         call insist_allocated(BC,'export_BCs')
         write(un,*) 'defined'
         write(un,*) BC%BCL%defined
         if (BC%BCL%defined) then
           call export(BC%face,un)
           ! call export(BC%edge,un)
           ! call export(BC%corner,un)
           call export(BC%PA_face_BCs,un)
           call export(BC%PA_face_implicit_BCs,un)
           call export(BC%BCL,un)
         endif
       end subroutine

       subroutine import_BCs(BC,un)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: un
         read(un,*)
         read(un,*) BC%BCL%defined
         if (BC%BCL%defined) then
           call import(BC%face,un)
           ! call export(BC%edge,un)
           ! call export(BC%corner,un)
           call import(BC%PA_face_BCs,un)
           call import(BC%PA_face_implicit_BCs,un)
           call import(BC%BCL,un)
         endif
       end subroutine

       subroutine export_BCs_wrapper(BC,dir,name)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(BC,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_BCs_wrapper(BC,dir,name)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(BC,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! *******************************************************************************
       ! ********************************* INIT FACES **********************************
       ! *******************************************************************************

       function defined_BCs(BC) result(d)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         logical :: d
         d = BC%BCL%defined
       end function

       subroutine init_Dirichlet_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         do i=1,6; call init_Dirichlet_face(BC,i); enddo
       end subroutine
       subroutine init_Dirichlet_face(BC,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: face
         integer :: dir
         dir = dir_given_face(face)
         call check_prereq(BC)
         call init_Dirichlet(BC%face,face)
         call init_PA_face(BC,face)
         call define_logicals(BC)
         BC%BCL%BCT_defined = .true.
       end subroutine

       subroutine init_Neumann_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         do i=1,6; call init_Neumann(BC,i); enddo
       end subroutine
       subroutine init_Neumann_face(BC,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: face
         integer :: dir
         dir = dir_given_face(face)
         call check_prereq(BC)
         call init_Neumann(BC%face,face)
         call init_PA_face(BC,face)
         BC%BCL%BCT_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_Robin_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         do i=1,6; call init_Robin_face(BC,i); enddo
       end subroutine
       subroutine init_Robin_face(BC,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: face
         integer :: dir
         dir = dir_given_face(face)
         call check_prereq(BC)
         call init_Robin(BC%face,face)
         call init_PA_face(BC,face)
         BC%BCL%BCT_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_Periodic_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         do i=1,6; call init_Periodic_face(BC,i); enddo
       end subroutine
       subroutine init_Periodic_face(BC,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: face
         integer :: dir
         dir = dir_given_face(face)
         call check_prereq(BC)
         call init_Periodic(BC%face,face)
         call init_PA_face(BC,face)
         BC%BCL%BCT_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_symmetric_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         do i=1,6; call init_symmetric_face(BC,i); enddo
       end subroutine
       subroutine init_symmetric_face(BC,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: face
         integer :: dir
         dir = dir_given_face(face)
         call check_prereq(BC)
         call init_Symmetric(BC%face,face)
         call init_PA_face(BC,face)
         BC%BCL%BCT_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_antisymmetric_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         do i=1,6; call init_antisymmetric_face(BC,i); enddo
       end subroutine
       subroutine init_antisymmetric_face(BC,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: face
         integer :: dir
         dir = dir_given_face(face)
         call check_prereq(BC)
         call init_antisymmetric(BC%face,face)
         call init_PA_face(BC,face)
         BC%BCL%BCT_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_PA_face_BC(BC,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: face
         integer :: dir
         dir = dir_given_face(face)
         call check_prereq(BC)
         call remove(BC%PA_face_BCs,face)
         call remove(BC%PA_face_implicit_BCs,face)
         if (CC_along(BC%DL,dir)) then
           if (is_prescribed(BC%face%SB(face)%bct)) then
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Dirichlet_C,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Neumann_C  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Periodic_C_prescribed ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Robin_C_prescribed    ,face)
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Dirichlet_C_implicit,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Neumann_C_implicit  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Periodic_C_implicit ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Robin_C_implicit_prescribed,face)
           else
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Dirichlet_C,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Neumann_C  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Periodic_C ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Robin_C    ,face)
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Dirichlet_C_implicit,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Neumann_C_implicit  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Periodic_C_implicit ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Robin_C_implicit    ,face)
           endif
         elseif ( N_along(BC%DL,dir)) then
           if (is_prescribed(BC%face%SB(face)%bct)) then
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Dirichlet_N,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Neumann_N  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Periodic_N_prescribed,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Robin_N    ,face)
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Dirichlet_N_implicit,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Neumann_N_implicit  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Periodic_N_implicit ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Robin_N_implicit    ,face)
           else
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Dirichlet_N,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Neumann_N  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Periodic_N ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Robin_N    ,face)
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Dirichlet_N_implicit,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Neumann_N_implicit  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Periodic_N_implicit ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Robin_N_implicit    ,face)
           endif
           else; stop 'Error: bad DL in init_PA_face_BC in boundary_conditions.f90'
         endif
       end subroutine

       ! subroutine init_PA_face_BC(BC,face)
       !   implicit none
       !   type(boundary_conditions),intent(inout) :: BC
       !   integer,intent(in) :: face
       !   integer :: dir
       !   dir = dir_given_face(face)
       !   call check_prereq(BC)
       !   call remove(BC%PA_face_BCs,face)
       !   call remove(BC%PA_face_implicit_BCs,face)
       !   if (CC_along(BC%DL,dir)) then
       !     if (is_prescribed(BC%face%SB(face)%bct)) then
       !       if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Dirichlet_C_prescribed,face)
       !       if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Neumann_C_prescribed  ,face)
       !       if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Periodic_C_prescribed ,face)
       !       if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Robin_C_prescribed    ,face)
       !     else
       !       if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Dirichlet_C,face)
       !       if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Neumann_C  ,face)
       !       if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Periodic_C ,face)
       !       if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Robin_C    ,face)
       !     endif
       !     if (is_prescribed(BC%face%SB(face)%bct)) then
       !       if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Dirichlet_C_implicit_prescribed,face)
       !       if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Neumann_C_implicit_prescribed  ,face)
       !       if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Periodic_C_implicit_prescribed ,face)
       !       if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Robin_C_implicit_prescribed    ,face)
       !     else
       !       if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Dirichlet_C_implicit,face)
       !       if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Neumann_C_implicit  ,face)
       !       if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Periodic_C_implicit ,face)
       !       if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Robin_C_implicit    ,face)
       !     endif
       !   endif
       !   if ( N_along(BC%DL,dir)) then
       !     if (is_prescribed(BC%face%SB(face)%bct)) then
       !       if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Dirichlet_N_prescribed,face)
       !       if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Neumann_N_prescribed  ,face)
       !       if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Periodic_N_prescribed ,face)
       !       if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Robin_N_prescribed    ,face)
       !     else
       !       if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Dirichlet_N,face)
       !       if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Neumann_N  ,face)
       !       if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Periodic_N ,face)
       !       if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_BCs,Robin_N    ,face)
       !     endif
       !     if (is_prescribed(BC%face%SB(face)%bct)) then
       !       if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Dirichlet_N_implicit_prescribed,face)
       !       if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Neumann_N_implicit_prescribed  ,face)
       !       if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Periodic_N_implicit_prescribed ,face)
       !       if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Robin_N_implicit_prescribed    ,face)
       !     else
       !       if (is_Dirichlet(BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Dirichlet_N_implicit,face)
       !       if (is_Neumann(  BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Neumann_N_implicit  ,face)
       !       if (is_Periodic( BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Periodic_N_implicit ,face)
       !       if (is_Robin(    BC%face%SB(face)%bct)) call add(BC%PA_face_implicit_BCs,Robin_N_implicit    ,face)
       !     endif
       !   endif
       ! end subroutine

       ! *******************************************************************************
       ! ********************************* INIT EDGES **********************************
       ! *******************************************************************************

       ! *******************************************************************************
       ! ********************************* AUXILIARY ***********************************
       ! *******************************************************************************

       subroutine check_prereq(BC)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         if (.not.BC%BCL%GFs_defined) stop 'Error: BC GFs must be defined before type is defined in boundary_conditions.f90'
       end subroutine

       subroutine insist_allocated_BCs(BC,caller)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         character(len=*),intent(in) :: caller
         if (.not.BC%BCL%defined) then
           write(*,*) 'Error: BC not defined in '//caller//' in boundary_conditions.f90'
           stop 'Done'
         endif
       end subroutine

       subroutine define_logicals_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         logical,dimension(3) :: L
         BC%BCL%defined = BC%BCL%GFs_defined.and.BC%BCL%BCT_defined.and.BC%BCL%vals_defined

         L(1) = BC%face%BCL%all_Dirichlet
         L(2) = BC%edge%BCL%all_Dirichlet
         L(3) = BC%corner%BCL%all_Dirichlet
         BC%BCL%all_Dirichlet = all(L)

         L(1) = BC%face%BCL%all_Robin
         L(2) = BC%edge%BCL%all_Robin
         L(3) = BC%corner%BCL%all_Robin
         BC%BCL%all_Robin = all(L)

         L(1) = BC%face%BCL%all_Neumann
         L(2) = BC%edge%BCL%all_Neumann
         L(3) = BC%corner%BCL%all_Neumann
         BC%BCL%all_Neumann = all(L)

         L(1) = BC%face%BCL%any_Dirichlet
         L(2) = BC%edge%BCL%any_Dirichlet
         L(3) = BC%corner%BCL%any_Dirichlet
         BC%BCL%any_Dirichlet = any(L)

         L(1) = BC%face%BCL%any_Robin
         L(2) = BC%edge%BCL%any_Robin
         L(3) = BC%corner%BCL%any_Robin
         BC%BCL%any_Robin = any(L)

         L(1) = BC%face%BCL%any_Neumann
         L(2) = BC%edge%BCL%any_Neumann
         L(3) = BC%corner%BCL%any_Neumann
         BC%BCL%any_Neumann = any(L)
       end subroutine

       function get_all_Dirichlet_BCs(BC) result(L)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         logical :: L
         L = BC%BCL%all_Dirichlet
       end function

       function get_all_Neumann_BCs(BC) result(L)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         logical :: L
         L = BC%BCL%all_Neumann
       end function

       function get_all_Robin_BCs(BC) result(L)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         logical :: L
         L = BC%BCL%all_Robin
       end function

       function get_any_Dirichlet_BCs(BC) result(L)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         logical :: L
         L = BC%BCL%any_Dirichlet
       end function

       function get_any_Neumann_BCs(BC) result(L)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         logical :: L
         L = BC%BCL%any_Neumann
       end function

       function get_any_Robin_BCs(BC) result(L)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         logical :: L
         L = BC%BCL%any_Robin
       end function

       subroutine init_props_BCs(BC,Robin_coeff)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in),dimension(6) :: Robin_coeff
         call define_logicals(BC)
         call init_mixed(BC%f_BCs,BC%DL)
         call init_Robin_coeff(BC%f_BCs,Robin_coeff)
         call sort(BC%PA_face_BCs,BC%apply_BC_order,6)
         call sort(BC%PA_face_implicit_BCs,BC%apply_BC_order,6)
       end subroutine

       subroutine restrict_BCs(BC,B,dir)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         integer,dimension(3) :: eye
         integer :: x,y,z
         real(cp),dimension(6) :: Robin_coeff
         eye = eye_given_dir(dir)
         x = eye(1); y = eye(2); z = eye(3)
         if (BC%BCL%defined) then
           call restrict(BC%face,B%fb,BC%DL,dir,x,y,z,BC%face%n)
           ! call restrict(BC%edge,B%eb,BC%DL,dir,x,y,z,BC%edge%n)
           ! call restrict(BC%face,B%cb,BC%DL,dir,x,y,z,BC%corner%n)
           call init(BC%f_BCs,B%g,B%f)
           ! call init(BC%e_BCs,B%g,B%e)
           ! call init(BC%c_BCs,B%g,B%c)
           Robin_coeff = BC%f_BCs%Robin_coeff
           call init_props(BC,Robin_coeff)
         endif
       end subroutine

       subroutine prolongate_BCs(BC,B,dir)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         integer,dimension(3) :: eye
         integer :: x,y,z
         real(cp),dimension(6) :: Robin_coeff
         eye = eye_given_dir(dir)
         x = eye(1); y = eye(2); z = eye(3)
         if (BC%BCL%defined) then
           call prolongate(BC%face,B%fb,BC%DL,dir,x,y,z,BC%face%n)
           ! call prolongate(BC%edge,B%eb,BC%DL,dir,x,y,z,BC%edge%n)
           ! call prolongate(BC%face,B%cb,BC%DL,dir,x,y,z,BC%corner%n)
           call init(BC%f_BCs,B%g,B%f)
           ! call init(BC%e_BCs,B%g,B%e)
           ! call init(BC%c_BCs,B%g,B%c)
           Robin_coeff = BC%f_BCs%Robin_coeff
           call init_props(BC,Robin_coeff)
         endif
       end subroutine

       end module