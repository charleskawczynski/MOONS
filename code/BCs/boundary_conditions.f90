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

       public :: getAllNeumann
       public :: getDirichlet
       public :: getAllRobin

       public :: init_props

       type boundary_conditions
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
         ! OTHER
         type(data_location) :: DL                       ! data location (C,N,F,E)
         type(BC_logicals) :: BCL
       end type

       interface init;                module procedure init_GFs_BCs_DL;         end interface
       interface init;                module procedure init_BCs_copy;           end interface

       interface init;                module procedure init_vals_all_S;         end interface
       interface init;                module procedure init_vals_face_GF;       end interface
       interface init;                module procedure init_val_face_S;         end interface

       interface delete;              module procedure delete_BCs;              end interface
       interface display;             module procedure display_BCs;             end interface
       interface print;               module procedure print_BCs;               end interface
       interface export;              module procedure export_BCs;              end interface
       interface import;              module procedure import_BCs;              end interface

       interface export;              module procedure export_BCs_wrapper;      end interface
       interface import;              module procedure import_BCs_wrapper;      end interface

       interface init_Dirichlet;      module procedure init_Dirichlet_all;      end interface
       interface init_Dirichlet;      module procedure init_Dirichlet_face;     end interface
       interface init_Neumann;        module procedure init_Neumann_all;        end interface
       interface init_Neumann;        module procedure init_Neumann_face;       end interface
       interface init_Robin;          module procedure init_Robin_all;          end interface
       interface init_Robin;          module procedure init_Robin_face;         end interface
       interface init_periodic;       module procedure init_periodic_all;       end interface
       interface init_periodic;       module procedure init_periodic_face;      end interface
       interface init_symmetric;      module procedure init_symmetric_all;      end interface
       interface init_symmetric;      module procedure init_symmetric_face;     end interface
       interface init_antisymmetric;  module procedure init_antisymmetric_all;  end interface
       interface init_antisymmetric;  module procedure init_antisymmetric_face; end interface

       interface define_logicals;     module procedure define_logicals_BCs;     end interface
       interface insist_allocated;    module procedure insist_allocated_BCs;    end interface

       interface init_props;          module procedure init_props_BCs;          end interface

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
         call init(BC%face,BC_in%face)
         ! call init(BC%edge,BC_in%edge)
         ! call init(BC%corner,BC_in%corner)
         call init(BC%BCL,BC_in%BCL)
         call init(BC%DL,BC_in%DL)
         call init(BC%f_BCs,BC_in%f_BCs)
         call init(BC%PA_face_BCs,BC_in%PA_face_BCs)
         call init(BC%PA_face_implicit_BCs,BC_in%PA_face_implicit_BCs)
       end subroutine

       subroutine delete_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         call delete(BC%face)
         call delete(BC%edge)
         call delete(BC%corner)
         call delete(BC%f_BCs)
         call delete(BC%PA_face_BCs)
         call delete(BC%PA_face_implicit_BCs)
         call delete(BC%BCL)
         call define_logicals(BC)
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
         call assign(BC%face%b(face),vals)
         BC%BCL%vals_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_val_face_S(BC,val,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in) :: val
         integer,intent(in) :: face
         ! call assign(BC%face(ID)%b,val)
         call assign(BC%face%b(face),val)
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
           write(un,*) 'all_Dirichlet,all_Neumann,all_Robin'
           write(un,*) BC%BCL%all_Dirichlet,BC%BCL%all_Neumann,BC%BCL%all_Robin
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
           read(un,*) 
           read(un,*) BC%BCL%all_Dirichlet,BC%BCL%all_Neumann,BC%BCL%all_Robin
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

         call init_Dirichlet(BC%face,face)
         if (CC_along(BC%DL,dir)) then
         call remove(BC%PA_face_BCs,face)
         call add(BC%PA_face_BCs,Dirichlet_C,face)
         endif
         if ( N_along(BC%DL,dir)) then
         call remove(BC%PA_face_BCs,face)
         call add(BC%PA_face_BCs,Dirichlet_N,face)
         endif

         if (CC_along(BC%DL,dir)) then
         call remove(BC%PA_face_implicit_BCs,face)
         call add(BC%PA_face_implicit_BCs,Dirichlet_C_implicit,face)
         endif
         if ( N_along(BC%DL,dir)) then
         call remove(BC%PA_face_implicit_BCs,face)
         call add(BC%PA_face_implicit_BCs,Dirichlet_N_implicit,face)
         endif
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
         call check_prereq(BC)
         call init_Neumann(BC%face,face)
         if (CC_along(BC%DL,dir_given_face(face))) then
           call remove(BC%PA_face_BCs,face)
           call add(BC%PA_face_BCs,Neumann_C,face)
         endif
         if ( N_along(BC%DL,dir_given_face(face))) then
           call remove(BC%PA_face_BCs,face)
           call add(BC%PA_face_BCs,Neumann_N,face)
         endif

         if (CC_along(BC%DL,dir_given_face(face))) then
         call remove(BC%PA_face_implicit_BCs,face)
         call add(BC%PA_face_implicit_BCs,Neumann_C_implicit,face)
         endif
         if ( N_along(BC%DL,dir_given_face(face))) then
         call remove(BC%PA_face_implicit_BCs,face)
         call add(BC%PA_face_implicit_BCs,Neumann_N_implicit,face)
         endif
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
         call check_prereq(BC)
         call init_Robin(BC%face,face)
         if (CC_along(BC%DL,dir_given_face(face))) call add(BC%PA_face_BCs,Robin_C,face)
         if ( N_along(BC%DL,dir_given_face(face))) call add(BC%PA_face_BCs,Robin_N,face)

         if (CC_along(BC%DL,dir_given_face(face))) then
         call add(BC%PA_face_implicit_BCs,Robin_C_implicit,face)
         endif
         if ( N_along(BC%DL,dir_given_face(face))) then
         call add(BC%PA_face_implicit_BCs,Robin_N_implicit,face)
         endif
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
         if (CC_along(BC%DL,dir)) call add(BC%PA_face_BCs,Periodic_C,face)
         if ( N_along(BC%DL,dir)) call add(BC%PA_face_BCs,Periodic_N,face)

         if (CC_along(BC%DL,dir)) then
           call add(BC%PA_face_implicit_BCs,Periodic_C_implicit,face)
         endif
         if ( N_along(BC%DL,dir)) then
           ! 
         endif
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
         call check_prereq(BC)
         call init_symmetric(BC%face,face)
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
         call check_prereq(BC)
         call init_antisymmetric(BC%face,face)
         call define_logicals(BC)
       end subroutine

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
       end subroutine

       function getAllNeumann(BC) result(L)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         logical :: L
         L = BC%BCL%all_Neumann
       end function

       function getDirichlet(BC) result(L)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         logical :: L
         L = BC%BCL%all_Dirichlet
       end function

       function getAllRobin(BC) result(L)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         logical :: L
         L = BC%BCL%all_Robin
       end function

       subroutine init_props_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         call define_logicals_BCs(BC)
         call init_mixed(BC%f_BCs,BC%DL)
         call sort(BC%PA_face_BCs,(/3,4,5,6,1,2/),6)
         call sort(BC%PA_face_implicit_BCs,(/3,4,5,6,1,2/),6)
       end subroutine

       end module