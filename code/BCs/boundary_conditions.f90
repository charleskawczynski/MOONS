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
       !             call init(BCs,0.0)       (default)
       !             call init(BCs,0.0,face)
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
       use face_domain_mod
       use domain_mod
       use GF_mod
       use bctype_mod
       use BC_logicals_mod
       use IO_tools_mod
       use table_mod
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
       public :: init_antisymmetry

       public :: getAllNeumann
       public :: getDirichlet
       public :: getAllRobin

       public :: init_props

       type boundary_conditions
         ! BC values:
         type(grid_field),dimension(:),allocatable :: f  ! BC values size = 6
         type(grid_field),dimension(:),allocatable :: e  ! BC values size = 12
         type(grid_field),dimension(:),allocatable :: c  ! BC values size = 8
         type(bctype),dimension(6) :: bct_f              ! for self-documenting output
         type(bctype),dimension(12) :: bct_e             ! for self-documenting output
         type(bctype),dimension(8) :: bct_c              ! for self-documenting output

         type(face_domain) :: face_BCs                   ! indexes         for face BCs
         type(procedure_array) :: PA_face_BCs            ! procedure array for face BCs
         type(procedure_array) :: PA_face_implicit_BCs   ! procedure array for face BCs
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
       interface init_antisymmetry;   module procedure init_antisymmetry_all;   end interface
       interface init_antisymmetry;   module procedure init_antisymmetry_face;  end interface

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
         integer :: i
         call delete(BC)
         allocate(BC%f(6))
         allocate(BC%e(12))
         allocate(BC%c(8))
         call init(BC%DL,DL)
               if (is_CC(DL)) then; do i=1,6; call init_CC(  BC%f(i),B%fb(i)); enddo
         elseif (is_Node(DL)) then; do i=1,6; call init_Node(BC%f(i),B%fb(i)); enddo
         elseif (is_Face(DL)) then; do i=1,6; call init_Face(BC%f(i),B%fb(i),DL%face); enddo
         elseif (is_Edge(DL)) then; do i=1,6; call init_Edge(BC%f(i),B%fb(i),DL%edge); enddo
         endif
               if (is_CC(DL)) then; do i=1,12; call init_CC(  BC%e(i),B%eb(i)); enddo
         elseif (is_Node(DL)) then; do i=1,12; call init_Node(BC%e(i),B%eb(i)); enddo
         elseif (is_Face(DL)) then; do i=1,12; call init_Face(BC%e(i),B%eb(i),DL%face); enddo
         elseif (is_Edge(DL)) then; do i=1,12; call init_Edge(BC%e(i),B%eb(i),DL%edge); enddo
         endif
               if (is_CC(DL)) then; do i=1,8; call init_CC(  BC%c(i),B%cb(i)); enddo
         elseif (is_Node(DL)) then; do i=1,8; call init_Node(BC%c(i),B%cb(i)); enddo
         elseif (is_Face(DL)) then; do i=1,8; call init_Face(BC%c(i),B%cb(i),DL%face); enddo
         elseif (is_Edge(DL)) then; do i=1,8; call init_Edge(BC%c(i),B%cb(i),DL%edge); enddo
         endif
         call init_vals_all_S(BC,0.0_cp)
         BC%BCL%GFs_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_BCs_copy(BC_out,BC_in)
         implicit none
         type(boundary_conditions),intent(inout) :: BC_out
         type(boundary_conditions),intent(in) :: BC_in
         integer :: i
#ifdef _DEBUG_BOUNDARY_CONDITIONS_
         call insist_allocated(BC_in,'init_BCs_copy')
#endif
         call delete(BC_out)
         allocate(BC_out%f(6))
         allocate(BC_out%e(12))
         allocate(BC_out%c(8))
         do i=1,6;  call init(BC_out%f(i),BC_in%f(i)); enddo
         do i=1,12; call init(BC_out%e(i),BC_in%e(i)); enddo
         do i=1,8;  call init(BC_out%c(i),BC_in%c(i)); enddo
         call init(BC_out%face_BCs,BC_in%face_BCs)
         call init(BC_out%PA_face_BCs,BC_in%PA_face_BCs)
         call init(BC_out%PA_face_implicit_BCs,BC_in%PA_face_implicit_BCs)
         call init(BC_out%DL,BC_in%DL)
         ! call init(BC_out%B,BC_in%B)
         call init(BC_out%BCL,BC_in%BCL)
       end subroutine

       subroutine delete_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         if (allocated(BC%f)) then; do i=1,size(BC%f); call delete(BC%f(i)); enddo; deallocate(BC%f); endif
         if (allocated(BC%e)) then; do i=1,size(BC%e); call delete(BC%e(i)); enddo; deallocate(BC%e); endif
         if (allocated(BC%c)) then; do i=1,size(BC%c); call delete(BC%c(i)); enddo; deallocate(BC%c); endif
         call delete(BC%face_BCs)
         call delete(BC%PA_face_BCs)
         call delete(BC%PA_face_implicit_BCs)
         call delete(BC%BCL)
         call define_logicals(BC)
       end subroutine

       subroutine init_vals_all_S(BC,val)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in) :: val
         integer :: i
         do i=1,6;  call assign(BC%f(i),val); enddo
         do i=1,12; call assign(BC%e(i),val); enddo
         do i=1,8;  call assign(BC%c(i),val); enddo
         call define_logicals(BC)
         BC%BCL%vals_defined = .true.
       end subroutine

       subroutine init_vals_face_GF(BC,vals,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(grid_field),intent(in) :: vals
         integer,intent(in) :: face
         call assign(BC%f(face),vals)
         BC%BCL%vals_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_val_face_S(BC,val,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in) :: val
         integer,intent(in) :: face
         call assign(BC%f(face),val)
         BC%BCL%vals_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine display_BCs(BC,un)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         integer,intent(in) :: un
         integer :: i,col_width,precision
         if (BC%BCL%defined) then
           precision = 4; col_width = 10
           call export_table('Faces   :',(/(i,i=1,6)/),col_width,un)
           call export_table('Type    :',(/(get_bctype(BC%bct_f(i)),i=1,6)/),col_width,un)
           call export_table('meanVal :',(/(get_mean_value(BC%bct_f(i)),i=1,6)/),col_width,precision,un)
           ! precision = 1; col_width = 5
           ! call export_table('Edges   :',(/(i,i=1,12)/),col_width,un)
           ! call export_table('Type    :',(/(get_bctype(BC%bct_e(i)),i=1,12)/),col_width,un)
           ! call export_table('meanVal :',(/(get_mean_value(BC%bct_e(i)),i=1,12)/),col_width,precision,un)
           ! col_width = 10
           ! call export_table('Corners :',(/(i,i=1,8)/),col_width,un)
           ! call export_table('Type    :',(/(get_bctype(BC%bct_c(i)%b),i=1,8)/),col_width,un)
           ! call export_table('meanVal :',(/(get_mean_value(BC%bct_c(i)),i=1,8)/),col_width,precision,un)
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
         integer :: i
         call insist_allocated(BC,'export_BCs')
         write(un,*) 'defined'
         write(un,*) BC%BCL%defined
         if (BC%BCL%defined) then
           do i=1,6;  call export(BC%f(i),un); enddo
           do i=1,12; call export(BC%e(i),un); enddo
           do i=1,8;  call export(BC%c(i),un); enddo
           do i=1,6;  call export(BC%bct_f(i),un); enddo
           do i=1,12; call export(BC%bct_e(i),un); enddo
           do i=1,8;  call export(BC%bct_c(i),un); enddo
           call export(BC%PA_face_BCs,un)
           call export(BC%PA_face_implicit_BCs,un)
           call export(BC%face_BCs,un)
           call export(BC%BCL,un)
           write(un,*) 'all_Dirichlet,all_Neumann,all_Robin'
           write(un,*) BC%BCL%all_Dirichlet,BC%BCL%all_Neumann,BC%BCL%all_Robin
         endif
       end subroutine

       subroutine import_BCs(BC,un)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: un
         integer :: i
         read(un,*) 
         read(un,*) BC%BCL%defined
         if (BC%BCL%defined) then
           do i=1,6;  call import(BC%f(i),un); enddo
           do i=1,12; call import(BC%e(i),un); enddo
           do i=1,8;  call import(BC%c(i),un); enddo
           do i=1,6;  call import(BC%bct_f(i),un); enddo
           do i=1,12; call import(BC%bct_e(i),un); enddo
           do i=1,8;  call import(BC%bct_c(i),un); enddo
           call import(BC%PA_face_BCs,un)
           call import(BC%PA_face_implicit_BCs,un)
           call import(BC%face_BCs,un)
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

       subroutine init_Dirichlet_all(BC,B)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer :: i
         do i=1,6; call init_Dirichlet_face(BC,B,i); enddo
       end subroutine
       subroutine init_Dirichlet_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call init_Dirichlet(BC%bct_f(face))
         call init(BC%face_BCs,B,face)
         if (CC_along(BC%DL,dir_given_face(face))) then
         call remove(BC%PA_face_BCs,face)
         call add(BC%PA_face_BCs,Dirichlet_C,face)
         endif
         if ( N_along(BC%DL,dir_given_face(face))) then
         call remove(BC%PA_face_BCs,face)
         call add(BC%PA_face_BCs,Dirichlet_N,face)
         endif

         if (CC_along(BC%DL,dir_given_face(face))) then
         call remove(BC%PA_face_implicit_BCs,face)
         call add(BC%PA_face_implicit_BCs,Dirichlet_C_implicit,face)
         endif
         if ( N_along(BC%DL,dir_given_face(face))) then
         call remove(BC%PA_face_implicit_BCs,face)
         call add(BC%PA_face_implicit_BCs,Dirichlet_N_implicit,face)
         endif
         call define_logicals(BC)
         BC%BCL%BCT_defined = .true.
       end subroutine

       subroutine init_Neumann_all(BC,B)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer :: i
         do i=1,6; call init_Neumann(BC,B,i); enddo
         BC%BCL%BCT_defined = .true.
       end subroutine
       subroutine init_Neumann_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Neumann(BC%bct_f(face))
         call init(BC%face_BCs,B,face)
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

       subroutine init_Robin_all(BC,B)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer :: i
         do i=1,6; call init_Robin_face(BC,B,i); enddo
       end subroutine
       subroutine init_Robin_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Robin(BC%bct_f(face))
         call init(BC%face_BCs,B,face)
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

       subroutine init_Periodic_all(BC,B)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer :: i
         do i=1,6; call init_Periodic_face(BC,B,i); enddo
       end subroutine
       subroutine init_Periodic_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Periodic(BC%bct_f(face))
         call init(BC%face_BCs,B,face)
         if (CC_along(BC%DL,dir_given_face(face))) call add(BC%PA_face_BCs,Periodic_C,face)
         if ( N_along(BC%DL,dir_given_face(face))) call add(BC%PA_face_BCs,Periodic_N,face)

         if (CC_along(BC%DL,dir_given_face(face))) then
           call add(BC%PA_face_implicit_BCs,Periodic_C_implicit,face)
         endif
         if ( N_along(BC%DL,dir_given_face(face))) then
           call add(BC%PA_face_implicit_BCs,Periodic_N_implicit,face)
         endif
         BC%BCL%BCT_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_antisymmetry_all(BC,B)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer :: i
         do i=1,6; call init_antisymmetry_face(BC,B,i); enddo
       end subroutine
       subroutine init_antisymmetry_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_antisymmetric(BC%bct_f(face))
         call init(BC%face_BCs,B,face)
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
         logical,dimension(3) :: L
         L(1) = allocated(BC%f).and.(size(BC%f).eq.6)
         L(2) = allocated(BC%e).and.(size(BC%e).eq.12)
         L(3) = allocated(BC%c).and.(size(BC%c).eq.8)
         if (.not.all(L)) then
           write(*,*) 'Error: trying to copy unallocated BCs in '//caller//'in boundary_conditions.f90'
           write(*,*) 'size(BC%f) = ',size(BC%f)
           write(*,*) 'size(BC%e) = ',size(BC%e)
           write(*,*) 'size(BC%c) = ',size(BC%c)
           stop 'Done'
         endif
       end subroutine

       subroutine define_logicals_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         logical,dimension(3) :: L
         BC%BCL%defined = BC%BCL%GFs_defined.and.BC%BCL%BCT_defined.and.BC%BCL%vals_defined

         L(1) = all((/(is_Dirichlet(BC%bct_f(i)),i=1,6)/))
         L(2) = all((/(is_Dirichlet(BC%bct_e(i)),i=1,12)/))
         L(3) = all((/(is_Dirichlet(BC%bct_c(i)),i=1,8)/))
         BC%BCL%all_Dirichlet = all(L)

         L(1) = all((/(is_Robin(BC%bct_f(i)),i=1,6)/))
         L(2) = all((/(is_Robin(BC%bct_e(i)),i=1,12)/))
         L(3) = all((/(is_Robin(BC%bct_c(i)),i=1,8)/))
         BC%BCL%all_Robin = all(L)

         L(1) = all((/(is_Neumann(BC%bct_f(i)),i=1,6)/))
         L(2) = all((/(is_Neumann(BC%bct_e(i)),i=1,12)/))
         L(3) = all((/(is_Neumann(BC%bct_c(i)),i=1,8)/))
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
         call init_props(BC%face_BCs,BC%DL)
         ! call print(BC%PA_face_BCs)
         ! call sort(BC%PA_face_BCs,(/1,2,3,4,3,4/),6)
         call sort(BC%PA_face_BCs,(/1,2,5,6,3,4/),6)
         ! call print(BC%PA_face_BCs)
         ! stop 'Done in boundary_conditions.f90'
       end subroutine

       end module