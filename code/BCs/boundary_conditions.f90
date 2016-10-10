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
       use IO_tools_mod
       use table_mod
       use procedure_array_mod
       use apply_BCs_faces_bridge_mod
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
         type(domain) :: Dirichlet,Neumann,Periodic,Robin,symmetric,antisymmetric
         type(face_domain) :: D,N,P,R,S,A
         type(procedure_array) :: P_D,P_N,P_P,P_R,P_S,P_A
         ! type(block) :: B
         ! type(grid_field),dimension(6) :: f
         ! type(grid_field),dimension(12) :: e
         ! type(grid_field),dimension(8) :: c
         type(grid_field),dimension(:),allocatable :: f
         type(grid_field),dimension(:),allocatable :: e
         type(grid_field),dimension(:),allocatable :: c
         type(bctype),dimension(6) :: bct_f
         type(bctype),dimension(12) :: bct_e
         type(bctype),dimension(8) :: bct_c
         integer,dimension(6) :: i_f
         integer,dimension(12) :: i_e
         integer,dimension(8) :: i_c
         logical :: defined = .false.
         logical :: GFs_defined,BCT_defined,vals_defined
         logical :: all_Dirichlet,all_Neumann,all_Robin
         type(data_location) :: DL
       end type

       interface init;                module procedure init_GFs_BCs_DL;         end interface
       interface init;                module procedure init_BCs_copy;           end interface
       interface init;                module procedure init_vals_all_S;         end interface
       interface init;                module procedure init_vals_face_vals;     end interface
       interface init;                module procedure init_val_face_S;         end interface
       interface delete;              module procedure delete_BCs;              end interface
       interface display;             module procedure display_BCs;             end interface
       interface print;               module procedure print_BCs;               end interface
       interface export;              module procedure export_BCs;              end interface
       interface import;              module procedure import_BCs;              end interface

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
         ! call init(BC%B,B)
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
         call init_vals_all_S(BC,0.0_cp)
         BC%GFs_defined = .true.
         call define_logicals(BC)
       end subroutine

       subroutine init_BCs_copy(BC_out,BC_in)
         implicit none
         type(boundary_conditions),intent(inout) :: BC_out
         type(boundary_conditions),intent(in) :: BC_in
         integer :: i
         call insist_allocated(BC_in,'init_BCs_copy')
         do i=1,6;  call init(BC_out%f(i),BC_in%f(i)); enddo
         do i=1,12; call init(BC_out%e(i),BC_in%e(i)); enddo
         do i=1,8;  call init(BC_out%c(i),BC_in%c(i)); enddo
         call init(BC_out%DL,BC_in%DL)
         ! call init(BC_out%B,BC_in%B)
         BC_out%BCT_defined = BC_in%BCT_defined
         BC_out%vals_defined = BC_in%vals_defined
         BC_out%GFs_defined = BC_in%GFs_defined
         BC_out%defined = BC_in%defined
         BC_out%all_Dirichlet = BC_in%all_Dirichlet
         BC_out%all_Neumann = BC_in%all_Neumann
       end subroutine

       subroutine delete_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         if (allocated(BC%f)) then; do i=1,size(BC%f); call delete(BC%f(i)); enddo; deallocate(BC%f); endif
         if (allocated(BC%e)) then; do i=1,size(BC%e); call delete(BC%e(i)); enddo; deallocate(BC%e); endif
         if (allocated(BC%c)) then; do i=1,size(BC%c); call delete(BC%c(i)); enddo; deallocate(BC%c); endif
         BC%BCT_defined = .false.
         BC%GFs_defined = .false.
         BC%defined = .false.
         BC%vals_defined = .false.
         BC%all_Dirichlet = .false.
         BC%all_Neumann = .false.
         call delete(BC%Dirichlet)
         call delete(BC%Neumann)
         call delete(BC%Periodic)
         call delete(BC%Robin)
         call delete(BC%symmetric)
         call delete(BC%antisymmetric)
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
         BC%vals_defined = .true.
       end subroutine

       subroutine init_vals_face_vals(BC,vals,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),dimension(:,:,:),intent(in) :: vals
         integer,intent(in) :: face
         call assign(BC%f(face),vals)
         call define_logicals(BC)
       end subroutine

       subroutine init_val_face_S(BC,val,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in) :: val
         integer,intent(in) :: face
         call assign(BC%f(face),val)
         call define_logicals(BC)
       end subroutine

       subroutine display_BCs(BC,un)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         integer,intent(in) :: un
         integer :: i,col_width,precision
         ! write(un,*) 'Faces  : {xmin,xmax,ymin,ymax,zmin,zmax}'
         ! write(un,*) 'Edges  : {minmin,minmax,maxmin,maxmax} (x: y-z)'
         ! write(un,*) '       : {minmin,minmax,maxmin,maxmax} (y: x-z)'
         ! write(un,*) '       : {minmin,minmax,maxmin,maxmax} (z: x-y)'
         ! write(un,*) 'Corners: {min(x,y,z), max(x,y,z), min(y,z)/max(x)}'
         if (BC%defined) then
           precision = 4; col_width = 10
           call export_table('Faces   :',(/(i,i=1,6)/),col_width,un)
           call export_table('Type    :',(/(get_bctype(BC%bct_f(i)),i=1,6)/),col_width,un)
           call export_table('meanVal :',(/(get_mean_value(BC%bct_f(i)),i=1,6)/),col_width,precision,un)
           precision = 1; col_width = 5
           call export_table('Edges   :',(/(i,i=1,12)/),col_width,un)
           call export_table('Type    :',(/(get_bctype(BC%bct_e(i)),i=1,12)/),col_width,un)
           call export_table('meanVal :',(/(get_mean_value(BC%bct_e(i)),i=1,12)/),col_width,precision,un)
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
         write(un,*) BC%defined
         if (BC%defined) then
           do i=1,6;  call export(BC%f(i),un); enddo
           do i=1,12; call export(BC%e(i),un); enddo
           do i=1,8;  call export(BC%c(i),un); enddo
           do i=1,6;  call export(BC%bct_f(i),un); enddo
           do i=1,12; call export(BC%bct_e(i),un); enddo
           do i=1,8;  call export(BC%bct_c(i),un); enddo
           write(un,*) 'all_Dirichlet,all_Neumann,all_Robin'
           write(un,*) BC%all_Dirichlet,BC%all_Neumann,BC%all_Robin
         endif
       end subroutine

       subroutine import_BCs(BC,un)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: un
         integer :: i
         read(un,*) 
         read(un,*) BC%defined
         if (BC%defined) then
           do i=1,6;  call import(BC%f(i),un); enddo
           do i=1,12; call import(BC%e(i),un); enddo
           do i=1,8;  call import(BC%c(i),un); enddo
           do i=1,6;  call import(BC%bct_f(i),un); enddo
           do i=1,12; call import(BC%bct_e(i),un); enddo
           do i=1,8;  call import(BC%bct_c(i),un); enddo
           read(un,*) 
           read(un,*) BC%all_Dirichlet,BC%all_Neumann,BC%all_Robin
         endif
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine init_Dirichlet_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_Dirichlet(BC%bct_f(i)); enddo
         do i=1,12; call init_Dirichlet(BC%bct_e(i)); enddo
         do i=1,8;  call init_Dirichlet(BC%bct_c(i)); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_Neumann_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_Neumann(BC%bct_f(i)); enddo
         do i=1,12; call init_Neumann(BC%bct_e(i)); enddo
         do i=1,8;  call init_Neumann(BC%bct_c(i)); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_Robin_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_Robin(BC%bct_f(i)); enddo
         do i=1,12; call init_Robin(BC%bct_e(i)); enddo
         do i=1,8;  call init_Robin(BC%bct_c(i)); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_Periodic_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_Periodic(BC%bct_f(i)); enddo
         do i=1,12; call init_Periodic(BC%bct_e(i)); enddo
         do i=1,8;  call init_Periodic(BC%bct_c(i)); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_antisymmetry_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_antisymmetric(BC%bct_f(i)); enddo
         do i=1,12; call init_antisymmetric(BC%bct_e(i)); enddo
         do i=1,8;  call init_antisymmetric(BC%bct_c(i)); enddo
         call define_logicals(BC)
       end subroutine

       ! *******************************************************************************
       ! ********************************* INIT FACES **********************************
       ! *******************************************************************************

       subroutine init_Dirichlet_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Dirichlet(BC%bct_f(face))
         call init(BC%D,B,face)
         if (CC_along(BC%DL,dir_given_face(face))) call add(BC%P_D,Dirichlet_C,face)
         if (N_along(BC%DL,dir_given_face(face))) call add(BC%P_D,Dirichlet_N,face)
         call define_logicals(BC)
       end subroutine

       subroutine init_Neumann_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Neumann(BC%bct_f(face))
         call init(BC%N,B,face)
         if (CC_along(BC%DL,dir_given_face(face))) call add(BC%P_N,Neumann_C,face)
         if (N_along(BC%DL,dir_given_face(face))) call add(BC%P_N,Neumann_N,face)
         call define_logicals(BC)
       end subroutine

       subroutine init_Robin_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Robin(BC%bct_f(face))
         call init(BC%R,B,face)
         call define_logicals(BC)
       end subroutine

       subroutine init_Periodic_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Periodic(BC%bct_f(face))
         call init(BC%P,B,face)
         call define_logicals(BC)
       end subroutine

       subroutine init_antisymmetry_face(BC,B,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_antisymmetric(BC%bct_f(face))
         call init(BC%A,B,face)
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
         if (.not.BC%GFs_defined) stop 'Error: BC GFs must be defined before type is defined in boundary_conditions.f90'
       end subroutine

       subroutine insist_allocated_BCs(BC,caller)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         character(len=*),intent(in) :: caller
         logical,dimension(3) :: L
         L(1) = allocated(BC%f)
         L(2) = allocated(BC%e)
         L(3) = allocated(BC%c)
         if (.not.all(L)) then
           write(*,*) 'Error: trying to copy unallocated BCs in '//caller//'in boundary_conditions.f90'
           stop 'Done'
         endif
       end subroutine


       subroutine define_logicals_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         logical,dimension(3) :: L
         BC%defined = BC%GFs_defined.and.BC%BCT_defined.and.BC%vals_defined

         L(1) = all((/(is_Dirichlet(BC%bct_f(i)),i=1,6)/))
         L(2) = all((/(is_Dirichlet(BC%bct_e(i)),i=1,12)/))
         L(3) = all((/(is_Dirichlet(BC%bct_c(i)),i=1,8)/))
         BC%all_Dirichlet = all(L)

         L(1) = all((/(is_Robin(BC%bct_f(i)),i=1,6)/))
         L(2) = all((/(is_Robin(BC%bct_e(i)),i=1,12)/))
         L(3) = all((/(is_Robin(BC%bct_c(i)),i=1,8)/))
         BC%all_Robin = all(L)

         L(1) = all((/(is_Neumann(BC%bct_f(i)),i=1,6)/))
         L(2) = all((/(is_Neumann(BC%bct_e(i)),i=1,12)/))
         L(3) = all((/(is_Neumann(BC%bct_c(i)),i=1,8)/))
         BC%all_Neumann = all(L)
       end subroutine

       function getAllNeumann(BC) result(L)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         logical :: L
         L = BC%all_Neumann
       end function

       function getDirichlet(BC) result(L)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         logical :: L
         L = BC%all_Dirichlet
       end function

       function getAllRobin(BC) result(L)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         logical :: L
         L = BC%all_Robin
       end function

       subroutine init_props_BCs(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         call init_props(BC%D,BC%DL)
         call init_props(BC%N,BC%DL)
         call init_props(BC%P,BC%DL)
         call init_props(BC%R,BC%DL)
         call init_props(BC%S,BC%DL)
         call init_props(BC%A,BC%DL)
       end subroutine

       end module