       module boundary_conditions_extend_mod
       use boundary_conditions_mod
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
       use block_extend_mod
       use block_mod
       use string_mod
       use face_SD_mod
       use face_SD_extend_mod
       use GF_mod
       use bctype_mod
       use BC_logicals_mod
       use IO_tools_mod
       use table_mod
       use boundary_mod
       use boundary_extend_mod
       use procedure_array_mod
       use procedure_array_extend_mod
       use apply_face_BC_op_mod
       use apply_BCs_faces_bridge_mod
       use apply_BCs_faces_bridge_implicit_mod
       implicit none

       private
       public :: init ! Essentials

       ! Setters for type
       public :: init_Dirichlet
       public :: init_Neumann
       public :: init_Robin
       public :: init_periodic
       public :: init_symmetric
       public :: init_antisymmetric

       public :: set_prescribed

       public :: init_PA_face

       public :: get_all_Dirichlet
       public :: get_all_Neumann
       public :: get_all_Robin
       public :: get_any_Dirichlet
       public :: get_any_Neumann
       public :: get_any_Robin
       public :: get_any_Prescribed

       public :: init_props
       public :: defined
       public :: insist_allocated

       public :: restrict
       public :: prolongate

       interface init;               module procedure init_GFs_BCs_DL;         end interface

       interface init;               module procedure init_vals_all_S;         end interface
       interface init;               module procedure init_vals_face_GF;       end interface
       interface init;               module procedure init_val_face_S;         end interface

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
       interface set_prescribed;     module procedure set_prescribed_all;      end interface
       interface set_prescribed;     module procedure set_prescribed_face;     end interface
       interface init_PA_face;       module procedure init_PA_face_BC;         end interface

       interface get_all_Dirichlet;  module procedure get_all_Dirichlet_BCs;   end interface
       interface get_all_Neumann;    module procedure get_all_Neumann_BCs;     end interface
       interface get_all_Robin;      module procedure get_all_Robin_BCs;       end interface
       interface get_any_Dirichlet;  module procedure get_any_Dirichlet_BCs;   end interface
       interface get_any_Neumann;    module procedure get_any_Neumann_BCs;     end interface
       interface get_any_Robin;      module procedure get_any_Robin_BCs;       end interface
       interface get_any_Prescribed; module procedure get_any_Prescribed_BCs;  end interface

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

       subroutine init_vals_all_S(BC,val)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in) :: val
         call init(BC%face,val)
         ! call init(BC%edge,val)
         ! call init(BC%corner,val)
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

       subroutine set_prescribed_all(BC)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer :: i
         do i=1,6; call set_prescribed(BC,i); enddo
       end subroutine
       subroutine set_prescribed_face(BC,face)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         integer,intent(in) :: face
         integer :: dir
         dir = dir_given_face(face)
         call check_prereq(BC)
         call set_prescribed(BC%face,face)
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
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Dirichlet_C,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Neumann_C  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Periodic_C_prescribed ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Robin_C  ,face)
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Dirichlet_C_implicit,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Neumann_C_implicit  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Periodic_C_implicit ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Robin_C_implicit,face)
           else
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Dirichlet_C,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Neumann_C  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Periodic_C ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Robin_C ,face)
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Dirichlet_C_implicit,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Neumann_C_implicit  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Periodic_C_implicit ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Robin_C_implicit    ,face)
           endif
         elseif ( N_along(BC%DL,dir)) then
           if (is_prescribed(BC%face%SB(face)%bct)) then
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Dirichlet_N,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Neumann_N  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Periodic_N_prescribed,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Robin_N    ,face)
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Dirichlet_N_implicit,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Neumann_N_implicit  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Periodic_N_implicit ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Robin_N_implicit    ,face)
           else
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Dirichlet_N,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Neumann_N  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Periodic_N ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add_PA(BC%PA_face_BCs,Robin_N    ,face)
             if (is_Dirichlet(BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Dirichlet_N_implicit,face)
             if (is_Neumann(  BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Neumann_N_implicit  ,face)
             if (is_Periodic( BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Periodic_N_implicit ,face)
             if (is_Robin(    BC%face%SB(face)%bct)) call add_PA(BC%PA_face_implicit_BCs,Robin_N_implicit    ,face)
           endif
           else; stop 'Error: bad DL in init_PA_face_BC in boundary_conditions.f90'
         endif
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

         L(1) = get_all_Dirichlet(BC%face)
         ! L(2) = get_all_Dirichlet(BC%edge)
         ! L(3) = get_all_Dirichlet(BC%corner)
         BC%BCL%all_Dirichlet = L(1) ! all()

         L(1) = get_all_Robin(BC%face)
         ! L(2) = get_all_Robin(BC%edge)
         ! L(3) = get_all_Robin(BC%corner)
         BC%BCL%all_Robin = L(1) ! all()

         L(1) = get_all_Neumann(BC%face)
         ! L(2) = get_all_Neumann(BC%edge)
         ! L(3) = get_all_Neumann(BC%corner)
         BC%BCL%all_Neumann = L(1) ! all()

         L(1) = get_any_Dirichlet(BC%face)
         ! L(2) = get_any_Dirichlet(BC%edge)
         ! L(3) = get_any_Dirichlet(BC%corner)
         BC%BCL%any_Dirichlet = L(1) ! any()

         L(1) = get_any_Robin(BC%face)
         ! L(2) = get_any_Robin(BC%edge)
         ! L(3) = get_any_Robin(BC%corner)
         BC%BCL%any_Robin = L(1) ! any()

         L(1) = get_any_Neumann(BC%face)
         ! L(2) = get_any_Neumann(BC%edge)
         ! L(3) = get_any_Neumann(BC%corner)
         BC%BCL%any_Neumann = L(1) ! any()

         L(1) = get_any_Prescribed(BC%face)
         ! L(2) = get_any_Prescribed(BC%edge)
         ! L(3) = get_any_Prescribed(BC%corner)
         BC%BCL%any_Prescribed = L(1) ! any()
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

       function get_any_Prescribed_BCs(BC) result(L)
         implicit none
         type(boundary_conditions),intent(in) :: BC
         logical :: L
         L = BC%BCL%any_Prescribed
       end function

       subroutine init_props_BCs(BC,c_w,Robin_coeff)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         real(cp),intent(in),dimension(6) :: c_w,Robin_coeff
         call define_logicals(BC)
         call init_mixed(BC%f_BCs,BC%DL)
         call init_Robin_coeff(BC%f_BCs,c_w,Robin_coeff)
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
         real(cp),dimension(6) :: c_w,Robin_coeff
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
           c_w = BC%f_BCs%c_w
           call init_props(BC,c_w,Robin_coeff)
         endif
       end subroutine

       subroutine prolongate_BCs(BC,B,dir)
         implicit none
         type(boundary_conditions),intent(inout) :: BC
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         integer,dimension(3) :: eye
         integer :: x,y,z
         real(cp),dimension(6) :: c_w,Robin_coeff
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
           c_w = BC%f_BCs%c_w
           call init_props(BC,c_w,Robin_coeff)
         endif
       end subroutine

       end module