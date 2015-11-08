       module BCs_mod
       ! Making BCs is a 3 step process:
       ! 
       !       1) Set grid / shape
       !             call init(BCs,g,s)
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

       use grid_mod
       use bctype_mod
       use face_mod
       use edge_mod
       use corner_mod
       use IO_tools_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private
       public :: BCs
       public :: init,delete
       public :: print,export

       ! Setters for type
       public :: init_Dirichlet
       public :: init_Neumann
       public :: init_periodic
       public :: init_antisymmetry

       public :: getAllNeumann
       public :: getDirichlet

       type BCs
         type(face),dimension(6) :: f   ! xmin,xmax,ymin,ymax,zmin,zmax
         type(edge),dimension(12) :: e  ! {1:12} = {x,y,z: minmin,minmax,maxmin,maxmax}
         type(corner),dimension(8) :: c ! 
         type(grid) :: g
         integer,dimension(3) :: s
         logical :: gridDefined = .false.
         logical :: defined = .false.
         logical :: all_Dirichlet,all_Neumann
       end type

       interface init;                module procedure init_BCs_copy;           end interface

       interface init;                module procedure init_gridShape_BCs;      end interface

       interface init_Dirichlet;      module procedure init_Dirichlet_all;      end interface
       interface init_Dirichlet;      module procedure init_Dirichlet_face;     end interface
       interface init_Neumann;        module procedure init_Neumann_all;        end interface
       interface init_Neumann;        module procedure init_Neumann_face;       end interface
       interface init_periodic;       module procedure init_periodic_all;       end interface
       interface init_periodic;       module procedure init_periodic_face;      end interface
       interface init_antisymmetry;   module procedure init_antisymmetry_all;   end interface
       interface init_antisymmetry;   module procedure init_antisymmetry_face;  end interface

       interface init;                module procedure init_vals_all_S;         end interface
       interface init;                module procedure init_vals_face_vals;     end interface
       interface init;                module procedure init_val_face_S;         end interface

       interface delete;              module procedure delete_BCs;              end interface
       interface print;               module procedure print_BCs;               end interface
       interface export;              module procedure export_BCs;              end interface

       contains

       ! *******************************************************************************
       ! ********************************** INIT GRID (1) ******************************
       ! *******************************************************************************

       subroutine init_gridShape_BCs(BC,g,s)
         implicit none
         type(BCs),intent(inout) :: BC
         type(grid),intent(in) :: g
         integer,dimension(3),intent(in) :: s
         integer :: i
         call init(BC%g,g); BC%s = s
         call init(BC%f(1),(/s(2),s(3)/))
         call init(BC%f(2),(/s(2),s(3)/))
         call init(BC%f(3),(/s(1),s(3)/))
         call init(BC%f(4),(/s(1),s(3)/))
         call init(BC%f(5),(/s(1),s(2)/))
         call init(BC%f(6),(/s(1),s(2)/))

         do i=1,4;  call init(BC%e(i),s(1)); enddo
         do i=5,8;  call init(BC%e(i),s(2)); enddo
         do i=9,12; call init(BC%e(i),s(3)); enddo

         BC%gridDefined = .true.
         call define_logicals(BC)
       end subroutine

       ! *******************************************************************************
       ! ********************************** INIT TYPE (2) ******************************
       ! *******************************************************************************

       subroutine init_Dirichlet_all(BC)
         implicit none
         type(BCs),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_Dirichlet(BC%f(i)%b); enddo
         do i=1,12; call init_Dirichlet(BC%e(i)%b); enddo
         do i=1,8;  call init_Dirichlet(BC%c(i)%b); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_Neumann_all(BC)
         implicit none
         type(BCs),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_Neumann(BC%f(i)%b); enddo
         do i=1,12; call init_Neumann(BC%e(i)%b); enddo
         do i=1,8;  call init_Neumann(BC%c(i)%b); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_Periodic_all(BC)
         implicit none
         type(BCs),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_Periodic(BC%f(i)%b); enddo
         do i=1,12; call init_Periodic(BC%e(i)%b); enddo
         do i=1,8;  call init_Periodic(BC%c(i)%b); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_antisymmetry_all(BC)
         implicit none
         type(BCs),intent(inout) :: BC
         integer :: i
         call check_prereq(BC)
         do i=1,6;  call init_antisymmetric(BC%f(i)%b); enddo
         do i=1,12; call init_antisymmetric(BC%e(i)%b); enddo
         do i=1,8;  call init_antisymmetric(BC%c(i)%b); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_Dirichlet_face(BC,face)
         implicit none
         type(BCs),intent(inout) :: BC
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Dirichlet(BC%f(face)%b)
         call define_logicals(BC)
       end subroutine

       subroutine init_Neumann_face(BC,face)
         implicit none
         type(BCs),intent(inout) :: BC
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Neumann(BC%f(face)%b)
         call define_logicals(BC)
       end subroutine

       subroutine init_Periodic_face(BC,face)
         implicit none
         type(BCs),intent(inout) :: BC
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_Periodic(BC%f(face)%b)
         call define_logicals(BC)
       end subroutine

       subroutine init_antisymmetry_face(BC,face)
         implicit none
         type(BCs),intent(inout) :: BC
         integer,intent(in) :: face
         call check_prereq(BC)
         call init_antisymmetric(BC%f(face)%b)
         call define_logicals(BC)
       end subroutine

       ! *******************************************************************************
       ! ********************************** INIT VALS (3) ******************************
       ! *******************************************************************************

       subroutine init_vals_all_S(BC,val)
         implicit none
         type(BCs),intent(inout) :: BC
         real(cp),intent(in) :: val
         integer :: i
         do i=1,6;  call init(BC%f(i),val); enddo
         do i=1,12; call init(BC%e(i),val); enddo
         do i=1,8;  call init(BC%c(i),val); enddo
         call define_logicals(BC)
       end subroutine

       subroutine init_vals_face_vals(BC,vals,face)
         implicit none
         type(BCs),intent(inout) :: BC
         real(cp),dimension(:,:),intent(in) :: vals
         integer,intent(in) :: face
         call init(BC%f(face),vals)
         call define_logicals(BC)
       end subroutine

       subroutine init_val_face_S(BC,val,face)
         implicit none
         type(BCs),intent(inout) :: BC
         real(cp),intent(in) :: val
         integer,intent(in) :: face
         call init(BC%f(face),val)
         call define_logicals(BC)
       end subroutine

       ! *******************************************************************************
       ! ******************************** COPY / DELETE ********************************
       ! *******************************************************************************

       subroutine init_BCs_copy(b_out,b_in)
         implicit none
         type(BCs),intent(inout) :: b_out
         type(BCs),intent(in) :: b_in
         integer :: i
         do i=1,6;  call init(b_out%f(i),b_in%f(i)); enddo
         do i=1,12; call init(b_out%e(i),b_in%e(i)); enddo
         do i=1,8;  call init(b_out%c(i),b_in%c(i)); enddo
         call init(b_out%g,b_in%g)
         b_out%s = b_in%s
         b_out%gridDefined = b_in%gridDefined
         b_out%defined = b_in%defined
         b_out%all_Dirichlet = b_in%all_Dirichlet
         b_out%all_Neumann = b_in%all_Neumann
       end subroutine

       subroutine delete_BCs(BC)
         implicit none
         type(BCs),intent(inout) :: BC
         integer :: i
         do i=1,6;  call delete(BC%f(i)); enddo
         do i=1,12; call delete(BC%e(i)); enddo
         do i=1,8;  call delete(BC%c(i)); enddo
         call delete(BC%g)
         BC%gridDefined = .false.
         BC%s = 0
         call define_logicals(BC)
       end subroutine

       ! *******************************************************************************
       ! ******************************* PRINT / EXPORT ********************************
       ! *******************************************************************************

       subroutine print_BCs(BC,name)
         implicit none
         type(BCs), intent(in) :: BC
         character(len=*),intent(in) :: name
         call exp_BCs(BC,name,6)
       end subroutine

       subroutine export_BCs(BC,dir,name)
         implicit none
         type(BCs), intent(in) :: BC
         character(len=*),intent(in) :: dir,name
         integer :: NewU
         NewU = newAndOpen(dir,name//'_BoundaryConditions')
         call exp_BCs(BC,name,newU)
         call closeAndMessage(newU,name//'_BoundaryConditions',dir)
       end subroutine

       subroutine exp_BCs(BC,name,newU)
         implicit none
         type(BCs), intent(in) :: BC
         character(len=*),intent(in) :: name
         integer,intent(in) :: NewU
         integer :: i
         write(newU,*) 'Boundary conditions for ' // trim(adjustl(name))

         write(newU,*) 'Faces  : {xmin,xmax,ymin,ymax,zmin,zmax}'
         write(newU,*) 'Edges  : {minmin,minmax,maxmin,maxmax} (x: y-z)'
         write(newU,*) '       : {minmin,minmax,maxmin,maxmax} (y: x-z)'
         write(newU,*) '       : {minmin,minmax,maxmin,maxmax} (z: x-y)'
         write(newU,*) 'Corners: {min(x,y,z), max(x,y,z), min(y,z)/max(x)}'
         if (BC%defined) then
             write(newU,*) ' -------------- FACES -------------- '
           do i=1,6
             write(newU,*) 'Face ',i
             call export(BC%f(i),newU)
           enddo

             write(newU,*) ' -------------- EDGES -------------- '
           do i=1,12
             write(newU,*) 'Edge ',i
             call export(BC%e(i),newU)
           enddo

             write(newU,*) ' ------------- CORNERS ------------- '
           do i=1,8
             write(newU,*) 'Corner ',i
             call export(BC%c(i),newU)
           enddo
         endif
       end subroutine

       ! *******************************************************************************
       ! ********************************* AUXILIARY ***********************************
       ! *******************************************************************************

       subroutine check_prereq(BC)
         implicit none
         type(BCs),intent(in) :: BC
         if (.not.BC%gridDefined) stop 'Error: BC grid must be defined before type is defined'
       end subroutine

       subroutine define_logicals(BC)
         implicit none
         type(BCs),intent(inout) :: BC
         integer :: i
         logical,dimension(3) :: TF
         TF(1) = all((/BC%gridDefined,(BC%f(i)%defined,i=1,6)/))
         TF(2) = all((/BC%gridDefined,(BC%e(i)%defined,i=1,12)/))
         TF(3) = all((/BC%gridDefined,(BC%c(i)%defined,i=1,8)/))
         BC%defined = all(TF)

         TF(1) = all((/(BC%f(i)%b%Dirichlet,i=1,6)/))
         TF(2) = all((/(BC%e(i)%b%Dirichlet,i=1,12)/))
         TF(3) = all((/(BC%c(i)%b%Dirichlet,i=1,8)/))
         BC%all_Dirichlet = all(TF)

         TF(1) = all((/(BC%f(i)%b%Neumann,i=1,6)/))
         TF(2) = all((/(BC%e(i)%b%Neumann,i=1,12)/))
         TF(3) = all((/(BC%c(i)%b%Neumann,i=1,8)/))
         BC%all_Neumann = all(TF)
       end subroutine

       function getAllNeumann(BC) result(TF)
         implicit none
         type(BCs),intent(inout) :: BC
         logical :: TF
         TF = BC%all_Neumann
       end function

       function getDirichlet(BC) result(TF)
         implicit none
         type(BCs),intent(inout) :: BC
         logical :: TF
         TF = BC%all_Dirichlet
       end function

       end module