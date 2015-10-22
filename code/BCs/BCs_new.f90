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

       use grid_mod
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

       integer,parameter :: Dirichlet_n = 1    ! Correspond to applyBCs.f90
       integer,parameter :: Dirichlet_cc = 2   ! Correspond to applyBCs.f90
       integer,parameter :: Neumann_n = 3      ! Correspond to applyBCs.f90
       ! integer,parameter :: Neumann_cc = 4   ! Correspond to applyBCs.f90
       integer,parameter :: Neumann_cc = 5     ! Correspond to applyBCs.f90
       integer,parameter :: periodic_n = 6     ! Correspond to applyBCs.f90
       integer,parameter :: periodic_cc = 7    ! Correspond to applyBCs.f90

       private
       public :: BCs
       public :: init,delete
       public :: print,export

       ! Setters for type
       public :: init_Dirichlet
       public :: init_Neumann
       public :: init_periodic

       public :: getAllNeumann
       public :: getDirichlet

       public :: print_defined

       type BCs
         type(face),dimension(6) :: f
         type(edge),dimension(12) :: e
         type(corner),dimension(8) :: c
         type(grid) :: g
         integer,dimension(3) :: s
         logical :: gridDefined = .false.
         logical :: defined = .false.
         logical :: all_Dirichlet,all_Neumann
       end type

       interface init;            module procedure init_BCs_copy;       end interface

       interface init;            module procedure init_gridShape_BCs;  end interface

       interface init_Dirichlet;  module procedure init_Dirichlet_all;  end interface
       interface init_Dirichlet;  module procedure init_Dirichlet_face; end interface
       interface init_Neumann;    module procedure init_Neumann_all;    end interface
       interface init_Neumann;    module procedure init_Neumann_face;   end interface
       interface init_periodic;   module procedure init_periodic_all;   end interface
       interface init_periodic;   module procedure init_periodic_face;  end interface

       interface init;            module procedure init_vals_all_S;     end interface
       interface init;            module procedure init_vals_face_vals; end interface
       interface init;            module procedure init_val_face_S;     end interface

       interface delete;          module procedure delete_BCs;          end interface
       interface print;           module procedure print_BCs;           end interface
       interface export;          module procedure export_BCs;          end interface

       contains

       ! *******************************************************************************
       ! ********************************** INIT GRID (1) ******************************
       ! *******************************************************************************

       subroutine init_gridShape_BCs(b,g,s)
         implicit none
         type(BCs),intent(inout) :: b
         type(grid),intent(in) :: g
         integer,dimension(3),intent(in) :: s
         call init(b%g,g); b%s = s
         call init(b%f(1),(/s(2),s(3)/))
         call init(b%f(2),(/s(2),s(3)/))
         call init(b%f(3),(/s(1),s(3)/))
         call init(b%f(4),(/s(1),s(3)/))
         call init(b%f(5),(/s(1),s(2)/))
         call init(b%f(6),(/s(1),s(2)/))

         do i=1,4;  call init(b%e(i),s(1)); enddo
         do i=5,8;  call init(b%e(i),s(2)); enddo
         do i=9,12; call init(b%e(i),s(3)); enddo

         b%gridDefined = .true.
         call define_logicals(b)
       end subroutine

       ! *******************************************************************************
       ! ********************************** INIT TYPE (2) ******************************
       ! *******************************************************************************

       subroutine init_ND_all(b,bctype_n,bctype_cc)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: bctype_n,bctype_cc
         integer :: k
         if (.not.b%gridDefined) stop 'Error: BC grid must be defined before type is defined'
         do k=1,6; call init_ND_face(b,k,bctype_n,bctype_cc); enddo
         call define_logicals(b)
       end subroutine

       subroutine init_ND_face(b,f,bctype_n,bctype_cc)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: f,bctype_n,bctype_cc
         if (.not.b%gridDefined) stop 'Error: BC grid must be defined before type is defined'
         select case (f)
         case (1,2); if (b%s(1).eq.b%g%c(1)%sn) then; call init(b%f(f),bctype_n)
                 elseif (b%s(1).eq.b%g%c(1)%sc) then; call init(b%f(f),bctype_cc)
                 endif
         case (3,4); if (b%s(2).eq.b%g%c(2)%sn) then; call init(b%f(f),bctype_n)
                 elseif (b%s(2).eq.b%g%c(2)%sc) then; call init(b%f(f),bctype_cc)
                 endif
         case (5,6); if (b%s(3).eq.b%g%c(3)%sn) then; call init(b%f(f),bctype_n)
                 elseif (b%s(3).eq.b%g%c(3)%sc) then; call init(b%f(f),bctype_cc)
                 endif
         end select
         call define_logicals(b)
       end subroutine

       subroutine init_Dirichlet_all(b)
         implicit none
         type(BCs),intent(inout) :: b
         call check_prereq(b)
         call init_ND_all(b,Dirichlet_n,Dirichlet_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Neumann_all(b)
         implicit none
         type(BCs),intent(inout) :: b
         call check_prereq(b)
         call init_ND_all(b,Neumann_n,Neumann_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Periodic_all(b)
         implicit none
         type(BCs),intent(inout) :: b
         call check_prereq(b)
         call init_ND_all(b,periodic_n,periodic_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Dirichlet_face(b,f)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: f
         call check_prereq(b)
         call init_ND_face(b,f,Dirichlet_n,Dirichlet_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Neumann_face(b,f)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: f
         call check_prereq(b)
         call init_ND_face(b,f,Neumann_n,Neumann_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Periodic_face(b,f)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: f
         call check_prereq(b)
         call init_ND_face(b,f,periodic_n,periodic_cc)
         call define_logicals(b)
       end subroutine

       ! *******************************************************************************
       ! ********************************** INIT VALS (3) ******************************
       ! *******************************************************************************

       subroutine init_vals_all_S(b,val)
         implicit none
         type(BCs),intent(inout) :: b
         real(cp),intent(in) :: val
         integer :: i
         do i=1,6; call init(b%f(i),val); enddo
         call define_logicals(b)
       end subroutine

       subroutine init_vals_face_vals(b,vals,face)
         implicit none
         type(BCs),intent(inout) :: b
         real(cp),dimension(:,:),intent(in) :: vals
         integer,intent(in) :: face
         call init(b%f(face),vals)
         call define_logicals(b)
       end subroutine

       subroutine init_val_face_S(b,val,face)
         implicit none
         type(BCs),intent(inout) :: b
         real(cp),intent(in) :: val
         integer,intent(in) :: face
         call init(b%f(face),val)
         call define_logicals(b)
       end subroutine

       ! *******************************************************************************
       ! ******************************** COPY / DELETE ********************************
       ! *******************************************************************************

       subroutine init_BCs_copy(b_out,b_in)
         implicit none
         type(BCs),intent(inout) :: b_out
         type(BCs),intent(in) :: b_in
         integer :: i
         do i=1,6; call init(b_out%f(i),b_in%f(i)); enddo
         call init(b_out%g,b_in%g)
         b_out%s = b_in%s
         b_out%defined = b_in%defined
         b_out%all_Dirichlet = b_in%all_Dirichlet
         b_out%all_Neumann = b_in%all_Neumann
       end subroutine

       subroutine delete_BCs(b)
         implicit none
         type(BCs),intent(inout) :: b
         integer :: i
         do i=1,6; call delete(b%f(i)); enddo
         do i=1,12; call delete(b%e(i)); enddo
         do i=1,8; call delete(b%c(i)); enddo
         call delete(b%g)
         b%gridDefined = .false.
         call define_logicals(b)
       end subroutine

       ! *******************************************************************************
       ! ******************************* PRINT / EXPORT ********************************
       ! *******************************************************************************

       subroutine print_BCs(b,name)
         implicit none
         type(BCs), intent(in) :: b
         character(len=*),intent(in) :: name
         call exp_AllBCs(b,name,6)
       end subroutine

       subroutine export_BCs(b,dir,name)
         implicit none
         type(BCs), intent(in) :: b
         character(len=*),intent(in) :: dir,name
         integer :: NewU
         NewU = newAndOpen(dir,name//'_BoundaryConditions')
         call exp_AllBCs(b,name,newU)
         call closeAndMessage(newU,name//'_BoundaryConditions',dir)
       end subroutine

       subroutine exp_AllBCs(b,name,newU)
         implicit none
         type(BCs), intent(in) :: b
         character(len=*),intent(in) :: name
         integer,intent(in) :: NewU
         write(newU,*) 'Boundary conditions for ' // trim(adjustl(name))
         if (b%defined) then
           call exp_BC(1,b%f(1)%bctype,newU)
           call exp_BC(2,b%f(2)%bctype,newU)
           call exp_BC(3,b%f(3)%bctype,newU)
           call exp_BC(4,b%f(4)%bctype,newU)
           call exp_BC(5,b%f(5)%bctype,newU)
           call exp_BC(6,b%f(6)%bctype,newU)
         endif
       end subroutine

       subroutine exp_BC(face,bctype,NewU)
         implicit none
         integer,intent(in) :: NewU,face,bctype
         if (face.eq.1) then; write(newU,'(7A)',advance='no') ' xmin: '; endif
         if (face.eq.2) then; write(newU,'(7A)',advance='no') ' xmax: '; endif
         if (face.eq.3) then; write(newU,'(7A)',advance='no') ' ymin: '; endif
         if (face.eq.4) then; write(newU,'(7A)',advance='no') ' ymax: '; endif
         if (face.eq.5) then; write(newU,'(7A)',advance='no') ' zmin: '; endif
         if (face.eq.6) then; write(newU,'(7A)',advance='no') ' zmax: '; endif
         if (bctype.eq.1) then; write(newU,*) 'Dirichlet - direct - wall coincident'; endif
         if (bctype.eq.2) then; write(newU,*) 'Dirichlet - interpolated - wall incoincident'; endif
         if (bctype.eq.3) then; write(newU,*) 'Neumann - direct - wall coincident ~O(dh^2)'; endif
         if (bctype.eq.4) then; write(newU,*) 'Neumann - direct - wall coincident ~O(dh)'; endif
         if (bctype.eq.5) then; write(newU,*) 'Neumann - interpolated - wall incoincident O(dh)'; endif
         if (bctype.eq.6) then; write(newU,*) 'Periodic - direct - wall coincident ~O(dh)'; endif
         if (bctype.eq.7) then; write(newU,*) 'Periodic - interpolated - wall incoincident ~O(dh)'; endif
         if (bctype.eq.8) then; write(newU,*) 'Periodic - interpolated - wall incoincident ~O(dh^2)'; endif
       end subroutine

       ! *******************************************************************************
       ! ********************************* AUXILIARY ***********************************
       ! *******************************************************************************

       subroutine check_prereq(b)
         implicit none
         type(BCs),intent(in) :: b
         if (.not.b%gridDefined) stop 'Error: BC grid must be defined before type is defined'
       end subroutine

       subroutine define_logicals(b)
         implicit none
         type(BCs),intent(inout) :: b
         integer :: i
         b%defined = all((/b%gridDefined,(b%f(i)%defined,i=1,6)/))

         b%all_Dirichlet = all((/((b%f(i)%bctype.eq.Dirichlet_cc).or.&
                                  (b%f(i)%bctype.eq.Dirichlet_n),i=1,6)/))

         b%all_Neumann = all((/((b%f(i)%bctype.eq.Neumann_cc).or.&
                                (b%f(i)%bctype.eq.Neumann_n),i=1,6)/))
       end subroutine

       subroutine print_defined(b)
         implicit none
         type(BCs),intent(in) :: b
         write(*,*) 'face(1) {type,valule} = ',b%f(1)%def
         write(*,*) 'face(2) {type,valule} = ',b%f(2)%def
         write(*,*) 'face(3) {type,valule} = ',b%f(3)%def
         write(*,*) 'face(4) {type,valule} = ',b%f(4)%def
         write(*,*) 'face(5) {type,valule} = ',b%f(5)%def
         write(*,*) 'face(6) {type,valule} = ',b%f(6)%def
         write(*,*) ''
         write(*,*) 'face(1) {type} = ',b%f(1)%bctype
         write(*,*) 'face(2) {type} = ',b%f(2)%bctype
         write(*,*) 'face(3) {type} = ',b%f(3)%bctype
         write(*,*) 'face(4) {type} = ',b%f(4)%bctype
         write(*,*) 'face(5) {type} = ',b%f(5)%bctype
         write(*,*) 'face(6) {type} = ',b%f(6)%bctype
         write(*,*) ''
         write(*,*) 'face(1) {val} = ',b%f(1)%val
         write(*,*) 'face(2) {val} = ',b%f(2)%val
         write(*,*) 'face(3) {val} = ',b%f(3)%val
         write(*,*) 'face(4) {val} = ',b%f(4)%val
         write(*,*) 'face(5) {val} = ',b%f(5)%val
         write(*,*) 'face(6) {val} = ',b%f(6)%val
         write(*,*) ''
         write(*,*) 'face(1) {s} = ',b%f(1)%s
         write(*,*) 'face(2) {s} = ',b%f(2)%s
         write(*,*) 'face(3) {s} = ',b%f(3)%s
         write(*,*) 'face(4) {s} = ',b%f(4)%s
         write(*,*) 'face(5) {s} = ',b%f(5)%s
         write(*,*) 'face(6) {s} = ',b%f(6)%s
         write(*,*) ''
         write(*,*) 'b_grid = ',b%gridDefined
         write(*,*) 'b_all = ',b%defined
       end subroutine

       function getAllNeumann(b) result(TF)
         implicit none
         type(BCs),intent(inout) :: b
         logical :: TF
         TF = b%all_Neumann
       end function

       function getDirichlet(b) result(TF)
         implicit none
         type(BCs),intent(inout) :: b
         logical :: TF
         TF = b%all_Dirichlet
       end function

       end module