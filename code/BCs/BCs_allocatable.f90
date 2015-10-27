       module BC_mod
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

       use mesh_mod
       use SF_mod
       use boundary_mod
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

       integer,parameter :: Dirichlet_n = 1      ! Correspond to applyBCs.f90
       integer,parameter :: Dirichlet_cc = 2     ! Correspond to applyBCs.f90
       integer,parameter :: Neumann_n = 3        ! Correspond to applyBCs.f90
       ! integer,parameter :: Neumann_cc = 4     ! Correspond to applyBCs.f90
       integer,parameter :: Neumann_cc = 5       ! Correspond to applyBCs.f90
       integer,parameter :: periodic_n = 6       ! Correspond to applyBCs.f90
       integer,parameter :: periodic_cc = 7      ! Correspond to applyBCs.f90
       integer,parameter :: antisymmetry_n = 9   ! Correspond to applyBCs.f90
       integer,parameter :: antisymmetry_cc = 10 ! Correspond to applyBCs.f90

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

       public :: print_defined

       type BCs
         integer :: s
         type(BC_grid),dimension(:),allocatable :: BC_g
         type(mesh) :: m
         logical :: meshDefined = .false.
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

       subroutine init_BCs(B,m,U)
         implicit none
         type(BCs),intent(inout) :: b
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: U
         call init(b%m,m)
         call init(b%face(1),(/s(2),s(3)/))
         call init(b%face(2),(/s(2),s(3)/))
         call init(b%face(3),(/s(1),s(3)/))
         call init(b%face(4),(/s(1),s(3)/))
         call init(b%face(5),(/s(1),s(2)/))
         call init(b%face(6),(/s(1),s(2)/))
         b%gridDefined = .true.
         call define_logicals(b)
       end subroutine

       ! *******************************************************************************
       ! ********************************** INIT TYPE (2) ******************************
       ! *******************************************************************************

       subroutine init_BCType_all(b,bctype_n,bctype_cc)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: bctype_n,bctype_cc
         integer :: k
         if (.not.b%gridDefined) stop 'Error: BC grid must be defined before type is defined'
         do k=1,6; call init_BCType_face(b,k,bctype_n,bctype_cc); enddo
         call define_logicals(b)
       end subroutine

       subroutine init_BCType_face(b,face,bctype_n,bctype_cc)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: face,bctype_n,bctype_cc
         if (.not.b%gridDefined) stop 'Error: BC grid must be defined before type is defined'
         select case (face)
         case (1,2); if (b%s(1).eq.b%g%c(1)%sn) then; call init(b%face(face),bctype_n)
                 elseif (b%s(1).eq.b%g%c(1)%sc) then; call init(b%face(face),bctype_cc)
                 else; stop 'Error: size mismatch in init_BCType_face in BCs.f90'
                 endif
         case (3,4); if (b%s(2).eq.b%g%c(2)%sn) then; call init(b%face(face),bctype_n)
                 elseif (b%s(2).eq.b%g%c(2)%sc) then; call init(b%face(face),bctype_cc)
                 else; stop 'Error: size mismatch in init_BCType_face in BCs.f90'
                 endif
         case (5,6); if (b%s(3).eq.b%g%c(3)%sn) then; call init(b%face(face),bctype_n)
                 elseif (b%s(3).eq.b%g%c(3)%sc) then; call init(b%face(face),bctype_cc)
                 else; stop 'Error: size mismatch in init_BCType_face in BCs.f90'
                 endif
         case default; stop 'Error: case not found in init_BCType_face in BCs.f90'
         end select
         call define_logicals(b)
       end subroutine

       subroutine init_Dirichlet_all(b)
         implicit none
         type(BCs),intent(inout) :: b
         call check_prereq(b)
         call init_BCType_all(b,Dirichlet_n,Dirichlet_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Neumann_all(b)
         implicit none
         type(BCs),intent(inout) :: b
         call check_prereq(b)
         call init_BCType_all(b,Neumann_n,Neumann_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Periodic_all(b)
         implicit none
         type(BCs),intent(inout) :: b
         call check_prereq(b)
         call init_BCType_all(b,periodic_n,periodic_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_antisymmetry_all(b)
         implicit none
         type(BCs),intent(inout) :: b
         call check_prereq(b)
         call init_BCType_all(b,periodic_n,periodic_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Dirichlet_face(b,face)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: face
         call check_prereq(b)
         call init_BCType_face(b,face,Dirichlet_n,Dirichlet_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Neumann_face(b,face)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: face
         call check_prereq(b)
         call init_BCType_face(b,face,Neumann_n,Neumann_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_Periodic_face(b,face)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: face
         call check_prereq(b)
         call init_BCType_face(b,face,periodic_n,periodic_cc)
         call define_logicals(b)
       end subroutine

       subroutine init_antisymmetry_face(b,face)
         implicit none
         type(BCs),intent(inout) :: b
         integer,intent(in) :: face
         call check_prereq(b)
         call init_BCType_face(b,face,antisymmetry_n,antisymmetry_cc)
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
         do i=1,6; call init(b%face(i),val); enddo
         call define_logicals(b)
       end subroutine

       subroutine init_vals_face_vals(b,vals,face)
         implicit none
         type(BCs),intent(inout) :: b
         real(cp),dimension(:,:),intent(in) :: vals
         integer,intent(in) :: face
         call init(b%face(face),vals)
         call define_logicals(b)
       end subroutine

       subroutine init_val_face_S(b,val,face)
         implicit none
         type(BCs),intent(inout) :: b
         real(cp),intent(in) :: val
         integer,intent(in) :: face
         call init(b%face(face),val)
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
         do i=1,6; call init(b_out%face(i),b_in%face(i)); enddo
         call init(b_out%g,b_in%g)
         b_out%s = b_in%s
         b_out%gridDefined = b_in%gridDefined
         b_out%defined = b_in%defined
         b_out%all_Dirichlet = b_in%all_Dirichlet
         b_out%all_Neumann = b_in%all_Neumann
       end subroutine

       subroutine delete_BCs(b)
         implicit none
         type(BCs),intent(inout) :: b
         integer :: i
         do i=1,6; call delete(b%face(i)); enddo
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
           call exp_BC(1,b%face(1)%bctype,newU)
           call exp_BC(2,b%face(2)%bctype,newU)
           call exp_BC(3,b%face(3)%bctype,newU)
           call exp_BC(4,b%face(4)%bctype,newU)
           call exp_BC(5,b%face(5)%bctype,newU)
           call exp_BC(6,b%face(6)%bctype,newU)
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
         if (bctype.eq.9) then; write(newU,*) 'Anti-symmetry - direct - wall coincident'; endif
         if (bctype.eq.10) then; write(newU,*) 'Anti-symmetry - interpolated - wall incoincident'; endif
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
         b%defined = all((/b%gridDefined,(b%face(i)%defined,i=1,6)/))

         b%all_Dirichlet = all((/((b%face(i)%bctype.eq.Dirichlet_cc).or.&
                                  (b%face(i)%bctype.eq.Dirichlet_n),i=1,6)/))

         b%all_Neumann = all((/((b%face(i)%bctype.eq.Neumann_cc).or.&
                                (b%face(i)%bctype.eq.Neumann_n),i=1,6)/))
       end subroutine

       subroutine print_defined(b)
         implicit none
         type(BCs),intent(in) :: b
         write(*,*) 'face(1) {type,valule} = ',b%face(1)%def
         write(*,*) 'face(2) {type,valule} = ',b%face(2)%def
         write(*,*) 'face(3) {type,valule} = ',b%face(3)%def
         write(*,*) 'face(4) {type,valule} = ',b%face(4)%def
         write(*,*) 'face(5) {type,valule} = ',b%face(5)%def
         write(*,*) 'face(6) {type,valule} = ',b%face(6)%def
         write(*,*) ''
         write(*,*) 'face(1) {type} = ',b%face(1)%bctype
         write(*,*) 'face(2) {type} = ',b%face(2)%bctype
         write(*,*) 'face(3) {type} = ',b%face(3)%bctype
         write(*,*) 'face(4) {type} = ',b%face(4)%bctype
         write(*,*) 'face(5) {type} = ',b%face(5)%bctype
         write(*,*) 'face(6) {type} = ',b%face(6)%bctype
         write(*,*) ''
         write(*,*) 'face(1) {val} = ',b%face(1)%val
         write(*,*) 'face(2) {val} = ',b%face(2)%val
         write(*,*) 'face(3) {val} = ',b%face(3)%val
         write(*,*) 'face(4) {val} = ',b%face(4)%val
         write(*,*) 'face(5) {val} = ',b%face(5)%val
         write(*,*) 'face(6) {val} = ',b%face(6)%val
         write(*,*) ''
         write(*,*) 'face(1) {s} = ',b%face(1)%s
         write(*,*) 'face(2) {s} = ',b%face(2)%s
         write(*,*) 'face(3) {s} = ',b%face(3)%s
         write(*,*) 'face(4) {s} = ',b%face(4)%s
         write(*,*) 'face(5) {s} = ',b%face(5)%s
         write(*,*) 'face(6) {s} = ',b%face(6)%s
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