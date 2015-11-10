       module boundary_mod
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
       public :: boundary
       public :: init_Dirichlet,init_Neumann,init_Periodic,init_symmetric,init_antisymmetric
       public :: init_face,init_edge,init_corner
       public :: init,delete
       public :: print,export

       type boundary
         logical :: Dirichlet,Neumann,Periodic,symmetric,antisymmetric
         logical :: face,edge,corner
         real(cp) :: meanVal
         logical,dimension(3) :: def = .false. ! {bctype,location,value}
         logical :: defined = .false. ! = all(defined)
       end type

       interface init_Dirichlet;      module procedure init_Dirichlet_b;      end interface
       interface init_Neumann;        module procedure init_Neumann_b;        end interface
       interface init_Periodic;       module procedure init_Periodic_b;       end interface
       interface init_symmetric;      module procedure init_symmetric_b;      end interface
       interface init_antisymmetric;  module procedure init_antisymmetric_b;  end interface

       interface init_face;           module procedure init_face_b;           end interface
       interface init_edge;           module procedure init_edge_b;           end interface
       interface init_corner;         module procedure init_corner_b;         end interface

       interface init;                 module procedure init_val0;            end interface
       interface init;                 module procedure init_val1;            end interface
       interface init;                 module procedure init_val2;            end interface
       interface init;                 module procedure init_copy;            end interface
       interface delete;               module procedure delete_boundary;      end interface

       interface print;                module procedure print_boundary;       end interface
       interface export;               module procedure export_boundary;      end interface

       contains

       ! *******************************************************************************
       ! *********************************** TYPE **************************************
       ! *******************************************************************************

       subroutine deleteType(b)
         implicit none
         type(boundary),intent(inout) :: b
         b%Dirichlet = .false.;   b%Neumann = .false.; 
         b%Periodic = .false.;    b%symmetric = .false.; 
         b%antisymmetric = .false.; b%def(1) = .false.; b%defined = all(b%def)
       end subroutine

       subroutine init_Dirichlet_b(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteType(b); b%Dirichlet = .true.; b%def(1) = .true.; b%defined = all(b%def)
       end subroutine
       subroutine init_Neumann_b(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteType(b); b%Neumann = .true.; b%def(1) = .true.; b%defined = all(b%def)
       end subroutine
       subroutine init_Periodic_b(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteType(b); b%Periodic = .true.; b%def(1) = .true.; b%defined = all(b%def)
       end subroutine
       subroutine init_symmetric_b(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteType(b); b%symmetric = .true.; b%def(1) = .true.; b%defined = all(b%def)
       end subroutine
       subroutine init_antisymmetric_b(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteType(b); b%antisymmetric = .true.; b%def(1) = .true.; b%defined = all(b%def)
       end subroutine

       ! *******************************************************************************
       ! ********************************* LOCATION ************************************
       ! *******************************************************************************

       subroutine deleteLocation(b)
         implicit none
         type(boundary),intent(inout) :: b
         b%face = .false.; b%edge = .false.; b%corner = .false.; b%def(2) = .false.; b%defined = all(b%def)
       end subroutine

       subroutine init_face_b(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteLocation(b); b%face = .true.; b%def(2) = .true.; b%defined = all(b%def)
       end subroutine
       subroutine init_edge_b(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteLocation(b); b%edge = .true.; b%def(2) = .true.; b%defined = all(b%def)
       end subroutine
       subroutine init_corner_b(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteLocation(b); b%corner = .true.; b%def(2) = .true.; b%defined = all(b%def)
       end subroutine

       ! *******************************************************************************
       ! ********************************** VAL ****************************************
       ! *******************************************************************************

       subroutine init_val0(b,val)
         implicit none
         type(boundary),intent(inout) :: b
         real(cp),intent(in) :: val
         b%meanVal = val; b%def(3) = .true.; b%defined = all(b%def)
       end subroutine

       subroutine init_val1(b,val)
         implicit none
         type(boundary),intent(inout) :: b
         real(cp),dimension(:),intent(in) :: val
         b%meanVal = sum(val)/size(val); b%def(3) = .true.; b%defined = all(b%def)
       end subroutine

       subroutine init_val2(b,val)
         implicit none
         type(boundary),intent(inout) :: b
         real(cp),dimension(:,:),intent(in) :: val
         b%meanVal = sum(val)/size(val); b%def(3) = .true.; b%defined = all(b%def)
       end subroutine

       ! *******************************************************************************
       ! ******************************** COPY & DELETE ********************************
       ! *******************************************************************************

       subroutine init_copy(b_out,b_in)
         implicit none
         type(boundary),intent(inout) :: b_out
         type(boundary),intent(in) :: b_in
         if (.not.b_in%defined) stop 'Error: trying to copy BC that has not been fully defined'
         b_out%Dirichlet = b_in%Dirichlet
         b_out%Neumann = b_in%Neumann
         b_out%Periodic = b_in%Periodic
         b_out%symmetric = b_in%symmetric
         b_out%antisymmetric = b_in%antisymmetric

         b_out%face = b_in%face
         b_out%edge = b_in%edge
         b_out%corner = b_in%corner

         b_out%meanVal = b_in%meanVal
         b_out%def = b_in%def
         b_out%defined = b_in%defined
       end subroutine

       subroutine delete_boundary(b)
         implicit none
         type(boundary),intent(inout) :: b
         call deleteType(b)
         call deleteLocation(b)
         call init(b,0.0_cp)
         b%def = .false.
         b%defined = all(b%def)
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_boundary(b)
         implicit none
         type(boundary), intent(in) :: b
         call export(b,6)
       end subroutine

       subroutine export_boundary(b,NewU)
         implicit none
         type(boundary),intent(in) :: b
         integer,intent(in) :: NewU
         logical :: TF
         TF = .false. ! show all regardless

         if (.not.b%defined) then 
           write(newU,*) 'def = ',b%def
           write(newU,*) 'defined = ',b%defined
           stop 'Error: trying to export boundary before fully defined'
         endif
         if (TF.or.b%Dirichlet)     write(newU,*) 'Dirichlet = ',b%Dirichlet
         if (TF.or.b%Neumann)       write(newU,*) 'Neumann = ',b%Neumann
         if (TF.or.b%Periodic)      write(newU,*) 'Periodic = ',b%Periodic
         if (TF.or.b%symmetric)     write(newU,*) 'symmetric = ',b%symmetric
         if (TF.or.b%antisymmetric) write(newU,*) 'antisymmetric = ',b%antisymmetric
         if (TF.or.b%face)          write(newU,*) 'face = ',b%face
         if (TF.or.b%edge)          write(newU,*) 'edge = ',b%edge
         if (TF.or.b%corner)        write(newU,*) 'corner = ',b%corner

         write(newU,*) 'def = ',b%def
         write(newU,*) 'defined = ',b%defined
         write(newU,*) 'meanVal = ',b%meanVal
       end subroutine

       end module