       module face_mod
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
       public :: face
       public :: init,delete
       public :: print,export

       type face
         integer :: bctype
         real(cp),dimension(:,:),allocatable :: vals
         real(cp) :: val
         integer,dimension(2) :: s
         logical,dimension(2) :: def = .false. ! true if (bctype,vals) are defined
         logical :: defined = .false. ! = all(defined)
       end type

       interface init;       module procedure init_type;             end interface
       interface init;       module procedure init_vals_RF;          end interface
       interface init;       module procedure init_val;              end interface
       interface init;       module procedure init_shape;            end interface
       interface init;       module procedure init_copy;             end interface

       interface delete;     module procedure delete_face;       end interface
       interface print;      module procedure print_face;        end interface
       interface export;     module procedure export_face;       end interface
       interface export;     module procedure export_face_unit;  end interface

       contains

       ! *******************************************************************************
       ! ********************************* INIT/DELETE *********************************
       ! *******************************************************************************

       subroutine init_type(f,bctype)
         implicit none
         type(face),intent(inout) :: f
         integer,intent(in) :: bctype
         f%bctype = bctype
         f%def(1) = .true.
         f%defined = all(f%def)
       end subroutine

       subroutine init_vals_RF(f,vals)
         implicit none
         type(face),intent(inout) :: f
         real(cp),dimension(:,:),intent(in) :: vals
         if (allocated(f%vals)) deallocate(f%vals)
         ! f%s = shape(vals) ! Is this necessary/good?
         ! Make sure that f%s has been defined here in debug mode
         allocate(f%vals(f%s(1),f%s(2)))
         f%val = vals(1,1)
         f%vals = vals
         f%def(2) = .true.
         f%defined = all(f%def)
       end subroutine

       subroutine init_shape(f,s)
         implicit none
         type(face),intent(inout) :: f
         integer,dimension(2),intent(in) :: s
         f%s = s
       end subroutine

       subroutine init_val(f,val)
         implicit none
         type(face),intent(inout) :: f
         real(cp),intent(in) :: val
         if (allocated(f%vals)) deallocate(f%vals)
         allocate(f%vals(f%s(1),f%s(2)))
         f%vals = val
         f%val = val
         f%def(2) = .true.
         f%defined = all(f%def)
       end subroutine

       subroutine init_copy(b_out,b_in)
         implicit none
         type(face),intent(inout) :: b_out
         type(face),intent(in) :: b_in
         if (.not.b_in%defined) stop 'Error: trying to copy BC that has not been fully defined'
         if (allocated(b_in%vals)) then
           call init(b_out,b_in%vals)
         else; stop 'Error: trying to copy BC that has not been allocated vals'
         endif
         b_out%bctype = b_in%bctype
         b_out%val = b_in%val
         b_out%def = b_out%def
         b_out%defined = b_out%defined
         b_out%s = b_in%s
       end subroutine

       subroutine delete_face(f)
         implicit none
         type(face),intent(inout) :: f
         if (allocated(f%vals)) deallocate(f%vals)
         f%s = 0
         f%def = .false.
         f%defined = all(f%def)
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_face(f,name)
         implicit none
         type(face), intent(in) :: f
         character(len=*),intent(in) :: name
         call exp_face(f,name,6)
       end subroutine

       subroutine export_face(f,dir,name)
         implicit none
         type(face), intent(in) :: f
         character(len=*),intent(in) :: dir,name
         integer :: NewU
         NewU = newAndOpen(dir,name//'_face')
         call exp_face(f,name,newU)
         call closeAndMessage(newU,name//'_faceConditions',dir)
       end subroutine

       subroutine export_face_unit(f,newU,name)
         implicit none
         type(face), intent(in) :: f
         integer,intent(in) :: newU
         character(len=*),intent(in) :: name
         call exp_face(f,name,newU)
       end subroutine

       subroutine exp_face(f,name,newU)
         implicit none
         type(face), intent(in) :: f
         character(len=*),intent(in) :: name
         integer,intent(in) :: NewU
         if (.not.f%defined) stop 'Error: face not defined in writeface in face.f90'

         write(newU,*) 'face conditions for ' // trim(adjustl(name))
         call writeface(f%bctype,newU)
       end subroutine

       subroutine writeface(bctype,NewU)
         implicit none
         integer,intent(in) :: NewU,bctype
         if (bctype.eq.1) then; write(newU,*) 'Dirichlet - direct - wall coincident'; endif
         if (bctype.eq.2) then; write(newU,*) 'Dirichlet - interpolated - wall incoincident'; endif
         if (bctype.eq.3) then; write(newU,*) 'Neumann - direct - wall coincident ~O(dh^2)'; endif
         if (bctype.eq.4) then; write(newU,*) 'Neumann - direct - wall coincident ~O(dh)'; endif
         if (bctype.eq.5) then; write(newU,*) 'Neumann - interpolated - wall incoincident O(dh)'; endif
         if (bctype.eq.6) then; write(newU,*) 'Periodic - direct - wall coincident ~O(dh)'; endif
         if (bctype.eq.7) then; write(newU,*) 'Periodic - interpolated - wall incoincident ~O(dh)'; endif
         if (bctype.eq.8) then; write(newU,*) 'Periodic - interpolated - wall incoincident ~O(dh^2)'; endif
       end subroutine

       end module