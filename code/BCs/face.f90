       module face_mod
       use current_precision_mod
       use IO_tools_mod
       use bctype_mod
       implicit none

       private
       public :: face
       public :: init,delete
       public :: print,export

       type face
         type(bctype) :: b
         real(cp),dimension(:,:),allocatable :: vals
         integer,dimension(2) :: s
         logical,dimension(2) :: def = .false. ! (shape,vals)
         logical :: defined = .false. ! = all(def)
       end type

       interface init;       module procedure init_shape;            end interface
       interface init;       module procedure init_vals_RF;          end interface
       interface init;       module procedure init_val;              end interface
       interface init;       module procedure init_copy;             end interface

       interface delete;     module procedure delete_face;           end interface
       interface print;      module procedure print_face;            end interface
       interface export;     module procedure export_face;           end interface

       contains

       ! *******************************************************************************
       ! ********************************* INIT/DELETE *********************************
       ! *******************************************************************************

       subroutine init_shape(f,s)
         implicit none
         type(face),intent(inout) :: f
         integer,dimension(2),intent(in) :: s
         if ((s(1).lt.1).or.(s(2).lt.1)) stop 'Error: shape input in init_shape < 1 in face.f90'
         if (allocated(f%vals)) deallocate(f%vals)
         allocate(f%vals(s(1),s(2)))
         f%s = s
         f%def(1) = .true.
         f%defined = all(f%def)
       end subroutine

       subroutine init_vals_RF(f,vals)
         implicit none
         type(face),intent(inout) :: f
         real(cp),dimension(:,:),intent(in) :: vals
         integer,dimension(2) :: s
         s = shape(vals)
         if ((s(1).lt.1).or.(s(2).lt.1)) stop 'Error: shape input in init_vals_RF < 1 in face.f90'
         if ((s(1).ne.f%s(1)).or.(s(2).ne.f%s(2))) stop 'Error: shape mis-match in init_vals_RF in face.f90'
         call init(f%b,vals)
         f%vals = vals
         f%def(2) = .true.
         f%defined = all(f%def)
       end subroutine

       subroutine init_val(f,val)
         implicit none
         type(face),intent(inout) :: f
         real(cp),intent(in) :: val
         integer,dimension(2) :: s
         s = f%s
         if ((s(1).lt.1).or.(s(2).lt.1)) stop 'Error: shape in init_val < 1 in face.f90'
         call init(f%b,val)
         f%vals = val
         f%def(2) = .true.
         f%defined = all(f%def)
       end subroutine

       subroutine init_copy(b_out,b_in)
         implicit none
         type(face),intent(inout) :: b_out
         type(face),intent(in) :: b_in
         if (.not.b_in%defined) stop 'Error: trying to copy undefined face in face.f90'
         call init(b_out%b,b_in%b)
         b_out%vals = b_in%vals
         b_out%def = b_in%def
         b_out%defined = b_in%defined
         b_out%s = b_in%s
       end subroutine

       subroutine delete_face(f)
         implicit none
         type(face),intent(inout) :: f
         if (allocated(f%vals)) deallocate(f%vals)
         call delete(f%b)
         f%s = 0
         f%def = .false.
         f%defined = .false.
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_face(f)
         implicit none
         type(face), intent(in) :: f
         call export(f,6)
       end subroutine

       subroutine export_face(f,newU)
         implicit none
         type(face), intent(in) :: f
         integer,intent(in) :: NewU
         if (.not.f%defined) stop 'Error: face not defined in export_face in face.f90'
         call export(f%b,newU)
       end subroutine

       end module