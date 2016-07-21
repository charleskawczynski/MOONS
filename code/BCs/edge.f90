       module edge_mod
       ! Edges are organized as follows
       !        minmin(i)
       !        minmax(i)
       !        maxmin(i)
       !        maxmax(i)
       ! for direction i, covering all 12 edge.
       ! 
       ! To be more explicit:
       ! 
       ! x:  (i=1)   minmin(1):  ymin,zmin ! Right hand rule
       !             minmax(2):  ymin,zmax ! Right hand rule
       !             maxmin(3):  ymax,zmin ! Right hand rule
       !             maxmax(4):  ymax,zmax ! Right hand rule
       ! 
       ! y:  (i=2)   minmin(5):  xmin,zmin ! LEFT hand rule
       !             minmax(6):  xmin,zmax ! LEFT hand rule
       !             maxmin(7):  xmax,zmin ! LEFT hand rule
       !             maxmax(8):  xmax,zmax ! LEFT hand rule
       ! 
       ! z:  (i=3)   minmin(9):  xmin,ymin ! Right hand rule
       !             minmax(10): xmin,ymax ! Right hand rule
       !             maxmin(11): xmax,ymin ! Right hand rule
       !             maxmax(12): xmax,ymax ! Right hand rule
       ! 
       ! 
       use current_precision_mod
       use IO_tools_mod
       use bctype_mod
       implicit none

       private
       public :: edge
       public :: init,delete,display,print,export,import ! Essentials

       type edge
         type(bctype) :: b
         real(cp),dimension(:),allocatable :: vals
         integer :: s
         logical,dimension(2) :: def = .false. ! (size,vals)
         logical :: defined = .false. ! = all(e%def)
       end type

       interface init;       module procedure init_size;             end interface
       interface init;       module procedure init_vals_RF;          end interface
       interface init;       module procedure init_val;              end interface
       interface init;       module procedure init_copy;             end interface
       interface delete;     module procedure delete_edge;           end interface
       interface display;    module procedure display_edge;          end interface
       interface print;      module procedure print_edge;            end interface
       interface export;     module procedure export_edge;           end interface
       interface import;     module procedure import_edge;           end interface

       contains

       ! *******************************************************************************
       ! ********************************* INIT/DELETE *********************************
       ! *******************************************************************************

       subroutine init_size(e,s)
         implicit none
         type(edge),intent(inout) :: e
         integer,intent(in) :: s
         if (s.lt.1) stop 'Error: edge size input less than 1 in edge.f90'
         e%s = s
         if (allocated(e%vals)) deallocate(e%vals)
         allocate(e%vals(s))
         e%def(1) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_vals_RF(e,vals)
         implicit none
         type(edge),intent(inout) :: e
         real(cp),dimension(:),intent(in) :: vals
         integer :: s
         s = size(vals)
         if (s.lt.1) stop 'Error: edge size input less than 1 in edge.f90'
         if (s.ne.e%s) stop 'Error: shape mis-match in init_vals_RF in edge.f90'
         call init(e%b,vals)
         e%vals = vals
         e%def(2) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_val(e,val)
         implicit none
         type(edge),intent(inout) :: e
         real(cp),intent(in) :: val
         integer :: s
         if (.not.allocated(e%vals)) stop 'Error: edge not allocated in edge.f90'
         s = size(e%vals)
         if (s.lt.1) stop 'Error: edge size less than 1 in edge.f90'
         call init(e%b,val)
         e%vals = val
         e%def(2) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_copy(e_out,e_in)
         implicit none
         type(edge),intent(inout) :: e_out
         type(edge),intent(in) :: e_in
         if (.not.e_in%defined) stop 'Error: trying to copy undefined edge in edge.f90'
         call init(e_out%b,e_in%b)
         e_out%vals = e_in%vals
         e_out%def = e_in%def
         e_out%defined = e_in%defined
         e_out%s = e_in%s
       end subroutine

       subroutine delete_edge(e)
         implicit none
         type(edge),intent(inout) :: e
         if (allocated(e%vals)) deallocate(e%vals)
         call delete(e%b)
         e%s = 0
         e%def = .false.
         e%defined = .false.
       end subroutine

       subroutine display_edge(e,newU)
         implicit none
         type(edge), intent(in) :: e
         integer,intent(in) :: NewU
         if (.not.e%defined) stop 'Error: edge not defined in export_edge in edge.f90'
         call display(e%b,newU)
       end subroutine

       subroutine print_edge(e)
         implicit none
         type(edge), intent(in) :: e
         call display(e,6)
       end subroutine

       subroutine export_edge(e,un)
         implicit none
         type(edge),intent(in) :: e
         integer,intent(in) :: un
         if (.not.e%defined) stop 'Error: edge not defined in export_edge in edge.f90'
         call export(e%b,un)
         write(un,*) 's'
         write(un,*) e%s
         write(un,*) 'vals'
         write(un,*) e%vals
         write(un,*) 'def'
         write(un,*) e%def
         write(un,*) 'defined'
         write(un,*) e%defined
       end subroutine

       subroutine import_edge(e,un)
         implicit none
         type(edge),intent(inout) :: e
         integer,intent(in) :: un
         call delete(e)
         call import(e%b,un)
         read(un,*) 
         read(un,*) e%s
         allocate(e%vals(e%s))
         read(un,*) 
         read(un,*) e%vals
         read(un,*) 
         read(un,*) e%def
         read(un,*) 
         read(un,*) e%defined
       end subroutine

       end module