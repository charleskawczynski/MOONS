      module monitor_mod
      use mesh_mod
      use norms_mod
      use SF_mod
      use VF_mod
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
      public :: monitor
      public :: init,apply,delete
      ! public :: restart

      interface init;      module procedure init_monitor;         end interface
      interface apply;     module procedure apply_monitor;        end interface
      interface apply;     module procedure apply_monitor_norms;  end interface
      interface delete;    module procedure delete_monitor;       end interface
      ! interface restart;   module procedure restart_monitor;      end interface

      type monitor
        integer :: un
      end type

      contains

      subroutine init_monitor(mon,dir,name,norms)
        implicit none
        type(monitor),intent(inout) :: mon
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: norms
        mon%un = newAndOpen(dir,name)
        write(mon%un,*) 'TITLE = "'//name// '"'
        if (norms) then; write(mon%un,*) 'VARIABLES = nstep,L1,L2,Linf'
        else;            write(mon%un,*) 'VARIABLES = nstep,energy'
        endif
      end subroutine

      subroutine apply_monitor(mon,n,x)
        implicit none
        type(monitor),intent(inout) :: mon
        integer,intent(in) :: n
        real(cp),intent(in) :: x
        write(mon%un,*) n,x
      end subroutine

      subroutine apply_monitor_norms(mon,n,norm)
        implicit none
        type(monitor),intent(inout) :: mon
        integer,intent(in) :: n
        type(norms),intent(in) :: norm
        write(mon%un,*) n,norm%L1,norm%L2,norm%Linf
      end subroutine

      subroutine delete_monitor(mon)
        implicit none
        type(monitor),intent(inout) :: mon
        mon%un = 0
      end subroutine

      end module