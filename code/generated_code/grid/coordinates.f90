       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module coordinates_mod
       use current_precision_mod
       use IO_tools_mod
       use array_mod
       use sparse_mod
       implicit none

       private
       public :: coordinates
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_co;    end interface
       interface delete;       module procedure delete_co;       end interface
       interface display;      module procedure display_co;      end interface
       interface display_short;module procedure display_short_co;end interface
       interface display;      module procedure display_wrap_co; end interface
       interface print;        module procedure print_co;        end interface
       interface print_short;  module procedure print_short_co;  end interface
       interface export;       module procedure export_co;       end interface
       interface import;       module procedure import_co;       end interface
       interface export;       module procedure export_wrap_co;  end interface
       interface import;       module procedure import_wrap_co;  end interface

       type coordinates
         real(cp) :: hmin = 0.0_cp
         real(cp) :: hmax = 0.0_cp
         real(cp) :: amin = 0.0_cp
         real(cp) :: amax = 0.0_cp
         real(cp) :: maxRange = 0.0_cp
         real(cp) :: dhMin = 0.0_cp
         real(cp) :: dhMax = 0.0_cp
         real(cp) :: dhc_e = 0.0_cp
         real(cp) :: dhn_e = 0.0_cp
         real(cp) :: hc_e = 0.0_cp
         real(cp) :: hn_e = 0.0_cp
         integer :: sc = 0
         integer :: sn = 0
         integer :: N = 0
         logical :: defined = .false.
         integer :: i_midplane = 0
         logical :: stencils_defined = .false.
         logical,dimension(2) :: stencils_modified = .false.
         type(sparse) :: stagCC2N
         type(sparse) :: stagN2CC
         type(sparse) :: theta
         type(sparse),dimension(2) :: colCC
         type(sparse),dimension(2) :: colN
         type(sparse),dimension(2) :: colCC_centered
         type(array) :: hn
         type(array) :: hc
         type(array) :: dhn
         type(array) :: dhc
       end type

       contains

       subroutine init_copy_co(this,that)
         implicit none
         type(coordinates),intent(inout) :: this
         type(coordinates),intent(in) :: that
         integer :: i_colCC
         integer :: i_colN
         integer :: i_colCC_centered
         integer :: s_colCC
         integer :: s_colN
         integer :: s_colCC_centered
         call delete(this)
         this%hmin = that%hmin
         this%hmax = that%hmax
         this%amin = that%amin
         this%amax = that%amax
         this%maxRange = that%maxRange
         this%dhMin = that%dhMin
         this%dhMax = that%dhMax
         this%dhc_e = that%dhc_e
         this%dhn_e = that%dhn_e
         this%hc_e = that%hc_e
         this%hn_e = that%hn_e
         this%sc = that%sc
         this%sn = that%sn
         this%N = that%N
         this%defined = that%defined
         this%i_midplane = that%i_midplane
         this%stencils_defined = that%stencils_defined
         this%stencils_modified = that%stencils_modified
         call init(this%stagCC2N,that%stagCC2N)
         call init(this%stagN2CC,that%stagN2CC)
         call init(this%theta,that%theta)
         s_colCC = size(that%colCC)
         do i_colCC=1,s_colCC
           call init(this%colCC(i_colCC),that%colCC(i_colCC))
         enddo
         s_colN = size(that%colN)
         do i_colN=1,s_colN
           call init(this%colN(i_colN),that%colN(i_colN))
         enddo
         s_colCC_centered = size(that%colCC_centered)
         do i_colCC_centered=1,s_colCC_centered
           call init(this%colCC_centered(i_colCC_centered),that%colCC_centered(i_colCC_centered))
         enddo
         call init(this%hn,that%hn)
         call init(this%hc,that%hc)
         call init(this%dhn,that%dhn)
         call init(this%dhc,that%dhc)
       end subroutine

       subroutine delete_co(this)
         implicit none
         type(coordinates),intent(inout) :: this
         integer :: i_colCC
         integer :: i_colN
         integer :: i_colCC_centered
         integer :: s_colCC
         integer :: s_colN
         integer :: s_colCC_centered
         this%hmin = 0.0_cp
         this%hmax = 0.0_cp
         this%amin = 0.0_cp
         this%amax = 0.0_cp
         this%maxRange = 0.0_cp
         this%dhMin = 0.0_cp
         this%dhMax = 0.0_cp
         this%dhc_e = 0.0_cp
         this%dhn_e = 0.0_cp
         this%hc_e = 0.0_cp
         this%hn_e = 0.0_cp
         this%sc = 0
         this%sn = 0
         this%N = 0
         this%defined = .false.
         this%i_midplane = 0
         this%stencils_defined = .false.
         this%stencils_modified = .false.
         call delete(this%stagCC2N)
         call delete(this%stagN2CC)
         call delete(this%theta)
         s_colCC = size(this%colCC)
         do i_colCC=1,s_colCC
           call delete(this%colCC(i_colCC))
         enddo
         s_colN = size(this%colN)
         do i_colN=1,s_colN
           call delete(this%colN(i_colN))
         enddo
         s_colCC_centered = size(this%colCC_centered)
         do i_colCC_centered=1,s_colCC_centered
           call delete(this%colCC_centered(i_colCC_centered))
         enddo
         call delete(this%hn)
         call delete(this%hc)
         call delete(this%dhn)
         call delete(this%dhc)
       end subroutine

       subroutine display_co(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_colCC
         integer :: i_colN
         integer :: i_colCC_centered
         integer :: s_colCC
         integer :: s_colN
         integer :: s_colCC_centered
         write(un,*) ' -------------------- coordinates'
         write(un,*) 'hmin              = ',this%hmin
         write(un,*) 'hmax              = ',this%hmax
         write(un,*) 'amin              = ',this%amin
         write(un,*) 'amax              = ',this%amax
         write(un,*) 'maxRange          = ',this%maxRange
         write(un,*) 'dhMin             = ',this%dhMin
         write(un,*) 'dhMax             = ',this%dhMax
         write(un,*) 'dhc_e             = ',this%dhc_e
         write(un,*) 'dhn_e             = ',this%dhn_e
         write(un,*) 'hc_e              = ',this%hc_e
         write(un,*) 'hn_e              = ',this%hn_e
         write(un,*) 'sc                = ',this%sc
         write(un,*) 'sn                = ',this%sn
         write(un,*) 'N                 = ',this%N
         write(un,*) 'defined           = ',this%defined
         write(un,*) 'i_midplane        = ',this%i_midplane
         write(un,*) 'stencils_defined  = ',this%stencils_defined
         write(un,*) 'stencils_modified = ',this%stencils_modified
         call display(this%stagCC2N,un)
         call display(this%stagN2CC,un)
         call display(this%theta,un)
         s_colCC = size(this%colCC)
         do i_colCC=1,s_colCC
           call display(this%colCC(i_colCC),un)
         enddo
         s_colN = size(this%colN)
         do i_colN=1,s_colN
           call display(this%colN(i_colN),un)
         enddo
         s_colCC_centered = size(this%colCC_centered)
         do i_colCC_centered=1,s_colCC_centered
           call display(this%colCC_centered(i_colCC_centered),un)
         enddo
         call display(this%hn,un)
         call display(this%hc,un)
         call display(this%dhn,un)
         call display(this%dhc,un)
       end subroutine

       subroutine display_short_co(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_colCC
         integer :: i_colN
         integer :: i_colCC_centered
         integer :: s_colCC
         integer :: s_colN
         integer :: s_colCC_centered
         write(un,*) 'hmin              = ',this%hmin
         write(un,*) 'hmax              = ',this%hmax
         write(un,*) 'amin              = ',this%amin
         write(un,*) 'amax              = ',this%amax
         write(un,*) 'maxRange          = ',this%maxRange
         write(un,*) 'dhMin             = ',this%dhMin
         write(un,*) 'dhMax             = ',this%dhMax
         write(un,*) 'dhc_e             = ',this%dhc_e
         write(un,*) 'dhn_e             = ',this%dhn_e
         write(un,*) 'hc_e              = ',this%hc_e
         write(un,*) 'hn_e              = ',this%hn_e
         write(un,*) 'sc                = ',this%sc
         write(un,*) 'sn                = ',this%sn
         write(un,*) 'N                 = ',this%N
         write(un,*) 'defined           = ',this%defined
         write(un,*) 'i_midplane        = ',this%i_midplane
         write(un,*) 'stencils_defined  = ',this%stencils_defined
         write(un,*) 'stencils_modified = ',this%stencils_modified
         call display(this%stagCC2N,un)
         call display(this%stagN2CC,un)
         call display(this%theta,un)
         s_colCC = size(this%colCC)
         do i_colCC=1,s_colCC
           call display(this%colCC(i_colCC),un)
         enddo
         s_colN = size(this%colN)
         do i_colN=1,s_colN
           call display(this%colN(i_colN),un)
         enddo
         s_colCC_centered = size(this%colCC_centered)
         do i_colCC_centered=1,s_colCC_centered
           call display(this%colCC_centered(i_colCC_centered),un)
         enddo
         call display(this%hn,un)
         call display(this%hc,un)
         call display(this%dhn,un)
         call display(this%dhc,un)
       end subroutine

       subroutine print_co(this)
         implicit none
         type(coordinates),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_co(this)
         implicit none
         type(coordinates),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_co(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_colCC
         integer :: i_colN
         integer :: i_colCC_centered
         integer :: s_colCC
         integer :: s_colN
         integer :: s_colCC_centered
         write(un,*) 'hmin               = ';write(un,*) this%hmin
         write(un,*) 'hmax               = ';write(un,*) this%hmax
         write(un,*) 'amin               = ';write(un,*) this%amin
         write(un,*) 'amax               = ';write(un,*) this%amax
         write(un,*) 'maxRange           = ';write(un,*) this%maxRange
         write(un,*) 'dhMin              = ';write(un,*) this%dhMin
         write(un,*) 'dhMax              = ';write(un,*) this%dhMax
         write(un,*) 'dhc_e              = ';write(un,*) this%dhc_e
         write(un,*) 'dhn_e              = ';write(un,*) this%dhn_e
         write(un,*) 'hc_e               = ';write(un,*) this%hc_e
         write(un,*) 'hn_e               = ';write(un,*) this%hn_e
         write(un,*) 'sc                 = ';write(un,*) this%sc
         write(un,*) 'sn                 = ';write(un,*) this%sn
         write(un,*) 'N                  = ';write(un,*) this%N
         write(un,*) 'defined            = ';write(un,*) this%defined
         write(un,*) 'i_midplane         = ';write(un,*) this%i_midplane
         write(un,*) 'stencils_defined   = ';write(un,*) this%stencils_defined
         write(un,*) 'stencils_modified  = ';write(un,*) this%stencils_modified
         call export(this%stagCC2N,un)
         call export(this%stagN2CC,un)
         call export(this%theta,un)
         s_colCC = size(this%colCC)
         write(un,*) s_colCC
         do i_colCC=1,s_colCC
           call export(this%colCC(i_colCC),un)
         enddo
         s_colN = size(this%colN)
         write(un,*) s_colN
         do i_colN=1,s_colN
           call export(this%colN(i_colN),un)
         enddo
         s_colCC_centered = size(this%colCC_centered)
         write(un,*) s_colCC_centered
         do i_colCC_centered=1,s_colCC_centered
           call export(this%colCC_centered(i_colCC_centered),un)
         enddo
         call export(this%hn,un)
         call export(this%hc,un)
         call export(this%dhn,un)
         call export(this%dhc,un)
       end subroutine

       subroutine import_co(this,un)
         implicit none
         type(coordinates),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_colCC
         integer :: i_colN
         integer :: i_colCC_centered
         integer :: s_colCC
         integer :: s_colN
         integer :: s_colCC_centered
         call delete(this)
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%amin
         read(un,*); read(un,*) this%amax
         read(un,*); read(un,*) this%maxRange
         read(un,*); read(un,*) this%dhMin
         read(un,*); read(un,*) this%dhMax
         read(un,*); read(un,*) this%dhc_e
         read(un,*); read(un,*) this%dhn_e
         read(un,*); read(un,*) this%hc_e
         read(un,*); read(un,*) this%hn_e
         read(un,*); read(un,*) this%sc
         read(un,*); read(un,*) this%sn
         read(un,*); read(un,*) this%N
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%i_midplane
         read(un,*); read(un,*) this%stencils_defined
         read(un,*); read(un,*) this%stencils_modified
         call import(this%stagCC2N,un)
         call import(this%stagN2CC,un)
         call import(this%theta,un)
         read(un,*) s_colCC
         do i_colCC=1,s_colCC
           call import(this%colCC(i_colCC),un)
         enddo
         read(un,*) s_colN
         do i_colN=1,s_colN
           call import(this%colN(i_colN),un)
         enddo
         read(un,*) s_colCC_centered
         do i_colCC_centered=1,s_colCC_centered
           call import(this%colCC_centered(i_colCC_centered),un)
         enddo
         call import(this%hn,un)
         call import(this%hc,un)
         call import(this%dhn,un)
         call import(this%dhc,un)
       end subroutine

       subroutine display_wrap_co(this,dir,name)
         implicit none
         type(coordinates),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_co(this,dir,name)
         implicit none
         type(coordinates),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_co(this,dir,name)
         implicit none
         type(coordinates),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module