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

       interface init;         module procedure init_copy_coordinates;      end interface
       interface delete;       module procedure delete_coordinates;         end interface
       interface display;      module procedure display_coordinates;        end interface
       interface display_short;module procedure display_short_coordinates;  end interface
       interface display;      module procedure display_wrapper_coordinates;end interface
       interface print;        module procedure print_coordinates;          end interface
       interface print_short;  module procedure print_short_coordinates;    end interface
       interface export;       module procedure export_coordinates;         end interface
       interface import;       module procedure import_coordinates;         end interface
       interface export;       module procedure export_wrapper_coordinates; end interface
       interface import;       module procedure import_wrapper_coordinates; end interface

       type coordinates
         real(cp) :: hmin = 0.0_cp
         real(cp) :: hmax = 0.0_cp
         real(cp) :: amin = 0.0_cp
         real(cp) :: amax = 0.0_cp
         real(cp) :: maxrange = 0.0_cp
         real(cp) :: dhmin = 0.0_cp
         real(cp) :: dhmax = 0.0_cp
         real(cp) :: dhc_e = 0.0_cp
         real(cp) :: dhn_e = 0.0_cp
         real(cp) :: hc_e = 0.0_cp
         real(cp) :: hn_e = 0.0_cp
         integer :: sc = 0
         integer :: sn = 0
         integer :: n = 0
         logical :: defined = .false.
         integer :: i_midplane = 0
         logical :: stencils_defined = .false.
         logical,dimension(2) :: stencils_modified = .false.
         type(sparse) :: stagcc2n
         type(sparse) :: stagn2cc
         type(sparse) :: theta
         type(sparse),dimension(2) :: colcc
         type(sparse),dimension(2) :: coln
         type(sparse),dimension(2) :: colcc_centered
         type(array) :: hn
         type(array) :: hc
         type(array) :: dhn
         type(array) :: dhc
       end type

       contains

       subroutine init_copy_coordinates(this,that)
         implicit none
         type(coordinates),intent(inout) :: this
         type(coordinates),intent(in) :: that
         integer :: i_colcc
         integer :: i_coln
         integer :: i_colcc_centered
         integer :: s_colcc
         integer :: s_coln
         integer :: s_colcc_centered
         call delete(this)
         this%hmin = that%hmin
         this%hmax = that%hmax
         this%amin = that%amin
         this%amax = that%amax
         this%maxrange = that%maxrange
         this%dhmin = that%dhmin
         this%dhmax = that%dhmax
         this%dhc_e = that%dhc_e
         this%dhn_e = that%dhn_e
         this%hc_e = that%hc_e
         this%hn_e = that%hn_e
         this%sc = that%sc
         this%sn = that%sn
         this%n = that%n
         this%defined = that%defined
         this%i_midplane = that%i_midplane
         this%stencils_defined = that%stencils_defined
         this%stencils_modified = that%stencils_modified
         call init(this%stagcc2n,that%stagcc2n)
         call init(this%stagn2cc,that%stagn2cc)
         call init(this%theta,that%theta)
         s_colcc = size(that%colcc)
         do i_colcc=1,s_colcc
           call init(this%colcc(i_colcc),that%colcc(i_colcc))
         enddo
         s_coln = size(that%coln)
         do i_coln=1,s_coln
           call init(this%coln(i_coln),that%coln(i_coln))
         enddo
         s_colcc_centered = size(that%colcc_centered)
         do i_colcc_centered=1,s_colcc_centered
           call init(this%colcc_centered(i_colcc_centered),that%colcc_centered(i_colcc_centered))
         enddo
         call init(this%hn,that%hn)
         call init(this%hc,that%hc)
         call init(this%dhn,that%dhn)
         call init(this%dhc,that%dhc)
       end subroutine

       subroutine delete_coordinates(this)
         implicit none
         type(coordinates),intent(inout) :: this
         integer :: i_colcc
         integer :: i_coln
         integer :: i_colcc_centered
         integer :: s_colcc
         integer :: s_coln
         integer :: s_colcc_centered
         this%hmin = 0.0_cp
         this%hmax = 0.0_cp
         this%amin = 0.0_cp
         this%amax = 0.0_cp
         this%maxrange = 0.0_cp
         this%dhmin = 0.0_cp
         this%dhmax = 0.0_cp
         this%dhc_e = 0.0_cp
         this%dhn_e = 0.0_cp
         this%hc_e = 0.0_cp
         this%hn_e = 0.0_cp
         this%sc = 0
         this%sn = 0
         this%n = 0
         this%defined = .false.
         this%i_midplane = 0
         this%stencils_defined = .false.
         this%stencils_modified = .false.
         call delete(this%stagcc2n)
         call delete(this%stagn2cc)
         call delete(this%theta)
         s_colcc = size(this%colcc)
         do i_colcc=1,s_colcc
           call delete(this%colcc(i_colcc))
         enddo
         s_coln = size(this%coln)
         do i_coln=1,s_coln
           call delete(this%coln(i_coln))
         enddo
         s_colcc_centered = size(this%colcc_centered)
         do i_colcc_centered=1,s_colcc_centered
           call delete(this%colcc_centered(i_colcc_centered))
         enddo
         call delete(this%hn)
         call delete(this%hc)
         call delete(this%dhn)
         call delete(this%dhc)
       end subroutine

       subroutine display_coordinates(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_colcc
         integer :: i_coln
         integer :: i_colcc_centered
         integer :: s_colcc
         integer :: s_coln
         integer :: s_colcc_centered
         write(un,*) ' -------------------- coordinates'
         write(un,*) 'hmin              = ',this%hmin
         write(un,*) 'hmax              = ',this%hmax
         write(un,*) 'amin              = ',this%amin
         write(un,*) 'amax              = ',this%amax
         write(un,*) 'maxrange          = ',this%maxrange
         write(un,*) 'dhmin             = ',this%dhmin
         write(un,*) 'dhmax             = ',this%dhmax
         write(un,*) 'dhc_e             = ',this%dhc_e
         write(un,*) 'dhn_e             = ',this%dhn_e
         write(un,*) 'hc_e              = ',this%hc_e
         write(un,*) 'hn_e              = ',this%hn_e
         write(un,*) 'sc                = ',this%sc
         write(un,*) 'sn                = ',this%sn
         write(un,*) 'n                 = ',this%n
         write(un,*) 'defined           = ',this%defined
         write(un,*) 'i_midplane        = ',this%i_midplane
         write(un,*) 'stencils_defined  = ',this%stencils_defined
         write(un,*) 'stencils_modified = ',this%stencils_modified
         call display(this%stagcc2n,un)
         call display(this%stagn2cc,un)
         call display(this%theta,un)
         s_colcc = size(this%colcc)
         do i_colcc=1,s_colcc
           call display(this%colcc(i_colcc),un)
         enddo
         s_coln = size(this%coln)
         do i_coln=1,s_coln
           call display(this%coln(i_coln),un)
         enddo
         s_colcc_centered = size(this%colcc_centered)
         do i_colcc_centered=1,s_colcc_centered
           call display(this%colcc_centered(i_colcc_centered),un)
         enddo
         call display(this%hn,un)
         call display(this%hc,un)
         call display(this%dhn,un)
         call display(this%dhc,un)
       end subroutine

       subroutine display_short_coordinates(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_colcc
         integer :: i_coln
         integer :: i_colcc_centered
         integer :: s_colcc
         integer :: s_coln
         integer :: s_colcc_centered
         write(un,*) 'hmin              = ',this%hmin
         write(un,*) 'hmax              = ',this%hmax
         write(un,*) 'amin              = ',this%amin
         write(un,*) 'amax              = ',this%amax
         write(un,*) 'maxrange          = ',this%maxrange
         write(un,*) 'dhmin             = ',this%dhmin
         write(un,*) 'dhmax             = ',this%dhmax
         write(un,*) 'dhc_e             = ',this%dhc_e
         write(un,*) 'dhn_e             = ',this%dhn_e
         write(un,*) 'hc_e              = ',this%hc_e
         write(un,*) 'hn_e              = ',this%hn_e
         write(un,*) 'sc                = ',this%sc
         write(un,*) 'sn                = ',this%sn
         write(un,*) 'n                 = ',this%n
         write(un,*) 'defined           = ',this%defined
         write(un,*) 'i_midplane        = ',this%i_midplane
         write(un,*) 'stencils_defined  = ',this%stencils_defined
         write(un,*) 'stencils_modified = ',this%stencils_modified
         call display(this%stagcc2n,un)
         call display(this%stagn2cc,un)
         call display(this%theta,un)
         s_colcc = size(this%colcc)
         do i_colcc=1,s_colcc
           call display(this%colcc(i_colcc),un)
         enddo
         s_coln = size(this%coln)
         do i_coln=1,s_coln
           call display(this%coln(i_coln),un)
         enddo
         s_colcc_centered = size(this%colcc_centered)
         do i_colcc_centered=1,s_colcc_centered
           call display(this%colcc_centered(i_colcc_centered),un)
         enddo
         call display(this%hn,un)
         call display(this%hc,un)
         call display(this%dhn,un)
         call display(this%dhc,un)
       end subroutine

       subroutine print_coordinates(this)
         implicit none
         type(coordinates),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_coordinates(this)
         implicit none
         type(coordinates),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_coordinates(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_colcc
         integer :: i_coln
         integer :: i_colcc_centered
         integer :: s_colcc
         integer :: s_coln
         integer :: s_colcc_centered
         write(un,*) 'hmin               = ';write(un,*) this%hmin
         write(un,*) 'hmax               = ';write(un,*) this%hmax
         write(un,*) 'amin               = ';write(un,*) this%amin
         write(un,*) 'amax               = ';write(un,*) this%amax
         write(un,*) 'maxrange           = ';write(un,*) this%maxrange
         write(un,*) 'dhmin              = ';write(un,*) this%dhmin
         write(un,*) 'dhmax              = ';write(un,*) this%dhmax
         write(un,*) 'dhc_e              = ';write(un,*) this%dhc_e
         write(un,*) 'dhn_e              = ';write(un,*) this%dhn_e
         write(un,*) 'hc_e               = ';write(un,*) this%hc_e
         write(un,*) 'hn_e               = ';write(un,*) this%hn_e
         write(un,*) 'sc                 = ';write(un,*) this%sc
         write(un,*) 'sn                 = ';write(un,*) this%sn
         write(un,*) 'n                  = ';write(un,*) this%n
         write(un,*) 'defined            = ';write(un,*) this%defined
         write(un,*) 'i_midplane         = ';write(un,*) this%i_midplane
         write(un,*) 'stencils_defined   = ';write(un,*) this%stencils_defined
         write(un,*) 'stencils_modified  = ';write(un,*) this%stencils_modified
         call export(this%stagcc2n,un)
         call export(this%stagn2cc,un)
         call export(this%theta,un)
         s_colcc = size(this%colcc)
         write(un,*) s_colcc
         do i_colcc=1,s_colcc
           call export(this%colcc(i_colcc),un)
         enddo
         s_coln = size(this%coln)
         write(un,*) s_coln
         do i_coln=1,s_coln
           call export(this%coln(i_coln),un)
         enddo
         s_colcc_centered = size(this%colcc_centered)
         write(un,*) s_colcc_centered
         do i_colcc_centered=1,s_colcc_centered
           call export(this%colcc_centered(i_colcc_centered),un)
         enddo
         call export(this%hn,un)
         call export(this%hc,un)
         call export(this%dhn,un)
         call export(this%dhc,un)
       end subroutine

       subroutine import_coordinates(this,un)
         implicit none
         type(coordinates),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_colcc
         integer :: i_coln
         integer :: i_colcc_centered
         integer :: s_colcc
         integer :: s_coln
         integer :: s_colcc_centered
         call delete(this)
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%amin
         read(un,*); read(un,*) this%amax
         read(un,*); read(un,*) this%maxrange
         read(un,*); read(un,*) this%dhmin
         read(un,*); read(un,*) this%dhmax
         read(un,*); read(un,*) this%dhc_e
         read(un,*); read(un,*) this%dhn_e
         read(un,*); read(un,*) this%hc_e
         read(un,*); read(un,*) this%hn_e
         read(un,*); read(un,*) this%sc
         read(un,*); read(un,*) this%sn
         read(un,*); read(un,*) this%n
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%i_midplane
         read(un,*); read(un,*) this%stencils_defined
         read(un,*); read(un,*) this%stencils_modified
         call import(this%stagcc2n,un)
         call import(this%stagn2cc,un)
         call import(this%theta,un)
         read(un,*) s_colcc
         do i_colcc=1,s_colcc
           call import(this%colcc(i_colcc),un)
         enddo
         read(un,*) s_coln
         do i_coln=1,s_coln
           call import(this%coln(i_coln),un)
         enddo
         read(un,*) s_colcc_centered
         do i_colcc_centered=1,s_colcc_centered
           call import(this%colcc_centered(i_colcc_centered),un)
         enddo
         call import(this%hn,un)
         call import(this%hc,un)
         call import(this%dhn,un)
         call import(this%dhc,un)
       end subroutine

       subroutine display_wrapper_coordinates(this,dir,name)
         implicit none
         type(coordinates),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_coordinates(this,dir,name)
         implicit none
         type(coordinates),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_coordinates(this,dir,name)
         implicit none
         type(coordinates),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module