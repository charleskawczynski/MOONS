       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module bc_logicals_mod
       use IO_tools_mod
       implicit none

       private
       public :: bc_logicals
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_bc_logicals;           end interface
       interface delete; module procedure delete_bc_logicals;         end interface
       interface display;module procedure display_bc_logicals;        end interface
       interface display;module procedure display_wrapper_bc_logicals;end interface
       interface print;  module procedure print_bc_logicals;          end interface
       interface export; module procedure export_bc_logicals;         end interface
       interface import; module procedure import_bc_logicals;         end interface
       interface export; module procedure export_wrapper_bc_logicals; end interface
       interface import; module procedure import_wrapper_bc_logicals; end interface

       type bc_logicals
         logical :: defined = .false.
         logical :: gfs_defined = .false.
         logical :: bct_defined = .false.
         logical :: vals_defined = .false.
         logical :: all_dirichlet = .false.
         logical :: all_neumann = .false.
         logical :: all_robin = .false.
         logical :: all_symmetric = .false.
         logical :: all_antisymmetric = .false.
         logical :: any_dirichlet = .false.
         logical :: any_neumann = .false.
         logical :: any_robin = .false.
         logical :: any_symmetric = .false.
         logical :: any_antisymmetric = .false.
         logical :: any_prescribed = .false.
       end type

       contains

       subroutine init_bc_logicals(this,that)
         implicit none
         type(bc_logicals),intent(inout) :: this
         type(bc_logicals),intent(in) :: that
         call delete(this)
         this%defined = that%defined
         this%gfs_defined = that%gfs_defined
         this%bct_defined = that%bct_defined
         this%vals_defined = that%vals_defined
         this%all_dirichlet = that%all_dirichlet
         this%all_neumann = that%all_neumann
         this%all_robin = that%all_robin
         this%all_symmetric = that%all_symmetric
         this%all_antisymmetric = that%all_antisymmetric
         this%any_dirichlet = that%any_dirichlet
         this%any_neumann = that%any_neumann
         this%any_robin = that%any_robin
         this%any_symmetric = that%any_symmetric
         this%any_antisymmetric = that%any_antisymmetric
         this%any_prescribed = that%any_prescribed
       end subroutine

       subroutine delete_bc_logicals(this)
         implicit none
         type(bc_logicals),intent(inout) :: this
         this%defined = .false.
         this%gfs_defined = .false.
         this%bct_defined = .false.
         this%vals_defined = .false.
         this%all_dirichlet = .false.
         this%all_neumann = .false.
         this%all_robin = .false.
         this%all_symmetric = .false.
         this%all_antisymmetric = .false.
         this%any_dirichlet = .false.
         this%any_neumann = .false.
         this%any_robin = .false.
         this%any_symmetric = .false.
         this%any_antisymmetric = .false.
         this%any_prescribed = .false.
       end subroutine

       subroutine display_bc_logicals(this,un)
         implicit none
         type(bc_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- bc_logicals'
         write(un,*) 'defined           = ',this%defined
         write(un,*) 'gfs_defined       = ',this%gfs_defined
         write(un,*) 'bct_defined       = ',this%bct_defined
         write(un,*) 'vals_defined      = ',this%vals_defined
         write(un,*) 'all_dirichlet     = ',this%all_dirichlet
         write(un,*) 'all_neumann       = ',this%all_neumann
         write(un,*) 'all_robin         = ',this%all_robin
         write(un,*) 'all_symmetric     = ',this%all_symmetric
         write(un,*) 'all_antisymmetric = ',this%all_antisymmetric
         write(un,*) 'any_dirichlet     = ',this%any_dirichlet
         write(un,*) 'any_neumann       = ',this%any_neumann
         write(un,*) 'any_robin         = ',this%any_robin
         write(un,*) 'any_symmetric     = ',this%any_symmetric
         write(un,*) 'any_antisymmetric = ',this%any_antisymmetric
         write(un,*) 'any_prescribed    = ',this%any_prescribed
       end subroutine

       subroutine print_bc_logicals(this)
         implicit none
         type(bc_logicals),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_bc_logicals(this,un)
         implicit none
         type(bc_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined            = ';write(un,*) this%defined
         write(un,*) 'gfs_defined        = ';write(un,*) this%gfs_defined
         write(un,*) 'bct_defined        = ';write(un,*) this%bct_defined
         write(un,*) 'vals_defined       = ';write(un,*) this%vals_defined
         write(un,*) 'all_dirichlet      = ';write(un,*) this%all_dirichlet
         write(un,*) 'all_neumann        = ';write(un,*) this%all_neumann
         write(un,*) 'all_robin          = ';write(un,*) this%all_robin
         write(un,*) 'all_symmetric      = ';write(un,*) this%all_symmetric
         write(un,*) 'all_antisymmetric  = ';write(un,*) this%all_antisymmetric
         write(un,*) 'any_dirichlet      = ';write(un,*) this%any_dirichlet
         write(un,*) 'any_neumann        = ';write(un,*) this%any_neumann
         write(un,*) 'any_robin          = ';write(un,*) this%any_robin
         write(un,*) 'any_symmetric      = ';write(un,*) this%any_symmetric
         write(un,*) 'any_antisymmetric  = ';write(un,*) this%any_antisymmetric
         write(un,*) 'any_prescribed     = ';write(un,*) this%any_prescribed
       end subroutine

       subroutine import_bc_logicals(this,un)
         implicit none
         type(bc_logicals),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%gfs_defined
         read(un,*); read(un,*) this%bct_defined
         read(un,*); read(un,*) this%vals_defined
         read(un,*); read(un,*) this%all_dirichlet
         read(un,*); read(un,*) this%all_neumann
         read(un,*); read(un,*) this%all_robin
         read(un,*); read(un,*) this%all_symmetric
         read(un,*); read(un,*) this%all_antisymmetric
         read(un,*); read(un,*) this%any_dirichlet
         read(un,*); read(un,*) this%any_neumann
         read(un,*); read(un,*) this%any_robin
         read(un,*); read(un,*) this%any_symmetric
         read(un,*); read(un,*) this%any_antisymmetric
         read(un,*); read(un,*) this%any_prescribed
       end subroutine

       subroutine display_wrapper_bc_logicals(this,dir,name)
         implicit none
         type(bc_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_bc_logicals(this,dir,name)
         implicit none
         type(bc_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_bc_logicals(this,dir,name)
         implicit none
         type(bc_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module