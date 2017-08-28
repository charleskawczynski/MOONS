       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module bctype_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: bctype
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_bctype;           end interface
       interface delete; module procedure delete_bctype;         end interface
       interface display;module procedure display_bctype;        end interface
       interface display;module procedure display_wrapper_bctype;end interface
       interface print;  module procedure print_bctype;          end interface
       interface export; module procedure export_bctype;         end interface
       interface import; module procedure import_bctype;         end interface
       interface export; module procedure export_wrapper_bctype; end interface
       interface import; module procedure import_wrapper_bctype; end interface

       type bctype
         logical :: dirichlet = .false.
         logical :: neumann = .false.
         logical :: robin = .false.
         logical :: periodic = .false.
         logical :: symmetric = .false.
         logical :: antisymmetric = .false.
         logical :: prescribed = .false.
         logical :: defined = .false.
         real(cp) :: meanval = 0.0_cp
         character(len=1) :: bct = ' '
       end type

       contains

       subroutine init_bctype(this,that)
         implicit none
         type(bctype),intent(inout) :: this
         type(bctype),intent(in) :: that
         call delete(this)
         this%dirichlet = that%dirichlet
         this%neumann = that%neumann
         this%robin = that%robin
         this%periodic = that%periodic
         this%symmetric = that%symmetric
         this%antisymmetric = that%antisymmetric
         this%prescribed = that%prescribed
         this%defined = that%defined
         this%meanval = that%meanval
         this%bct = that%bct
       end subroutine

       subroutine delete_bctype(this)
         implicit none
         type(bctype),intent(inout) :: this
         this%dirichlet = .false.
         this%neumann = .false.
         this%robin = .false.
         this%periodic = .false.
         this%symmetric = .false.
         this%antisymmetric = .false.
         this%prescribed = .false.
         this%defined = .false.
         this%meanval = 0.0_cp
         this%bct = ' '
       end subroutine

       subroutine display_bctype(this,un)
         implicit none
         type(bctype),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- bctype'
         write(un,*) 'dirichlet     = ',this%dirichlet
         write(un,*) 'neumann       = ',this%neumann
         write(un,*) 'robin         = ',this%robin
         write(un,*) 'periodic      = ',this%periodic
         write(un,*) 'symmetric     = ',this%symmetric
         write(un,*) 'antisymmetric = ',this%antisymmetric
         write(un,*) 'prescribed    = ',this%prescribed
         write(un,*) 'defined       = ',this%defined
         write(un,*) 'meanval       = ',this%meanval
         write(un,*) 'bct           = ',this%bct
       end subroutine

       subroutine print_bctype(this)
         implicit none
         type(bctype),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_bctype(this,un)
         implicit none
         type(bctype),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'dirichlet      = ';write(un,*) this%dirichlet
         write(un,*) 'neumann        = ';write(un,*) this%neumann
         write(un,*) 'robin          = ';write(un,*) this%robin
         write(un,*) 'periodic       = ';write(un,*) this%periodic
         write(un,*) 'symmetric      = ';write(un,*) this%symmetric
         write(un,*) 'antisymmetric  = ';write(un,*) this%antisymmetric
         write(un,*) 'prescribed     = ';write(un,*) this%prescribed
         write(un,*) 'defined        = ';write(un,*) this%defined
         write(un,*) 'meanval        = ';write(un,*) this%meanval
         write(un,*) 'bct            = ';write(un,*) this%bct
       end subroutine

       subroutine import_bctype(this,un)
         implicit none
         type(bctype),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%dirichlet
         read(un,*); read(un,*) this%neumann
         read(un,*); read(un,*) this%robin
         read(un,*); read(un,*) this%periodic
         read(un,*); read(un,*) this%symmetric
         read(un,*); read(un,*) this%antisymmetric
         read(un,*); read(un,*) this%prescribed
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%meanval
         read(un,*); read(un,*) this%bct
       end subroutine

       subroutine display_wrapper_bctype(this,dir,name)
         implicit none
         type(bctype),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_bctype(this,dir,name)
         implicit none
         type(bctype),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_bctype(this,dir,name)
         implicit none
         type(bctype),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module