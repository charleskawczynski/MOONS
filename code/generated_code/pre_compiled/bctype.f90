       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module bctype_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: bctype
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_bctype;        end interface
       interface delete;           module procedure delete_bctype;           end interface
       interface display;          module procedure display_bctype;          end interface
       interface display_short;    module procedure display_short_bctype;    end interface
       interface display;          module procedure display_wrap_bctype;     end interface
       interface print;            module procedure print_bctype;            end interface
       interface print_short;      module procedure print_short_bctype;      end interface
       interface export;           module procedure export_bctype;           end interface
       interface export_primitives;module procedure export_primitives_bctype;end interface
       interface export_restart;   module procedure export_restart_bctype;   end interface
       interface import;           module procedure import_bctype;           end interface
       interface import_restart;   module procedure import_restart_bctype;   end interface
       interface import_primitives;module procedure import_primitives_bctype;end interface
       interface export;           module procedure export_wrap_bctype;      end interface
       interface import;           module procedure import_wrap_bctype;      end interface
       interface make_restart_dir; module procedure make_restart_dir_bctype; end interface
       interface suppress_warnings;module procedure suppress_warnings_bctype;end interface

       type bctype
         logical :: Dirichlet = .false.
         logical :: Neumann = .false.
         logical :: Robin = .false.
         logical :: Periodic = .false.
         logical :: symmetric = .false.
         logical :: antisymmetric = .false.
         logical :: prescribed = .false.
         logical :: defined = .false.
         real(cp) :: meanVal = 0.0_cp
         character(len=1) :: BCT = ' '
       end type

       contains

       subroutine init_copy_bctype(this,that)
         implicit none
         type(bctype),intent(inout) :: this
         type(bctype),intent(in) :: that
         call delete(this)
         this%Dirichlet = that%Dirichlet
         this%Neumann = that%Neumann
         this%Robin = that%Robin
         this%Periodic = that%Periodic
         this%symmetric = that%symmetric
         this%antisymmetric = that%antisymmetric
         this%prescribed = that%prescribed
         this%defined = that%defined
         this%meanVal = that%meanVal
         this%BCT = that%BCT
       end subroutine

       subroutine delete_bctype(this)
         implicit none
         type(bctype),intent(inout) :: this
         this%Dirichlet = .false.
         this%Neumann = .false.
         this%Robin = .false.
         this%Periodic = .false.
         this%symmetric = .false.
         this%antisymmetric = .false.
         this%prescribed = .false.
         this%defined = .false.
         this%meanVal = 0.0_cp
         this%BCT = ' '
       end subroutine

       subroutine display_bctype(this,un)
         implicit none
         type(bctype),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'Dirichlet     = ',this%Dirichlet
         write(un,*) 'Neumann       = ',this%Neumann
         write(un,*) 'Robin         = ',this%Robin
         write(un,*) 'Periodic      = ',this%Periodic
         write(un,*) 'symmetric     = ',this%symmetric
         write(un,*) 'antisymmetric = ',this%antisymmetric
         write(un,*) 'prescribed    = ',this%prescribed
         write(un,*) 'defined       = ',this%defined
         write(un,*) 'meanVal       = ',this%meanVal
         write(un,*) 'BCT           = ',this%BCT
       end subroutine

       subroutine display_short_bctype(this,un)
         implicit none
         type(bctype),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'Dirichlet     = ',this%Dirichlet
         write(un,*) 'Neumann       = ',this%Neumann
         write(un,*) 'Robin         = ',this%Robin
         write(un,*) 'Periodic      = ',this%Periodic
         write(un,*) 'symmetric     = ',this%symmetric
         write(un,*) 'antisymmetric = ',this%antisymmetric
         write(un,*) 'prescribed    = ',this%prescribed
         write(un,*) 'defined       = ',this%defined
         write(un,*) 'meanVal       = ',this%meanVal
         write(un,*) 'BCT           = ',this%BCT
       end subroutine

       subroutine display_wrap_bctype(this,dir,name)
         implicit none
         type(bctype),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_bctype(this)
         implicit none
         type(bctype),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_bctype(this)
         implicit none
         type(bctype),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_bctype(this,un)
         implicit none
         type(bctype),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'Dirichlet      = ';write(un,*) this%Dirichlet
         write(un,*) 'Neumann        = ';write(un,*) this%Neumann
         write(un,*) 'Robin          = ';write(un,*) this%Robin
         write(un,*) 'Periodic       = ';write(un,*) this%Periodic
         write(un,*) 'symmetric      = ';write(un,*) this%symmetric
         write(un,*) 'antisymmetric  = ';write(un,*) this%antisymmetric
         write(un,*) 'prescribed     = ';write(un,*) this%prescribed
         write(un,*) 'defined        = ';write(un,*) this%defined
         write(un,*) 'meanVal        = ';write(un,*) this%meanVal
         write(un,*) 'BCT            = ';write(un,*) this%BCT
       end subroutine

       subroutine export_bctype(this,un)
         implicit none
         type(bctype),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'Dirichlet      = ';write(un,*) this%Dirichlet
         write(un,*) 'Neumann        = ';write(un,*) this%Neumann
         write(un,*) 'Robin          = ';write(un,*) this%Robin
         write(un,*) 'Periodic       = ';write(un,*) this%Periodic
         write(un,*) 'symmetric      = ';write(un,*) this%symmetric
         write(un,*) 'antisymmetric  = ';write(un,*) this%antisymmetric
         write(un,*) 'prescribed     = ';write(un,*) this%prescribed
         write(un,*) 'defined        = ';write(un,*) this%defined
         write(un,*) 'meanVal        = ';write(un,*) this%meanVal
         write(un,*) 'BCT            = ';write(un,*) this%BCT
       end subroutine

       subroutine import_primitives_bctype(this,un)
         implicit none
         type(bctype),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%Dirichlet
         read(un,*); read(un,*) this%Neumann
         read(un,*); read(un,*) this%Robin
         read(un,*); read(un,*) this%Periodic
         read(un,*); read(un,*) this%symmetric
         read(un,*); read(un,*) this%antisymmetric
         read(un,*); read(un,*) this%prescribed
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%meanVal
         read(un,*); read(un,*) this%BCT
       end subroutine

       subroutine import_bctype(this,un)
         implicit none
         type(bctype),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%Dirichlet
         read(un,*); read(un,*) this%Neumann
         read(un,*); read(un,*) this%Robin
         read(un,*); read(un,*) this%Periodic
         read(un,*); read(un,*) this%symmetric
         read(un,*); read(un,*) this%antisymmetric
         read(un,*); read(un,*) this%prescribed
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%meanVal
         read(un,*); read(un,*) this%BCT
       end subroutine

       subroutine export_wrap_bctype(this,dir,name)
         implicit none
         type(bctype),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_bctype(this,dir,name)
         implicit none
         type(bctype),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_bctype(this,dir)
         implicit none
         type(bctype),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_restart_bctype(this,dir)
         implicit none
         type(bctype),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_restart_bctype(this,dir)
         implicit none
         type(bctype),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_bctype(this)
         implicit none
         type(bctype),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module