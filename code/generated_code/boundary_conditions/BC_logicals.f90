       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module BC_logicals_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: BC_logicals
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_BC_logicals;          end interface
       interface delete;           module procedure delete_BC_logicals;             end interface
       interface display;          module procedure display_BC_logicals;            end interface
       interface display_short;    module procedure display_short_BC_logicals;      end interface
       interface display;          module procedure display_wrap_BC_logicals;       end interface
       interface print;            module procedure print_BC_logicals;              end interface
       interface print_short;      module procedure print_short_BC_logicals;        end interface
       interface export;           module procedure export_BC_logicals;             end interface
       interface export_primitives;module procedure export_primitives_BC_logicals;  end interface
       interface import;           module procedure import_BC_logicals;             end interface
       interface export_structured;module procedure export_structured_D_BC_logicals;end interface
       interface import_structured;module procedure import_structured_D_BC_logicals;end interface
       interface import_primitives;module procedure import_primitives_BC_logicals;  end interface
       interface export;           module procedure export_wrap_BC_logicals;        end interface
       interface import;           module procedure import_wrap_BC_logicals;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_BC_logicals;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_BC_logicals;        end interface
       interface suppress_warnings;module procedure suppress_warnings_BC_logicals;  end interface

       type BC_logicals
         logical :: defined = .false.
         logical :: GFs_defined = .false.
         logical :: BCT_defined = .false.
         logical :: vals_defined = .false.
         logical :: all_Dirichlet = .false.
         logical :: all_Neumann = .false.
         logical :: all_Robin = .false.
         logical :: all_symmetric = .false.
         logical :: all_antisymmetric = .false.
         logical :: any_Dirichlet = .false.
         logical :: any_Neumann = .false.
         logical :: any_Robin = .false.
         logical :: any_symmetric = .false.
         logical :: any_antisymmetric = .false.
         logical :: any_prescribed = .false.
       end type

       contains

       subroutine init_copy_BC_logicals(this,that)
         implicit none
         type(BC_logicals),intent(inout) :: this
         type(BC_logicals),intent(in) :: that
         call delete(this)
         this%defined = that%defined
         this%GFs_defined = that%GFs_defined
         this%BCT_defined = that%BCT_defined
         this%vals_defined = that%vals_defined
         this%all_Dirichlet = that%all_Dirichlet
         this%all_Neumann = that%all_Neumann
         this%all_Robin = that%all_Robin
         this%all_symmetric = that%all_symmetric
         this%all_antisymmetric = that%all_antisymmetric
         this%any_Dirichlet = that%any_Dirichlet
         this%any_Neumann = that%any_Neumann
         this%any_Robin = that%any_Robin
         this%any_symmetric = that%any_symmetric
         this%any_antisymmetric = that%any_antisymmetric
         this%any_prescribed = that%any_prescribed
       end subroutine

       subroutine delete_BC_logicals(this)
         implicit none
         type(BC_logicals),intent(inout) :: this
         this%defined = .false.
         this%GFs_defined = .false.
         this%BCT_defined = .false.
         this%vals_defined = .false.
         this%all_Dirichlet = .false.
         this%all_Neumann = .false.
         this%all_Robin = .false.
         this%all_symmetric = .false.
         this%all_antisymmetric = .false.
         this%any_Dirichlet = .false.
         this%any_Neumann = .false.
         this%any_Robin = .false.
         this%any_symmetric = .false.
         this%any_antisymmetric = .false.
         this%any_prescribed = .false.
       end subroutine

       subroutine display_BC_logicals(this,un)
         implicit none
         type(BC_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined           = ',this%defined
         write(un,*) 'GFs_defined       = ',this%GFs_defined
         write(un,*) 'BCT_defined       = ',this%BCT_defined
         write(un,*) 'vals_defined      = ',this%vals_defined
         write(un,*) 'all_Dirichlet     = ',this%all_Dirichlet
         write(un,*) 'all_Neumann       = ',this%all_Neumann
         write(un,*) 'all_Robin         = ',this%all_Robin
         write(un,*) 'all_symmetric     = ',this%all_symmetric
         write(un,*) 'all_antisymmetric = ',this%all_antisymmetric
         write(un,*) 'any_Dirichlet     = ',this%any_Dirichlet
         write(un,*) 'any_Neumann       = ',this%any_Neumann
         write(un,*) 'any_Robin         = ',this%any_Robin
         write(un,*) 'any_symmetric     = ',this%any_symmetric
         write(un,*) 'any_antisymmetric = ',this%any_antisymmetric
         write(un,*) 'any_prescribed    = ',this%any_prescribed
       end subroutine

       subroutine display_short_BC_logicals(this,un)
         implicit none
         type(BC_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined           = ',this%defined
         write(un,*) 'GFs_defined       = ',this%GFs_defined
         write(un,*) 'BCT_defined       = ',this%BCT_defined
         write(un,*) 'vals_defined      = ',this%vals_defined
         write(un,*) 'all_Dirichlet     = ',this%all_Dirichlet
         write(un,*) 'all_Neumann       = ',this%all_Neumann
         write(un,*) 'all_Robin         = ',this%all_Robin
         write(un,*) 'all_symmetric     = ',this%all_symmetric
         write(un,*) 'all_antisymmetric = ',this%all_antisymmetric
         write(un,*) 'any_Dirichlet     = ',this%any_Dirichlet
         write(un,*) 'any_Neumann       = ',this%any_Neumann
         write(un,*) 'any_Robin         = ',this%any_Robin
         write(un,*) 'any_symmetric     = ',this%any_symmetric
         write(un,*) 'any_antisymmetric = ',this%any_antisymmetric
         write(un,*) 'any_prescribed    = ',this%any_prescribed
       end subroutine

       subroutine display_wrap_BC_logicals(this,dir,name)
         implicit none
         type(BC_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_BC_logicals(this)
         implicit none
         type(BC_logicals),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_BC_logicals(this)
         implicit none
         type(BC_logicals),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_BC_logicals(this,un)
         implicit none
         type(BC_logicals),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_BC_logicals(this,un)
         implicit none
         type(BC_logicals),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_BC_logicals(this,un)
         implicit none
         type(BC_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined            = ';write(un,*) this%defined
         write(un,*) 'GFs_defined        = ';write(un,*) this%GFs_defined
         write(un,*) 'BCT_defined        = ';write(un,*) this%BCT_defined
         write(un,*) 'vals_defined       = ';write(un,*) this%vals_defined
         write(un,*) 'all_Dirichlet      = ';write(un,*) this%all_Dirichlet
         write(un,*) 'all_Neumann        = ';write(un,*) this%all_Neumann
         write(un,*) 'all_Robin          = ';write(un,*) this%all_Robin
         write(un,*) 'all_symmetric      = ';write(un,*) this%all_symmetric
         write(un,*) 'all_antisymmetric  = ';write(un,*) this%all_antisymmetric
         write(un,*) 'any_Dirichlet      = ';write(un,*) this%any_Dirichlet
         write(un,*) 'any_Neumann        = ';write(un,*) this%any_Neumann
         write(un,*) 'any_Robin          = ';write(un,*) this%any_Robin
         write(un,*) 'any_symmetric      = ';write(un,*) this%any_symmetric
         write(un,*) 'any_antisymmetric  = ';write(un,*) this%any_antisymmetric
         write(un,*) 'any_prescribed     = ';write(un,*) this%any_prescribed
       end subroutine

       subroutine import_primitives_BC_logicals(this,un)
         implicit none
         type(BC_logicals),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%GFs_defined
         read(un,*); read(un,*) this%BCT_defined
         read(un,*); read(un,*) this%vals_defined
         read(un,*); read(un,*) this%all_Dirichlet
         read(un,*); read(un,*) this%all_Neumann
         read(un,*); read(un,*) this%all_Robin
         read(un,*); read(un,*) this%all_symmetric
         read(un,*); read(un,*) this%all_antisymmetric
         read(un,*); read(un,*) this%any_Dirichlet
         read(un,*); read(un,*) this%any_Neumann
         read(un,*); read(un,*) this%any_Robin
         read(un,*); read(un,*) this%any_symmetric
         read(un,*); read(un,*) this%any_antisymmetric
         read(un,*); read(un,*) this%any_prescribed
       end subroutine

       subroutine export_wrap_BC_logicals(this,dir,name)
         implicit none
         type(BC_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_BC_logicals(this,dir,name)
         implicit none
         type(BC_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_BC_logicals(this,dir)
         implicit none
         type(BC_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_BC_logicals(this,dir)
         implicit none
         type(BC_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_BC_logicals(this,dir)
         implicit none
         type(BC_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_BC_logicals(this,dir)
         implicit none
         type(BC_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_BC_logicals(this)
         implicit none
         type(BC_logicals),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module