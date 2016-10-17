       module BC_logicals_mod
       use IO_tools_mod

       implicit none
       private
       public :: BC_logicals
       public :: init,delete,display,print,export,import ! essentials

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
       end type

       interface init;           module procedure init_copy_BCL;      end interface
       interface delete;         module procedure delete_BCL;         end interface
       interface display;        module procedure display_BCL;        end interface
       interface print;          module procedure print_BCL;          end interface
       interface export;         module procedure export_BCL;         end interface
       interface import;         module procedure import_BCL;         end interface
       interface export;         module procedure export_BCL_wrapper; end interface
       interface import;         module procedure import_BCL_wrapper; end interface

       contains

       ! ****************************************************************
       ! ************************** ESSENTIALS **************************
       ! ****************************************************************

       subroutine init_copy_BCL(BCL,BCL_in)
         implicit none
         type(BC_logicals),intent(inout) :: BCL
         type(BC_logicals),intent(in) :: BCL_in
         BCL%defined = BCL_in%defined
         BCL%GFs_defined = BCL_in%GFs_defined
         BCL%BCT_defined = BCL_in%BCT_defined
         BCL%vals_defined = BCL_in%vals_defined
         BCL%all_Dirichlet = BCL_in%all_Dirichlet
         BCL%all_Neumann = BCL_in%all_Neumann
         BCL%all_Robin = BCL_in%all_Robin
         BCL%all_symmetric = BCL_in%all_symmetric
         BCL%all_antisymmetric = BCL_in%all_antisymmetric
       end subroutine

       subroutine delete_BCL(BCL)
         implicit none
         type(BC_logicals),intent(inout) :: BCL
         BCL%defined = .false.
         BCL%GFs_defined = .false.
         BCL%BCT_defined = .false.
         BCL%vals_defined = .false.
         BCL%all_Dirichlet = .false.
         BCL%all_Neumann = .false.
         BCL%all_Robin = .false.
         BCL%all_symmetric = .false.
         BCL%all_antisymmetric = .false.
       end subroutine

       subroutine display_BCL(BCL,un)
         implicit none
         type(BC_logicals),intent(in) :: BCL
         integer,intent(in) :: un
         write(un,*) 'defined = ',BCL%defined
         write(un,*) 'GFs_defined = ',BCL%GFs_defined
         write(un,*) 'BCT_defined = ',BCL%BCT_defined
         write(un,*) 'vals_defined = ',BCL%vals_defined
         write(un,*) 'all_Dirichlet = ',BCL%all_Dirichlet
         write(un,*) 'all_Neumann = ',BCL%all_Neumann
         write(un,*) 'all_Robin = ',BCL%all_Robin
         write(un,*) 'all_symmetric = ',BCL%all_symmetric
         write(un,*) 'all_antisymmetric = ',BCL%all_antisymmetric
       end subroutine

       subroutine print_BCL(BCL)
         implicit none
         type(BC_logicals),intent(in) :: BCL
         call display(BCL,6)
       end subroutine

       subroutine export_BCL(BCL,un)
         implicit none
         type(BC_logicals),intent(in) :: BCL
         integer,intent(in) :: un
         write(un,*) 'defined = ';           write(un,*) BCL%defined
         write(un,*) 'GFs_defined = ';       write(un,*) BCL%GFs_defined
         write(un,*) 'BCT_defined = ';       write(un,*) BCL%BCT_defined
         write(un,*) 'vals_defined = ';      write(un,*) BCL%vals_defined
         write(un,*) 'all_Dirichlet = ';     write(un,*) BCL%all_Dirichlet
         write(un,*) 'all_Neumann = ';       write(un,*) BCL%all_Neumann
         write(un,*) 'all_Robin = ';         write(un,*) BCL%all_Robin
         write(un,*) 'all_symmetric = ';     write(un,*) BCL%all_symmetric
         write(un,*) 'all_antisymmetric = '; write(un,*) BCL%all_antisymmetric
       end subroutine

       subroutine import_BCL(BCL,un)
         implicit none
         type(BC_logicals),intent(inout) :: BCL
         integer,intent(in) :: un
         read(un,*); read(un,*) BCL%defined
         read(un,*); read(un,*) BCL%GFs_defined
         read(un,*); read(un,*) BCL%BCT_defined
         read(un,*); read(un,*) BCL%vals_defined
         read(un,*); read(un,*) BCL%all_Dirichlet
         read(un,*); read(un,*) BCL%all_Neumann
         read(un,*); read(un,*) BCL%all_Robin
         read(un,*); read(un,*) BCL%all_symmetric
         read(un,*); read(un,*) BCL%all_antisymmetric
       end subroutine

       subroutine export_BCL_wrapper(BCL,dir,name)
         implicit none
         type(BC_logicals),intent(in) :: BCL
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(BCL,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_BCL_wrapper(BCL,dir,name)
         implicit none
         type(BC_logicals),intent(inout) :: BCL
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(BCL,un)
         call close_and_message(un,dir,name)
       end subroutine

       end module