       module single_procedure_mod
       use IO_tools_mod
       use face_domain_mod
       use GF_mod
       use apply_BCs_faces_bridge_mod

       implicit none
       private
       public :: single_procedure
       public :: init,delete,display,print,export,import

       public :: insist_defined

       type single_procedure
         integer :: ID
         procedure(apply_BC_op),pointer,nopass :: P
         logical :: defined = .false.
       end type

       interface init;           module procedure init_SP;           end interface
       interface init;           module procedure init_copy_SP;      end interface
       interface delete;         module procedure delete_SP;         end interface
       interface display;        module procedure display_SP;        end interface
       interface print;          module procedure print_SP;          end interface
       interface export;         module procedure export_SP;         end interface
       interface import;         module procedure import_SP;         end interface
       interface export;         module procedure export_SP_wrapper; end interface
       interface import;         module procedure import_SP_wrapper; end interface

       interface insist_defined; module procedure insist_defined_SP; end interface


       contains

       subroutine init_SP(SP,P,ID)
         implicit none
         type(single_procedure),intent(inout) :: SP
         procedure(apply_BC_op) :: P
         integer,intent(in) :: ID
         call delete(SP)
         SP%P => P
         SP%ID = ID
         SP%defined = .true.
       end subroutine

       subroutine init_copy_SP(SP,SP_in)
         implicit none
         type(single_procedure),intent(inout) :: SP
         type(single_procedure),intent(in) :: SP_in
         call insist_defined(SP_in,'init_copy_SP')
         call delete(SP)
         SP%P => SP_in%P
         SP%ID = SP_in%ID
         SP%defined = SP_in%defined
       end subroutine

       subroutine delete_SP(SP)
         implicit none
         type(single_procedure),intent(inout) :: SP
         nullify(SP%P)
         ! SP%P => null()
         SP%ID = 0
         SP%defined = .false.
       end subroutine

       subroutine display_SP(SP,un)
         implicit none
         type(single_procedure),intent(in) :: SP
         integer,intent(in) :: un
         write(un,*) 'ID = ',SP%ID
       end subroutine

       subroutine print_SP(SP)
         implicit none
         type(single_procedure),intent(in) :: SP
         call display(SP,6)
       end subroutine

       subroutine export_SP(SP,un)
         implicit none
         type(single_procedure),intent(in) :: SP
         integer,intent(in) :: un
         write(un,*) 'ID = '; write(un,*) SP%ID
       end subroutine

       subroutine import_SP(SP,un)
         implicit none
         type(single_procedure),intent(inout) :: SP
         integer,intent(in) :: un
         read(un,*); read(un,*) SP%ID
       end subroutine

       subroutine export_SP_wrapper(SP,dir,name)
         implicit none
         type(single_procedure),intent(in) :: SP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(SP,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_SP_wrapper(SP,dir,name)
         implicit none
         type(single_procedure),intent(inout) :: SP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(SP,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine insist_defined_SP(SP,caller)
         implicit none
         type(single_procedure),intent(in) :: SP
         character(len=*),intent(in) :: caller
         if (.not.SP%defined) then
         write(*,*) 'Error: SP is not defined in ',caller,' in single_procedure.f90'
         stop 'Done'
         endif
       end subroutine

       end module