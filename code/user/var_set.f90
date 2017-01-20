       module var_set_mod
       use var_mod
       use string_mod
       implicit none

       private
       public :: var_set
       public :: init,delete,export,import,display,print

       public :: export_import_SS

       type var_set
         type(var) :: T,U,p,B,B0,phi
       end type

       interface init;             module procedure init_VS;             end interface
       interface init;             module procedure init_copy_VS;        end interface
       interface delete;           module procedure delete_VS;           end interface
       interface export;           module procedure export_VS;           end interface
       interface import;           module procedure import_VS;           end interface
       interface display;          module procedure display_VS;          end interface
       interface print;            module procedure print_VS;            end interface

       interface export_import_SS; module procedure export_import_SS_VS; end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_VS(VS,T,U,P,B,B0,phi)
         implicit none
         type(var_set),intent(inout) :: VS
         type(var),intent(in) :: T,U,P,B,B0,phi
         call init(VS%T,T)
         call init(VS%U,U)
         call init(VS%P,P)
         call init(VS%B,B)
         call init(VS%B0,B0)
         call init(VS%phi,phi)
       end subroutine

       subroutine init_copy_VS(VS,VS_in)
         implicit none
         type(var_set),intent(inout) :: VS
         type(var_set),intent(in) :: VS_in
         call init(VS%T,VS_in%T)
         call init(VS%U,VS_in%U)
         call init(VS%P,VS_in%P)
         call init(VS%B,VS_in%B)
         call init(VS%B0,VS_in%B0)
         call init(VS%phi,VS_in%phi)
       end subroutine

       subroutine delete_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         call delete(VS%T)
         call delete(VS%U)
         call delete(VS%P)
         call delete(VS%B)
         call delete(VS%B0)
         call delete(VS%phi)
       end subroutine

       subroutine export_VS(VS,un)
         implicit none
         type(var_set),intent(in) :: VS
         integer,intent(in) :: un
         call export(VS%T,un)
         call export(VS%U,un)
         call export(VS%P,un)
         call export(VS%B,un)
         call export(VS%B0,un)
         call export(VS%phi,un)
       end subroutine

       subroutine import_VS(VS,un)
         implicit none
         type(var_set),intent(inout) :: VS
         integer,intent(in) :: un
         call import(VS%T,un)
         call import(VS%U,un)
         call import(VS%P,un)
         call import(VS%B,un)
         call import(VS%B0,un)
         call import(VS%phi,un)
       end subroutine

       subroutine display_VS(VS,un)
         implicit none
         type(var_set),intent(in) :: VS
         integer,intent(in) :: un
         write(un,*) '---------- VAR T ----------';   call display(VS%T,un)
         write(un,*) '---------- VAR U ----------';   call display(VS%U,un)
         write(un,*) '---------- VAR P ----------';   call display(VS%P,un)
         write(un,*) '---------- VAR B ----------';   call display(VS%B,un)
         write(un,*) '---------- VAR B0 ----------';  call display(VS%B0,un)
         write(un,*) '---------- VAR phi ----------'; call display(VS%phi,un)
         write(un,*) '-----------------------------'
       end subroutine

       subroutine print_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         call display(VS,6)
       end subroutine

       subroutine export_import_SS_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         if(VS%T%SS%restart) then;  call import(VS%T%ISP);  else;call export(VS%T%ISP);  endif
         if(VS%U%SS%restart) then;  call import(VS%U%ISP);  else;call export(VS%U%ISP);  endif
         if(VS%P%SS%restart) then;  call import(VS%P%ISP);  else;call export(VS%P%ISP);  endif
         if(VS%B%SS%restart) then;  call import(VS%B%ISP);  else;call export(VS%B%ISP);  endif
         if(VS%B0%SS%restart) then; call import(VS%B0%ISP); else;call export(VS%B0%ISP); endif
         if(VS%phi%SS%restart) then;call import(VS%phi%ISP);else;call export(VS%phi%ISP);endif

         if(VS%T%SS%restart) then;  call import(VS%T%TMP);  else;call export(VS%T%TMP);  endif
         if(VS%U%SS%restart) then;  call import(VS%U%TMP);  else;call export(VS%U%TMP);  endif
         if(VS%P%SS%restart) then;  call import(VS%P%TMP);  else;call export(VS%P%TMP);  endif
         if(VS%B%SS%restart) then;  call import(VS%B%TMP);  else;call export(VS%B%TMP);  endif
         if(VS%B0%SS%restart) then; call import(VS%B0%TMP); else;call export(VS%B0%TMP); endif
         if(VS%phi%SS%restart) then;call import(VS%phi%TMP);else;call export(VS%phi%TMP);endif
       end subroutine

       end module