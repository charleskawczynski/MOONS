       module export_frequency_params_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none
       private
       public :: export_frequency_params
       public :: init,delete,display,print,export,import
       public :: update

       integer,parameter :: frequency_base_default = 1
       integer,parameter :: frequency_exp_default = 6

       interface init;    module procedure init_EFP;      end interface
       interface init;    module procedure init_EFP_copy; end interface
       interface delete;  module procedure delete_EFP;    end interface
       interface display; module procedure display_EFP;   end interface
       interface print;   module procedure print_EFP;     end interface
       interface export;  module procedure export_EFP;    end interface
       interface import;  module procedure import_EFP;    end interface

       interface update;  module procedure update_EFP;         end interface

       type export_frequency_params
         logical :: export_ever = .false.
         logical :: export_first_step = .false.
         logical :: export_now = .false.
         integer :: frequency_base = frequency_base_default
         integer :: frequency_exp = frequency_exp_default
       end type

       contains

       subroutine init_EFP(EFP,export_ever,export_first_step,frequency_base,frequency_exp)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         logical,intent(in) :: export_ever,export_first_step
         integer,intent(in) :: frequency_base,frequency_exp
         EFP%export_ever       = export_ever
         EFP%export_now        = .false.
         EFP%export_first_step = export_first_step
         EFP%frequency_base    = frequency_base
         EFP%frequency_exp     = frequency_exp
       end subroutine

       subroutine init_EFP_copy(EFP,EFP_in)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         type(export_frequency_params),intent(in) :: EFP_in
         EFP%export_ever       = EFP_in%export_ever
         EFP%export_first_step = EFP_in%export_first_step
         EFP%export_now        = EFP_in%export_now
         EFP%frequency_base    = EFP_in%frequency_base
         EFP%frequency_exp     = EFP_in%frequency_exp
       end subroutine

       subroutine delete_EFP(EFP)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         EFP%export_ever       = .false.
         EFP%export_first_step = .false.
         EFP%export_now        = .false.
         EFP%frequency_base    = frequency_base_default
         EFP%frequency_exp     = frequency_exp_default
       end subroutine

       subroutine display_EFP(EFP,un)
         implicit none
         type(export_frequency_params),intent(in) :: EFP
         integer,intent(in) :: un
         write(un,*) 'export_ever       = ',EFP%export_ever
         write(un,*) 'export_first_step = ',EFP%export_first_step
         write(un,*) 'export_now        = ',EFP%export_now
         write(un,*) 'frequency_base    = ',EFP%frequency_base
         write(un,*) 'frequency_exp     = ',EFP%frequency_exp
       end subroutine

       subroutine print_EFP(EFP)
         implicit none
         type(export_frequency_params),intent(in) :: EFP
         call display(EFP,6)
       end subroutine

       subroutine export_EFP(EFP,un)
         implicit none
         type(export_frequency_params),intent(in) :: EFP
         integer,intent(in) :: un
         write(un,*) 'export_ever       = '; write(un,*) EFP%export_ever
         write(un,*) 'export_first_step = '; write(un,*) EFP%export_first_step
         write(un,*) 'export_now        = '; write(un,*) EFP%export_now
         write(un,*) 'frequency_base    = '; write(un,*) EFP%frequency_base
         write(un,*) 'frequency_exp     = '; write(un,*) EFP%frequency_exp
       end subroutine

       subroutine import_EFP(EFP,un)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         integer,intent(in) :: un
         read(un,*);call safe_read(EFP%export_ever      ,un,'import in export_frequency_params')
         read(un,*);call safe_read(EFP%export_first_step,un,'import in export_frequency_params')
         read(un,*);call safe_read(EFP%export_now       ,un,'import in export_frequency_params')
         read(un,*);call safe_read(EFP%frequency_base   ,un,'import in export_frequency_params')
         read(un,*);call safe_read(EFP%frequency_exp    ,un,'import in export_frequency_params')
         call safegaurd_import(EFP)
       end subroutine

       subroutine safegaurd_import(EFP)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         logical,dimension(2) :: L
         L(1) = EFP%frequency_exp.lt.0
         L(2) = EFP%frequency_exp.gt.6
         L(2) = EFP%frequency_base.lt.1
         if (any(L)) EFP%frequency_base = frequency_base_default
         if (any(L)) EFP%frequency_base = frequency_exp_default
       end subroutine

       subroutine update_EFP(EFP,n_step)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         integer(li),intent(in) :: n_step
         logical :: first_step,past_first_step
         if (EFP%export_ever) then
           first_step = n_step.eq.0
           past_first_step = n_step.gt.1
           EFP%export_now = mod(n_step,EFP%frequency_base**EFP%frequency_exp).eq.1
           if (EFP%export_first_step) then; EFP%export_now = EFP%export_now.or.first_step
           else;                           EFP%export_now = EFP%export_now.and.past_first_step
           endif
         else; EFP%export_now = .false.
         endif
       end subroutine

       end module