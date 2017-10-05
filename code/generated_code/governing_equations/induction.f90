       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module induction_mod
       use IO_tools_mod
       use PCG_solver_SF_mod
       use PCG_solver_VF_mod
       use SF_mod
       use TF_mod
       use VF_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use mesh_mod
       use mesh_domain_mod
       use string_mod
       implicit none

       private
       public :: induction
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_induction;          end interface
       interface delete;           module procedure delete_induction;             end interface
       interface display;          module procedure display_induction;            end interface
       interface display_short;    module procedure display_short_induction;      end interface
       interface display;          module procedure display_wrap_induction;       end interface
       interface print;            module procedure print_induction;              end interface
       interface print_short;      module procedure print_short_induction;        end interface
       interface export;           module procedure export_induction;             end interface
       interface export_primitives;module procedure export_primitives_induction;  end interface
       interface import;           module procedure import_induction;             end interface
       interface export_structured;module procedure export_structured_D_induction;end interface
       interface import_structured;module procedure import_structured_D_induction;end interface
       interface import_primitives;module procedure import_primitives_induction;  end interface
       interface export;           module procedure export_wrap_induction;        end interface
       interface import;           module procedure import_wrap_induction;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_induction;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_induction;        end interface
       interface suppress_warnings;module procedure suppress_warnings_induction;  end interface

       type induction
         logical :: suppress_warning = .false.
         type(mesh) :: m
         type(mesh) :: m_sigma
         type(PCG_Solver_VF) :: PCG_B
         type(PCG_Solver_SF) :: PCG_cleanB
         type(TF) :: U_E
         type(TF) :: temp_E_TF
         type(TF) :: temp_F1_TF
         type(TF) :: temp_F2_TF
         type(SF) :: sigmaInv_CC
         type(SF) :: divB
         type(SF) :: divJ
         type(SF) :: phi
         type(SF) :: temp_CC
         type(VF) :: F
         type(VF) :: Fnm1
         type(VF) :: L
         type(VF) :: J
         type(VF) :: temp_E
         type(VF) :: B
         type(VF) :: Bnm1
         type(VF) :: B0
         type(VF) :: B_interior
         type(VF) :: temp_F1
         type(VF) :: temp_F2
         type(VF) :: Bstar
         type(VF) :: dB0dt
         type(VF) :: temp_CC_VF
         type(VF) :: sigmaInv_edge
         type(VF) :: J_interior
         type(VF) :: curlUCrossB
         type(VF) :: CC_VF_fluid
         type(VF) :: CC_VF_sigma
         type(mesh_domain) :: MD_fluid
         type(mesh_domain) :: MD_sigma
       end type

       contains

       subroutine init_copy_induction(this,that)
         implicit none
         type(induction),intent(inout) :: this
         type(induction),intent(in) :: that
         call delete(this)
         this%suppress_warning = that%suppress_warning
         call init(this%m,that%m)
         call init(this%m_sigma,that%m_sigma)
         call init(this%PCG_B,that%PCG_B)
         call init(this%PCG_cleanB,that%PCG_cleanB)
         call init(this%U_E,that%U_E)
         call init(this%temp_E_TF,that%temp_E_TF)
         call init(this%temp_F1_TF,that%temp_F1_TF)
         call init(this%temp_F2_TF,that%temp_F2_TF)
         call init(this%sigmaInv_CC,that%sigmaInv_CC)
         call init(this%divB,that%divB)
         call init(this%divJ,that%divJ)
         call init(this%phi,that%phi)
         call init(this%temp_CC,that%temp_CC)
         call init(this%F,that%F)
         call init(this%Fnm1,that%Fnm1)
         call init(this%L,that%L)
         call init(this%J,that%J)
         call init(this%temp_E,that%temp_E)
         call init(this%B,that%B)
         call init(this%Bnm1,that%Bnm1)
         call init(this%B0,that%B0)
         call init(this%B_interior,that%B_interior)
         call init(this%temp_F1,that%temp_F1)
         call init(this%temp_F2,that%temp_F2)
         call init(this%Bstar,that%Bstar)
         call init(this%dB0dt,that%dB0dt)
         call init(this%temp_CC_VF,that%temp_CC_VF)
         call init(this%sigmaInv_edge,that%sigmaInv_edge)
         call init(this%J_interior,that%J_interior)
         call init(this%curlUCrossB,that%curlUCrossB)
         call init(this%CC_VF_fluid,that%CC_VF_fluid)
         call init(this%CC_VF_sigma,that%CC_VF_sigma)
         call init(this%MD_fluid,that%MD_fluid)
         call init(this%MD_sigma,that%MD_sigma)
       end subroutine

       subroutine delete_induction(this)
         implicit none
         type(induction),intent(inout) :: this
         this%suppress_warning = .false.
         call delete(this%m)
         call delete(this%m_sigma)
         call delete(this%PCG_B)
         call delete(this%PCG_cleanB)
         call delete(this%U_E)
         call delete(this%temp_E_TF)
         call delete(this%temp_F1_TF)
         call delete(this%temp_F2_TF)
         call delete(this%sigmaInv_CC)
         call delete(this%divB)
         call delete(this%divJ)
         call delete(this%phi)
         call delete(this%temp_CC)
         call delete(this%F)
         call delete(this%Fnm1)
         call delete(this%L)
         call delete(this%J)
         call delete(this%temp_E)
         call delete(this%B)
         call delete(this%Bnm1)
         call delete(this%B0)
         call delete(this%B_interior)
         call delete(this%temp_F1)
         call delete(this%temp_F2)
         call delete(this%Bstar)
         call delete(this%dB0dt)
         call delete(this%temp_CC_VF)
         call delete(this%sigmaInv_edge)
         call delete(this%J_interior)
         call delete(this%curlUCrossB)
         call delete(this%CC_VF_fluid)
         call delete(this%CC_VF_sigma)
         call delete(this%MD_fluid)
         call delete(this%MD_sigma)
       end subroutine

       subroutine display_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%m_sigma,un)
         call display(this%PCG_B,un)
         call display(this%PCG_cleanB,un)
         call display(this%U_E,un)
         call display(this%temp_E_TF,un)
         call display(this%temp_F1_TF,un)
         call display(this%temp_F2_TF,un)
         call display(this%sigmaInv_CC,un)
         call display(this%divB,un)
         call display(this%divJ,un)
         call display(this%phi,un)
         call display(this%temp_CC,un)
         call display(this%F,un)
         call display(this%Fnm1,un)
         call display(this%L,un)
         call display(this%J,un)
         call display(this%temp_E,un)
         call display(this%B,un)
         call display(this%Bnm1,un)
         call display(this%B0,un)
         call display(this%B_interior,un)
         call display(this%temp_F1,un)
         call display(this%temp_F2,un)
         call display(this%Bstar,un)
         call display(this%dB0dt,un)
         call display(this%temp_CC_VF,un)
         call display(this%sigmaInv_edge,un)
         call display(this%J_interior,un)
         call display(this%curlUCrossB,un)
         call display(this%CC_VF_fluid,un)
         call display(this%CC_VF_sigma,un)
         call display(this%MD_fluid,un)
         call display(this%MD_sigma,un)
       end subroutine

       subroutine display_short_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%m_sigma,un)
         call display(this%PCG_B,un)
         call display(this%PCG_cleanB,un)
         call display(this%U_E,un)
         call display(this%temp_E_TF,un)
         call display(this%temp_F1_TF,un)
         call display(this%temp_F2_TF,un)
         call display(this%sigmaInv_CC,un)
         call display(this%divB,un)
         call display(this%divJ,un)
         call display(this%phi,un)
         call display(this%temp_CC,un)
         call display(this%F,un)
         call display(this%Fnm1,un)
         call display(this%L,un)
         call display(this%J,un)
         call display(this%temp_E,un)
         call display(this%B,un)
         call display(this%Bnm1,un)
         call display(this%B0,un)
         call display(this%B_interior,un)
         call display(this%temp_F1,un)
         call display(this%temp_F2,un)
         call display(this%Bstar,un)
         call display(this%dB0dt,un)
         call display(this%temp_CC_VF,un)
         call display(this%sigmaInv_edge,un)
         call display(this%J_interior,un)
         call display(this%curlUCrossB,un)
         call display(this%CC_VF_fluid,un)
         call display(this%CC_VF_sigma,un)
         call display(this%MD_fluid,un)
         call display(this%MD_sigma,un)
       end subroutine

       subroutine display_wrap_induction(this,dir,name)
         implicit none
         type(induction),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_induction(this)
         implicit none
         type(induction),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_induction(this)
         implicit none
         type(induction),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
         call export(this%m,un)
         call export(this%m_sigma,un)
         call export(this%PCG_B,un)
         call export(this%PCG_cleanB,un)
         call export(this%U_E,un)
         call export(this%temp_E_TF,un)
         call export(this%temp_F1_TF,un)
         call export(this%temp_F2_TF,un)
         call export(this%sigmaInv_CC,un)
         call export(this%divB,un)
         call export(this%divJ,un)
         call export(this%phi,un)
         call export(this%temp_CC,un)
         call export(this%F,un)
         call export(this%Fnm1,un)
         call export(this%L,un)
         call export(this%J,un)
         call export(this%temp_E,un)
         call export(this%B,un)
         call export(this%Bnm1,un)
         call export(this%B0,un)
         call export(this%B_interior,un)
         call export(this%temp_F1,un)
         call export(this%temp_F2,un)
         call export(this%Bstar,un)
         call export(this%dB0dt,un)
         call export(this%temp_CC_VF,un)
         call export(this%sigmaInv_edge,un)
         call export(this%J_interior,un)
         call export(this%curlUCrossB,un)
         call export(this%CC_VF_fluid,un)
         call export(this%CC_VF_sigma,un)
         call export(this%MD_fluid,un)
         call export(this%MD_sigma,un)
       end subroutine

       subroutine import_induction(this,un)
         implicit none
         type(induction),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
         call import(this%m,un)
         call import(this%m_sigma,un)
         call import(this%PCG_B,un)
         call import(this%PCG_cleanB,un)
         call import(this%U_E,un)
         call import(this%temp_E_TF,un)
         call import(this%temp_F1_TF,un)
         call import(this%temp_F2_TF,un)
         call import(this%sigmaInv_CC,un)
         call import(this%divB,un)
         call import(this%divJ,un)
         call import(this%phi,un)
         call import(this%temp_CC,un)
         call import(this%F,un)
         call import(this%Fnm1,un)
         call import(this%L,un)
         call import(this%J,un)
         call import(this%temp_E,un)
         call import(this%B,un)
         call import(this%Bnm1,un)
         call import(this%B0,un)
         call import(this%B_interior,un)
         call import(this%temp_F1,un)
         call import(this%temp_F2,un)
         call import(this%Bstar,un)
         call import(this%dB0dt,un)
         call import(this%temp_CC_VF,un)
         call import(this%sigmaInv_edge,un)
         call import(this%J_interior,un)
         call import(this%curlUCrossB,un)
         call import(this%CC_VF_fluid,un)
         call import(this%CC_VF_sigma,un)
         call import(this%MD_fluid,un)
         call import(this%MD_sigma,un)
       end subroutine

       subroutine export_primitives_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning  = ';write(un,*) this%suppress_warning
       end subroutine

       subroutine import_primitives_induction(this,un)
         implicit none
         type(induction),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%suppress_warning
       end subroutine

       subroutine export_wrap_induction(this,dir,name)
         implicit none
         type(induction),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_induction(this,dir,name)
         implicit none
         type(induction),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_induction(this,dir)
         implicit none
         type(induction),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%PCG_B,dir//'PCG_B'//fortran_PS)
         call set_IO_dir(this%PCG_cleanB,dir//'PCG_cleanB'//fortran_PS)
         call set_IO_dir(this%MD_fluid,dir//'MD_fluid'//fortran_PS)
         call set_IO_dir(this%MD_sigma,dir//'MD_sigma'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_induction(this,dir)
         implicit none
         type(induction),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%PCG_B,dir//'PCG_B'//fortran_PS)
         call make_IO_dir(this%PCG_cleanB,dir//'PCG_cleanB'//fortran_PS)
         call make_IO_dir(this%MD_fluid,dir//'MD_fluid'//fortran_PS)
         call make_IO_dir(this%MD_sigma,dir//'MD_sigma'//fortran_PS)
       end subroutine

       subroutine export_structured_D_induction(this,dir)
         implicit none
         type(induction),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%PCG_B,dir//'PCG_B'//fortran_PS)
         call export_structured(this%PCG_cleanB,&
         dir//'PCG_cleanB'//fortran_PS)
         call export_structured(this%MD_fluid,dir//'MD_fluid'//fortran_PS)
         call export_structured(this%MD_sigma,dir//'MD_sigma'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_induction(this,dir)
         implicit none
         type(induction),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%PCG_B,dir//'PCG_B'//fortran_PS)
         call import_structured(this%PCG_cleanB,&
         dir//'PCG_cleanB'//fortran_PS)
         call import_structured(this%MD_fluid,dir//'MD_fluid'//fortran_PS)
         call import_structured(this%MD_sigma,dir//'MD_sigma'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_induction(this)
         implicit none
         type(induction),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module