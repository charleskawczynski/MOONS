       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module induction_mod
       use PCG_solver_SF_mod
       use string_mod
       use datatype_conversion_mod
       use mesh_domain_mod
       use IO_tools_mod
       use VF_mod
       use PCG_solver_VF_mod
       use TF_mod
       use mesh_mod
       use FFT_Solver_SF_mod
       use SF_mod
       use dir_manip_mod
       implicit none

       private
       public :: induction
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_induction;              end interface
       interface delete;                 module procedure delete_induction;                 end interface
       interface display;                module procedure display_induction;                end interface
       interface display_short;          module procedure display_short_induction;          end interface
       interface display;                module procedure display_wrap_induction;           end interface
       interface print;                  module procedure print_induction;                  end interface
       interface print_short;            module procedure print_short_induction;            end interface
       interface export;                 module procedure export_induction;                 end interface
       interface export_primitives;      module procedure export_primitives_induction;      end interface
       interface import;                 module procedure import_induction;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_induction;end interface
       interface export_structured;      module procedure export_structured_D_induction;    end interface
       interface import_structured;      module procedure import_structured_D_induction;    end interface
       interface import_primitives;      module procedure import_primitives_induction;      end interface
       interface export;                 module procedure export_wrap_induction;            end interface
       interface import;                 module procedure import_wrap_induction;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_induction;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_induction;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_induction;      end interface

       type induction
         logical :: suppress_warning = .false.
         type(mesh) :: m
         type(mesh) :: m_sigma
         type(PCG_Solver_VF) :: PCG_B
         type(PCG_Solver_SF) :: PCG_cleanB
         type(FFT_Solver_SF) :: FFT_cleanB
         type(TF) :: U_E
         type(TF) :: temp_E_TF
         type(TF) :: temp_F1_TF
         type(TF) :: temp_F2_TF
         type(TF) :: stresses
         type(TF) :: temp_CC_TF
         type(SF) :: sigmaInv_CC
         type(SF) :: cell_volume
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
         type(VF) :: Btot
         type(VF) :: B_interior
         type(VF) :: temp_F1
         type(VF) :: temp_F2
         type(VF) :: jCrossB
         type(VF) :: Bstar
         type(VF) :: dB0dt
         type(VF) :: sigmaInv_edge
         type(VF) :: J_interior
         type(VF) :: curlUCrossB
         type(VF) :: cell_inverse_area
         type(VF) :: CC_VF_fluid
         type(VF) :: CC_VF_sigma
         type(VF) :: temp_CC_VF
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
         call init(this%FFT_cleanB,that%FFT_cleanB)
         call init(this%U_E,that%U_E)
         call init(this%temp_E_TF,that%temp_E_TF)
         call init(this%temp_F1_TF,that%temp_F1_TF)
         call init(this%temp_F2_TF,that%temp_F2_TF)
         call init(this%stresses,that%stresses)
         call init(this%temp_CC_TF,that%temp_CC_TF)
         call init(this%sigmaInv_CC,that%sigmaInv_CC)
         call init(this%cell_volume,that%cell_volume)
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
         call init(this%Btot,that%Btot)
         call init(this%B_interior,that%B_interior)
         call init(this%temp_F1,that%temp_F1)
         call init(this%temp_F2,that%temp_F2)
         call init(this%jCrossB,that%jCrossB)
         call init(this%Bstar,that%Bstar)
         call init(this%dB0dt,that%dB0dt)
         call init(this%sigmaInv_edge,that%sigmaInv_edge)
         call init(this%J_interior,that%J_interior)
         call init(this%curlUCrossB,that%curlUCrossB)
         call init(this%cell_inverse_area,that%cell_inverse_area)
         call init(this%CC_VF_fluid,that%CC_VF_fluid)
         call init(this%CC_VF_sigma,that%CC_VF_sigma)
         call init(this%temp_CC_VF,that%temp_CC_VF)
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
         call delete(this%FFT_cleanB)
         call delete(this%U_E)
         call delete(this%temp_E_TF)
         call delete(this%temp_F1_TF)
         call delete(this%temp_F2_TF)
         call delete(this%stresses)
         call delete(this%temp_CC_TF)
         call delete(this%sigmaInv_CC)
         call delete(this%cell_volume)
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
         call delete(this%Btot)
         call delete(this%B_interior)
         call delete(this%temp_F1)
         call delete(this%temp_F2)
         call delete(this%jCrossB)
         call delete(this%Bstar)
         call delete(this%dB0dt)
         call delete(this%sigmaInv_edge)
         call delete(this%J_interior)
         call delete(this%curlUCrossB)
         call delete(this%cell_inverse_area)
         call delete(this%CC_VF_fluid)
         call delete(this%CC_VF_sigma)
         call delete(this%temp_CC_VF)
         call delete(this%MD_fluid)
         call delete(this%MD_sigma)
       end subroutine

       subroutine display_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning  = ',this%suppress_warning
         call display(this%m,un)
         call display(this%m_sigma,un)
         call display(this%PCG_B,un)
         call display(this%PCG_cleanB,un)
         call display(this%FFT_cleanB,un)
         call display(this%U_E,un)
         call display(this%temp_E_TF,un)
         call display(this%temp_F1_TF,un)
         call display(this%temp_F2_TF,un)
         call display(this%stresses,un)
         call display(this%temp_CC_TF,un)
         call display(this%sigmaInv_CC,un)
         call display(this%cell_volume,un)
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
         call display(this%Btot,un)
         call display(this%B_interior,un)
         call display(this%temp_F1,un)
         call display(this%temp_F2,un)
         call display(this%jCrossB,un)
         call display(this%Bstar,un)
         call display(this%dB0dt,un)
         call display(this%sigmaInv_edge,un)
         call display(this%J_interior,un)
         call display(this%curlUCrossB,un)
         call display(this%cell_inverse_area,un)
         call display(this%CC_VF_fluid,un)
         call display(this%CC_VF_sigma,un)
         call display(this%temp_CC_VF,un)
         call display(this%MD_fluid,un)
         call display(this%MD_sigma,un)
       end subroutine

       subroutine display_short_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning  = ',this%suppress_warning
         call display(this%m,un)
         call display(this%m_sigma,un)
         call display(this%PCG_B,un)
         call display(this%PCG_cleanB,un)
         call display(this%FFT_cleanB,un)
         call display(this%U_E,un)
         call display(this%temp_E_TF,un)
         call display(this%temp_F1_TF,un)
         call display(this%temp_F2_TF,un)
         call display(this%stresses,un)
         call display(this%temp_CC_TF,un)
         call display(this%sigmaInv_CC,un)
         call display(this%cell_volume,un)
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
         call display(this%Btot,un)
         call display(this%B_interior,un)
         call display(this%temp_F1,un)
         call display(this%temp_F2,un)
         call display(this%jCrossB,un)
         call display(this%Bstar,un)
         call display(this%dB0dt,un)
         call display(this%sigmaInv_edge,un)
         call display(this%J_interior,un)
         call display(this%curlUCrossB,un)
         call display(this%cell_inverse_area,un)
         call display(this%CC_VF_fluid,un)
         call display(this%CC_VF_sigma,un)
         call display(this%temp_CC_VF,un)
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
         call export(this%FFT_cleanB,un)
         call export(this%U_E,un)
         call export(this%temp_E_TF,un)
         call export(this%temp_F1_TF,un)
         call export(this%temp_F2_TF,un)
         call export(this%stresses,un)
         call export(this%temp_CC_TF,un)
         call export(this%sigmaInv_CC,un)
         call export(this%cell_volume,un)
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
         call export(this%Btot,un)
         call export(this%B_interior,un)
         call export(this%temp_F1,un)
         call export(this%temp_F2,un)
         call export(this%jCrossB,un)
         call export(this%Bstar,un)
         call export(this%dB0dt,un)
         call export(this%sigmaInv_edge,un)
         call export(this%J_interior,un)
         call export(this%curlUCrossB,un)
         call export(this%cell_inverse_area,un)
         call export(this%CC_VF_fluid,un)
         call export(this%CC_VF_sigma,un)
         call export(this%temp_CC_VF,un)
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
         call import(this%FFT_cleanB,un)
         call import(this%U_E,un)
         call import(this%temp_E_TF,un)
         call import(this%temp_F1_TF,un)
         call import(this%temp_F2_TF,un)
         call import(this%stresses,un)
         call import(this%temp_CC_TF,un)
         call import(this%sigmaInv_CC,un)
         call import(this%cell_volume,un)
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
         call import(this%Btot,un)
         call import(this%B_interior,un)
         call import(this%temp_F1,un)
         call import(this%temp_F2,un)
         call import(this%jCrossB,un)
         call import(this%Bstar,un)
         call import(this%dB0dt,un)
         call import(this%sigmaInv_edge,un)
         call import(this%J_interior,un)
         call import(this%curlUCrossB,un)
         call import(this%cell_inverse_area,un)
         call import(this%CC_VF_fluid,un)
         call import(this%CC_VF_sigma,un)
         call import(this%temp_CC_VF,un)
         call import(this%MD_fluid,un)
         call import(this%MD_sigma,un)
       end subroutine

       subroutine export_primitives_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning   = ';write(un,*) this%suppress_warning
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
         call set_IO_dir(this%FFT_cleanB,dir//'FFT_cleanB'//fortran_PS)
         call set_IO_dir(this%sigmaInv_CC,dir//'sigmaInv_CC'//fortran_PS)
         call set_IO_dir(this%cell_volume,dir//'cell_volume'//fortran_PS)
         call set_IO_dir(this%divB,dir//'divB'//fortran_PS)
         call set_IO_dir(this%divJ,dir//'divJ'//fortran_PS)
         call set_IO_dir(this%phi,dir//'phi'//fortran_PS)
         call set_IO_dir(this%temp_CC,dir//'temp_CC'//fortran_PS)
         call set_IO_dir(this%F,dir//'F'//fortran_PS)
         call set_IO_dir(this%Fnm1,dir//'Fnm1'//fortran_PS)
         call set_IO_dir(this%L,dir//'L'//fortran_PS)
         call set_IO_dir(this%J,dir//'J'//fortran_PS)
         call set_IO_dir(this%temp_E,dir//'temp_E'//fortran_PS)
         call set_IO_dir(this%B,dir//'B'//fortran_PS)
         call set_IO_dir(this%Bnm1,dir//'Bnm1'//fortran_PS)
         call set_IO_dir(this%B0,dir//'B0'//fortran_PS)
         call set_IO_dir(this%Btot,dir//'Btot'//fortran_PS)
         call set_IO_dir(this%B_interior,dir//'B_interior'//fortran_PS)
         call set_IO_dir(this%temp_F1,dir//'temp_F1'//fortran_PS)
         call set_IO_dir(this%temp_F2,dir//'temp_F2'//fortran_PS)
         call set_IO_dir(this%jCrossB,dir//'jCrossB'//fortran_PS)
         call set_IO_dir(this%Bstar,dir//'Bstar'//fortran_PS)
         call set_IO_dir(this%dB0dt,dir//'dB0dt'//fortran_PS)
         call set_IO_dir(this%sigmaInv_edge,&
         dir//'sigmaInv_edge'//fortran_PS)
         call set_IO_dir(this%J_interior,dir//'J_interior'//fortran_PS)
         call set_IO_dir(this%curlUCrossB,dir//'curlUCrossB'//fortran_PS)
         call set_IO_dir(this%cell_inverse_area,&
         dir//'cell_inverse_area'//fortran_PS)
         call set_IO_dir(this%CC_VF_fluid,dir//'CC_VF_fluid'//fortran_PS)
         call set_IO_dir(this%CC_VF_sigma,dir//'CC_VF_sigma'//fortran_PS)
         call set_IO_dir(this%temp_CC_VF,dir//'temp_CC_VF'//fortran_PS)
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
         call make_IO_dir(this%FFT_cleanB,dir//'FFT_cleanB'//fortran_PS)
         if (get_necessary_for_restart(this%sigmaInv_CC)) then
           call make_IO_dir(this%sigmaInv_CC,dir//'sigmaInv_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%cell_volume)) then
           call make_IO_dir(this%cell_volume,dir//'cell_volume'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divB)) then
           call make_IO_dir(this%divB,dir//'divB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divJ)) then
           call make_IO_dir(this%divJ,dir//'divJ'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%phi)) then
           call make_IO_dir(this%phi,dir//'phi'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC)) then
           call make_IO_dir(this%temp_CC,dir//'temp_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%F)) then
           call make_IO_dir(this%F,dir//'F'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Fnm1)) then
           call make_IO_dir(this%Fnm1,dir//'Fnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%L)) then
           call make_IO_dir(this%L,dir//'L'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%J)) then
           call make_IO_dir(this%J,dir//'J'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_E)) then
           call make_IO_dir(this%temp_E,dir//'temp_E'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B)) then
           call make_IO_dir(this%B,dir//'B'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Bnm1)) then
           call make_IO_dir(this%Bnm1,dir//'Bnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B0)) then
           call make_IO_dir(this%B0,dir//'B0'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Btot)) then
           call make_IO_dir(this%Btot,dir//'Btot'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B_interior)) then
           call make_IO_dir(this%B_interior,dir//'B_interior'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F1)) then
           call make_IO_dir(this%temp_F1,dir//'temp_F1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F2)) then
           call make_IO_dir(this%temp_F2,dir//'temp_F2'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%jCrossB)) then
           call make_IO_dir(this%jCrossB,dir//'jCrossB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Bstar)) then
           call make_IO_dir(this%Bstar,dir//'Bstar'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%dB0dt)) then
           call make_IO_dir(this%dB0dt,dir//'dB0dt'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%sigmaInv_edge)) then
           call make_IO_dir(this%sigmaInv_edge,&
           dir//'sigmaInv_edge'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%J_interior)) then
           call make_IO_dir(this%J_interior,dir//'J_interior'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%curlUCrossB)) then
           call make_IO_dir(this%curlUCrossB,dir//'curlUCrossB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%cell_inverse_area)) then
           call make_IO_dir(this%cell_inverse_area,&
           dir//'cell_inverse_area'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%CC_VF_fluid)) then
           call make_IO_dir(this%CC_VF_fluid,dir//'CC_VF_fluid'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%CC_VF_sigma)) then
           call make_IO_dir(this%CC_VF_sigma,dir//'CC_VF_sigma'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC_VF)) then
           call make_IO_dir(this%temp_CC_VF,dir//'temp_CC_VF'//fortran_PS)
         endif
         call make_IO_dir(this%MD_fluid,dir//'MD_fluid'//fortran_PS)
         call make_IO_dir(this%MD_sigma,dir//'MD_sigma'//fortran_PS)
       end subroutine

       subroutine export_folder_structure_induction(this,dir)
         implicit none
         type(induction),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         call export_structured(this%PCG_B,dir//'PCG_B'//fortran_PS)
         call export_structured(this%PCG_cleanB,&
         dir//'PCG_cleanB'//fortran_PS)
         call export_structured(this%FFT_cleanB,&
         dir//'FFT_cleanB'//fortran_PS)
         if (get_necessary_for_restart(this%sigmaInv_CC)) then
           call export_structured(this%sigmaInv_CC,&
           dir//'sigmaInv_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%cell_volume)) then
           call export_structured(this%cell_volume,&
           dir//'cell_volume'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divB)) then
           call export_structured(this%divB,dir//'divB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divJ)) then
           call export_structured(this%divJ,dir//'divJ'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%phi)) then
           call export_structured(this%phi,dir//'phi'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC)) then
           call export_structured(this%temp_CC,dir//'temp_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%F)) then
           call export_structured(this%F,dir//'F'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Fnm1)) then
           call export_structured(this%Fnm1,dir//'Fnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%L)) then
           call export_structured(this%L,dir//'L'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%J)) then
           call export_structured(this%J,dir//'J'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_E)) then
           call export_structured(this%temp_E,dir//'temp_E'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B)) then
           call export_structured(this%B,dir//'B'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Bnm1)) then
           call export_structured(this%Bnm1,dir//'Bnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B0)) then
           call export_structured(this%B0,dir//'B0'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Btot)) then
           call export_structured(this%Btot,dir//'Btot'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B_interior)) then
           call export_structured(this%B_interior,&
           dir//'B_interior'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F1)) then
           call export_structured(this%temp_F1,dir//'temp_F1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F2)) then
           call export_structured(this%temp_F2,dir//'temp_F2'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%jCrossB)) then
           call export_structured(this%jCrossB,dir//'jCrossB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Bstar)) then
           call export_structured(this%Bstar,dir//'Bstar'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%dB0dt)) then
           call export_structured(this%dB0dt,dir//'dB0dt'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%sigmaInv_edge)) then
           call export_structured(this%sigmaInv_edge,&
           dir//'sigmaInv_edge'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%J_interior)) then
           call export_structured(this%J_interior,&
           dir//'J_interior'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%curlUCrossB)) then
           call export_structured(this%curlUCrossB,&
           dir//'curlUCrossB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%cell_inverse_area)) then
           call export_structured(this%cell_inverse_area,&
           dir//'cell_inverse_area'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%CC_VF_fluid)) then
           call export_structured(this%CC_VF_fluid,&
           dir//'CC_VF_fluid'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%CC_VF_sigma)) then
           call export_structured(this%CC_VF_sigma,&
           dir//'CC_VF_sigma'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC_VF)) then
           call export_structured(this%temp_CC_VF,&
           dir//'temp_CC_VF'//fortran_PS)
         endif
         call export_structured(this%MD_fluid,dir//'MD_fluid'//fortran_PS)
         call export_structured(this%MD_sigma,dir//'MD_sigma'//fortran_PS)
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
         call export_structured(this%FFT_cleanB,&
         dir//'FFT_cleanB'//fortran_PS)
         if (get_necessary_for_restart(this%sigmaInv_CC)) then
           call export_structured(this%sigmaInv_CC,&
           dir//'sigmaInv_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%cell_volume)) then
           call export_structured(this%cell_volume,&
           dir//'cell_volume'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divB)) then
           call export_structured(this%divB,dir//'divB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divJ)) then
           call export_structured(this%divJ,dir//'divJ'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%phi)) then
           call export_structured(this%phi,dir//'phi'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC)) then
           call export_structured(this%temp_CC,dir//'temp_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%F)) then
           call export_structured(this%F,dir//'F'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Fnm1)) then
           call export_structured(this%Fnm1,dir//'Fnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%L)) then
           call export_structured(this%L,dir//'L'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%J)) then
           call export_structured(this%J,dir//'J'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_E)) then
           call export_structured(this%temp_E,dir//'temp_E'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B)) then
           call export_structured(this%B,dir//'B'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Bnm1)) then
           call export_structured(this%Bnm1,dir//'Bnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B0)) then
           call export_structured(this%B0,dir//'B0'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Btot)) then
           call export_structured(this%Btot,dir//'Btot'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B_interior)) then
           call export_structured(this%B_interior,&
           dir//'B_interior'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F1)) then
           call export_structured(this%temp_F1,dir//'temp_F1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F2)) then
           call export_structured(this%temp_F2,dir//'temp_F2'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%jCrossB)) then
           call export_structured(this%jCrossB,dir//'jCrossB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Bstar)) then
           call export_structured(this%Bstar,dir//'Bstar'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%dB0dt)) then
           call export_structured(this%dB0dt,dir//'dB0dt'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%sigmaInv_edge)) then
           call export_structured(this%sigmaInv_edge,&
           dir//'sigmaInv_edge'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%J_interior)) then
           call export_structured(this%J_interior,&
           dir//'J_interior'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%curlUCrossB)) then
           call export_structured(this%curlUCrossB,&
           dir//'curlUCrossB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%cell_inverse_area)) then
           call export_structured(this%cell_inverse_area,&
           dir//'cell_inverse_area'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%CC_VF_fluid)) then
           call export_structured(this%CC_VF_fluid,&
           dir//'CC_VF_fluid'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%CC_VF_sigma)) then
           call export_structured(this%CC_VF_sigma,&
           dir//'CC_VF_sigma'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC_VF)) then
           call export_structured(this%temp_CC_VF,&
           dir//'temp_CC_VF'//fortran_PS)
         endif
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
         call import_structured(this%FFT_cleanB,&
         dir//'FFT_cleanB'//fortran_PS)
         if (get_necessary_for_restart(this%sigmaInv_CC)) then
           call import_structured(this%sigmaInv_CC,&
           dir//'sigmaInv_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%cell_volume)) then
           call import_structured(this%cell_volume,&
           dir//'cell_volume'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divB)) then
           call import_structured(this%divB,dir//'divB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divJ)) then
           call import_structured(this%divJ,dir//'divJ'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%phi)) then
           call import_structured(this%phi,dir//'phi'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC)) then
           call import_structured(this%temp_CC,dir//'temp_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%F)) then
           call import_structured(this%F,dir//'F'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Fnm1)) then
           call import_structured(this%Fnm1,dir//'Fnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%L)) then
           call import_structured(this%L,dir//'L'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%J)) then
           call import_structured(this%J,dir//'J'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_E)) then
           call import_structured(this%temp_E,dir//'temp_E'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B)) then
           call import_structured(this%B,dir//'B'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Bnm1)) then
           call import_structured(this%Bnm1,dir//'Bnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B0)) then
           call import_structured(this%B0,dir//'B0'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Btot)) then
           call import_structured(this%Btot,dir//'Btot'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%B_interior)) then
           call import_structured(this%B_interior,&
           dir//'B_interior'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F1)) then
           call import_structured(this%temp_F1,dir//'temp_F1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F2)) then
           call import_structured(this%temp_F2,dir//'temp_F2'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%jCrossB)) then
           call import_structured(this%jCrossB,dir//'jCrossB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Bstar)) then
           call import_structured(this%Bstar,dir//'Bstar'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%dB0dt)) then
           call import_structured(this%dB0dt,dir//'dB0dt'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%sigmaInv_edge)) then
           call import_structured(this%sigmaInv_edge,&
           dir//'sigmaInv_edge'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%J_interior)) then
           call import_structured(this%J_interior,&
           dir//'J_interior'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%curlUCrossB)) then
           call import_structured(this%curlUCrossB,&
           dir//'curlUCrossB'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%cell_inverse_area)) then
           call import_structured(this%cell_inverse_area,&
           dir//'cell_inverse_area'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%CC_VF_fluid)) then
           call import_structured(this%CC_VF_fluid,&
           dir//'CC_VF_fluid'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%CC_VF_sigma)) then
           call import_structured(this%CC_VF_sigma,&
           dir//'CC_VF_sigma'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC_VF)) then
           call import_structured(this%temp_CC_VF,&
           dir//'temp_CC_VF'//fortran_PS)
         endif
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