       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module induction_terms_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use equation_term_mod
       use string_mod
       use dir_manip_mod
       implicit none

       private
       public :: induction_terms
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_induction_terms;              end interface
       interface delete;                 module procedure delete_induction_terms;                 end interface
       interface display;                module procedure display_induction_terms;                end interface
       interface display_short;          module procedure display_short_induction_terms;          end interface
       interface display;                module procedure display_wrap_induction_terms;           end interface
       interface print;                  module procedure print_induction_terms;                  end interface
       interface print_short;            module procedure print_short_induction_terms;            end interface
       interface export;                 module procedure export_induction_terms;                 end interface
       interface export_primitives;      module procedure export_primitives_induction_terms;      end interface
       interface import;                 module procedure import_induction_terms;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_induction_terms;end interface
       interface export_structured;      module procedure export_structured_D_induction_terms;    end interface
       interface import_structured;      module procedure import_structured_D_induction_terms;    end interface
       interface import_primitives;      module procedure import_primitives_induction_terms;      end interface
       interface export;                 module procedure export_wrap_induction_terms;            end interface
       interface import;                 module procedure import_wrap_induction_terms;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_induction_terms;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_induction_terms;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_induction_terms;      end interface

       type induction_terms
         type(equation_term) :: advection
         type(equation_term) :: diffusion
         type(equation_term) :: diffusion_linear
         type(equation_term) :: unsteady_B0
         type(equation_term) :: constant_dB0dt
         type(equation_term) :: current
         type(equation_term) :: B_applied
         logical,dimension(3) :: zero_source_components = .false.
         logical :: zero_source_components_any = .false.
       end type

       contains

       subroutine init_copy_induction_terms(this,that)
         implicit none
         type(induction_terms),intent(inout) :: this
         type(induction_terms),intent(in) :: that
         call delete(this)
         call init(this%advection,that%advection)
         call init(this%diffusion,that%diffusion)
         call init(this%diffusion_linear,that%diffusion_linear)
         call init(this%unsteady_B0,that%unsteady_B0)
         call init(this%constant_dB0dt,that%constant_dB0dt)
         call init(this%current,that%current)
         call init(this%B_applied,that%B_applied)
         this%zero_source_components = that%zero_source_components
         this%zero_source_components_any = that%zero_source_components_any
       end subroutine

       subroutine delete_induction_terms(this)
         implicit none
         type(induction_terms),intent(inout) :: this
         call delete(this%advection)
         call delete(this%diffusion)
         call delete(this%diffusion_linear)
         call delete(this%unsteady_B0)
         call delete(this%constant_dB0dt)
         call delete(this%current)
         call delete(this%B_applied)
         this%zero_source_components = .false.
         this%zero_source_components_any = .false.
       end subroutine

       subroutine display_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%diffusion_linear,un)
         call display(this%unsteady_B0,un)
         call display(this%constant_dB0dt,un)
         call display(this%current,un)
         call display(this%B_applied,un)
         write(un,*) 'zero_source_components     = ',&
         this%zero_source_components
         write(un,*) 'zero_source_components_any = ',&
         this%zero_source_components_any
       end subroutine

       subroutine display_short_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%diffusion_linear,un)
         call display(this%unsteady_B0,un)
         call display(this%constant_dB0dt,un)
         call display(this%current,un)
         call display(this%B_applied,un)
         write(un,*) 'zero_source_components     = ',&
         this%zero_source_components
         write(un,*) 'zero_source_components_any = ',&
         this%zero_source_components_any
       end subroutine

       subroutine display_wrap_induction_terms(this,dir,name)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_induction_terms(this)
         implicit none
         type(induction_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_induction_terms(this)
         implicit none
         type(induction_terms),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
         call export(this%advection,un)
         call export(this%diffusion,un)
         call export(this%diffusion_linear,un)
         call export(this%unsteady_B0,un)
         call export(this%constant_dB0dt,un)
         call export(this%current,un)
         call export(this%B_applied,un)
       end subroutine

       subroutine import_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
         call import(this%advection,un)
         call import(this%diffusion,un)
         call import(this%diffusion_linear,un)
         call import(this%unsteady_B0,un)
         call import(this%constant_dB0dt,un)
         call import(this%current,un)
         call import(this%B_applied,un)
       end subroutine

       subroutine export_primitives_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'zero_source_components      = ';write(un,*) this%zero_source_components
         write(un,*) 'zero_source_components_any  = ';write(un,*) this%zero_source_components_any
       end subroutine

       subroutine import_primitives_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%zero_source_components
         read(un,*); read(un,*) this%zero_source_components_any
       end subroutine

       subroutine export_wrap_induction_terms(this,dir,name)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_induction_terms(this,dir,name)
         implicit none
         type(induction_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_induction_terms(this,dir)
         implicit none
         type(induction_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%advection,dir//'advection'//fortran_PS)
         call set_IO_dir(this%diffusion,dir//'diffusion'//fortran_PS)
         call set_IO_dir(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call set_IO_dir(this%unsteady_B0,dir//'unsteady_B0'//fortran_PS)
         call set_IO_dir(this%constant_dB0dt,&
         dir//'constant_dB0dt'//fortran_PS)
         call set_IO_dir(this%current,dir//'current'//fortran_PS)
         call set_IO_dir(this%B_applied,dir//'B_applied'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_induction_terms(this,dir)
         implicit none
         type(induction_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%advection,dir//'advection'//fortran_PS)
         call make_IO_dir(this%diffusion,dir//'diffusion'//fortran_PS)
         call make_IO_dir(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call make_IO_dir(this%unsteady_B0,dir//'unsteady_B0'//fortran_PS)
         call make_IO_dir(this%constant_dB0dt,&
         dir//'constant_dB0dt'//fortran_PS)
         call make_IO_dir(this%current,dir//'current'//fortran_PS)
         call make_IO_dir(this%B_applied,dir//'B_applied'//fortran_PS)
       end subroutine

       subroutine export_folder_structure_induction_terms(this,dir)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         call export_structured(this%advection,dir//'advection'//fortran_PS)
         call export_structured(this%diffusion,dir//'diffusion'//fortran_PS)
         call export_structured(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call export_structured(this%unsteady_B0,&
         dir//'unsteady_B0'//fortran_PS)
         call export_structured(this%constant_dB0dt,&
         dir//'constant_dB0dt'//fortran_PS)
         call export_structured(this%current,dir//'current'//fortran_PS)
         call export_structured(this%B_applied,dir//'B_applied'//fortran_PS)
       end subroutine

       subroutine export_structured_D_induction_terms(this,dir)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%advection,dir//'advection'//fortran_PS)
         call export_structured(this%diffusion,dir//'diffusion'//fortran_PS)
         call export_structured(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call export_structured(this%unsteady_B0,&
         dir//'unsteady_B0'//fortran_PS)
         call export_structured(this%constant_dB0dt,&
         dir//'constant_dB0dt'//fortran_PS)
         call export_structured(this%current,dir//'current'//fortran_PS)
         call export_structured(this%B_applied,dir//'B_applied'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_induction_terms(this,dir)
         implicit none
         type(induction_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%advection,dir//'advection'//fortran_PS)
         call import_structured(this%diffusion,dir//'diffusion'//fortran_PS)
         call import_structured(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call import_structured(this%unsteady_B0,&
         dir//'unsteady_B0'//fortran_PS)
         call import_structured(this%constant_dB0dt,&
         dir//'constant_dB0dt'//fortran_PS)
         call import_structured(this%current,dir//'current'//fortran_PS)
         call import_structured(this%B_applied,dir//'B_applied'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_induction_terms(this)
         implicit none
         type(induction_terms),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module