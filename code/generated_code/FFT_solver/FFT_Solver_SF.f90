       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module FFT_Solver_SF_mod
       use IO_tools_mod
       use norms_mod
       use SF_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use norms_mod
       use string_mod
       implicit none

       private
       public :: FFT_Solver_SF
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings

       interface init;             module procedure init_copy_FFT_Solver_SF;          end interface
       interface delete;           module procedure delete_FFT_Solver_SF;             end interface
       interface display;          module procedure display_FFT_Solver_SF;            end interface
       interface display_short;    module procedure display_short_FFT_Solver_SF;      end interface
       interface display;          module procedure display_wrap_FFT_Solver_SF;       end interface
       interface print;            module procedure print_FFT_Solver_SF;              end interface
       interface print_short;      module procedure print_short_FFT_Solver_SF;        end interface
       interface export;           module procedure export_FFT_Solver_SF;             end interface
       interface export_primitives;module procedure export_primitives_FFT_Solver_SF;  end interface
       interface import;           module procedure import_FFT_Solver_SF;             end interface
       interface export_structured;module procedure export_structured_D_FFT_Solver_SF;end interface
       interface import_structured;module procedure import_structured_D_FFT_Solver_SF;end interface
       interface import_primitives;module procedure import_primitives_FFT_Solver_SF;  end interface
       interface export;           module procedure export_wrap_FFT_Solver_SF;        end interface
       interface import;           module procedure import_wrap_FFT_Solver_SF;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_FFT_Solver_SF;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_FFT_Solver_SF;        end interface
       interface suppress_warnings;module procedure suppress_warnings_FFT_Solver_SF;  end interface

       type FFT_Solver_SF
         type(SF) :: f
         type(SF) :: res
         type(SF) :: vol
         type(SF) :: coeff
         integer :: direction = 0
         type(norms) :: norm
         type(string) :: var_name
       end type

       contains

       subroutine init_copy_FFT_Solver_SF(this,that)
         implicit none
         type(FFT_Solver_SF),intent(inout) :: this
         type(FFT_Solver_SF),intent(in) :: that
         call delete(this)
         call init(this%f,that%f)
         call init(this%res,that%res)
         call init(this%vol,that%vol)
         call init(this%coeff,that%coeff)
         this%direction = that%direction
         call init(this%norm,that%norm)
         call init(this%var_name,that%var_name)
       end subroutine

       subroutine delete_FFT_Solver_SF(this)
         implicit none
         type(FFT_Solver_SF),intent(inout) :: this
         call delete(this%f)
         call delete(this%res)
         call delete(this%vol)
         call delete(this%coeff)
         this%direction = 0
         call delete(this%norm)
         call delete(this%var_name)
       end subroutine

       subroutine display_FFT_Solver_SF(this,un)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%f,un)
         call display(this%res,un)
         call display(this%vol,un)
         call display(this%coeff,un)
         write(un,*) 'direction = ',this%direction
         call display(this%norm,un)
         call display(this%var_name,un)
       end subroutine

       subroutine display_short_FFT_Solver_SF(this,un)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%f,un)
         call display(this%res,un)
         call display(this%vol,un)
         call display(this%coeff,un)
         write(un,*) 'direction = ',this%direction
         call display(this%norm,un)
         call display(this%var_name,un)
       end subroutine

       subroutine display_wrap_FFT_Solver_SF(this,dir,name)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_FFT_Solver_SF(this)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_FFT_Solver_SF(this)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_FFT_Solver_SF(this,un)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
         call export(this%f,un)
         call export(this%res,un)
         call export(this%vol,un)
         call export(this%coeff,un)
         call export(this%norm,un)
         call export(this%var_name,un)
       end subroutine

       subroutine import_FFT_Solver_SF(this,un)
         implicit none
         type(FFT_Solver_SF),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
         call import(this%f,un)
         call import(this%res,un)
         call import(this%vol,un)
         call import(this%coeff,un)
         call import(this%norm,un)
         call import(this%var_name,un)
       end subroutine

       subroutine export_primitives_FFT_Solver_SF(this,un)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'direction  = ';write(un,*) this%direction
       end subroutine

       subroutine import_primitives_FFT_Solver_SF(this,un)
         implicit none
         type(FFT_Solver_SF),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%direction
       end subroutine

       subroutine export_wrap_FFT_Solver_SF(this,dir,name)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_FFT_Solver_SF(this,dir,name)
         implicit none
         type(FFT_Solver_SF),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_FFT_Solver_SF(this,dir)
         implicit none
         type(FFT_Solver_SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%f,dir//'f'//fortran_PS)
         call set_IO_dir(this%res,dir//'res'//fortran_PS)
         call set_IO_dir(this%vol,dir//'vol'//fortran_PS)
         call set_IO_dir(this%coeff,dir//'coeff'//fortran_PS)
         call set_IO_dir(this%norm,dir//'norm'//fortran_PS)
         call set_IO_dir(this%var_name,dir//'var_name'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_FFT_Solver_SF(this,dir)
         implicit none
         type(FFT_Solver_SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         if (get_necessary_for_restart(this%f)) then
           call make_IO_dir(this%f,dir//'f'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%res)) then
           call make_IO_dir(this%res,dir//'res'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%vol)) then
           call make_IO_dir(this%vol,dir//'vol'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%coeff)) then
           call make_IO_dir(this%coeff,dir//'coeff'//fortran_PS)
         endif
         call make_IO_dir(this%norm,dir//'norm'//fortran_PS)
         call make_IO_dir(this%var_name,dir//'var_name'//fortran_PS)
       end subroutine

       subroutine export_structured_D_FFT_Solver_SF(this,dir)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         if (get_necessary_for_restart(this%f)) then
           call export_structured(this%f,dir//'f'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%res)) then
           call export_structured(this%res,dir//'res'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%vol)) then
           call export_structured(this%vol,dir//'vol'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%coeff)) then
           call export_structured(this%coeff,dir//'coeff'//fortran_PS)
         endif
         call export_structured(this%norm,dir//'norm'//fortran_PS)
         call export_structured(this%var_name,dir//'var_name'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_FFT_Solver_SF(this,dir)
         implicit none
         type(FFT_Solver_SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         if (get_necessary_for_restart(this%f)) then
           call import_structured(this%f,dir//'f'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%res)) then
           call import_structured(this%res,dir//'res'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%vol)) then
           call import_structured(this%vol,dir//'vol'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%coeff)) then
           call import_structured(this%coeff,dir//'coeff'//fortran_PS)
         endif
         call import_structured(this%norm,dir//'norm'//fortran_PS)
         call import_structured(this%var_name,dir//'var_name'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_FFT_Solver_SF(this)
         implicit none
         type(FFT_Solver_SF),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module