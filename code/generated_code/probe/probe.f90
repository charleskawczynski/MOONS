       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module probe_mod
       use current_precision_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use string_mod
       use dir_manip_mod
       implicit none

       private
       public :: probe
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings,export,import,&
       export_structured,import_structured

       interface init;                   module procedure init_copy_probe;              end interface
       interface delete;                 module procedure delete_probe;                 end interface
       interface display;                module procedure display_probe;                end interface
       interface display_short;          module procedure display_short_probe;          end interface
       interface display;                module procedure display_wrap_probe;           end interface
       interface print;                  module procedure print_probe;                  end interface
       interface print_short;            module procedure print_short_probe;            end interface
       interface export;                 module procedure export_probe;                 end interface
       interface export_primitives;      module procedure export_primitives_probe;      end interface
       interface import;                 module procedure import_probe;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_probe;end interface
       interface export_structured;      module procedure export_structured_D_probe;    end interface
       interface import_structured;      module procedure import_structured_D_probe;    end interface
       interface import_primitives;      module procedure import_primitives_probe;      end interface
       interface export;                 module procedure export_wrap_probe;            end interface
       interface import;                 module procedure import_wrap_probe;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_probe;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_probe;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_probe;      end interface
       interface export;                 module procedure export_DN_probe;              end interface
       interface import;                 module procedure import_DN_probe;              end interface
       interface export_structured;      module procedure export_structured_DN_probe;   end interface
       interface import_structured;      module procedure import_structured_DN_probe;   end interface

       type probe
         type(string) :: tec_dir
         type(string) :: tec_name
         real(cp) :: d = 0.0_cp
         real(cp) :: d_data_dt = 0.0_cp
         real(cp) :: d_amax = 0.0_cp
         real(cp) :: t = 0.0_cp
         real(cp) :: abs_d_data_dt = 0.0_cp
         real(cp) :: abs_d_data_dt_by_dmax = 0.0_cp
         integer :: un = 0
         integer :: cols = 0
         integer(li) :: n_step = 0
         logical :: restart = .false.
         logical :: simple = .false.
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_probe(this,that)
         implicit none
         type(probe),intent(inout) :: this
         type(probe),intent(in) :: that
         call delete(this)
         call init(this%tec_dir,that%tec_dir)
         call init(this%tec_name,that%tec_name)
         this%d = that%d
         this%d_data_dt = that%d_data_dt
         this%d_amax = that%d_amax
         this%t = that%t
         this%abs_d_data_dt = that%abs_d_data_dt
         this%abs_d_data_dt_by_dmax = that%abs_d_data_dt_by_dmax
         this%un = that%un
         this%cols = that%cols
         this%n_step = that%n_step
         this%restart = that%restart
         this%simple = that%simple
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_probe(this)
         implicit none
         type(probe),intent(inout) :: this
         call delete(this%tec_dir)
         call delete(this%tec_name)
         this%d = 0.0_cp
         this%d_data_dt = 0.0_cp
         this%d_amax = 0.0_cp
         this%t = 0.0_cp
         this%abs_d_data_dt = 0.0_cp
         this%abs_d_data_dt_by_dmax = 0.0_cp
         this%un = 0
         this%cols = 0
         this%n_step = 0
         this%restart = .false.
         this%simple = .false.
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_probe(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         call display(this%tec_dir,un)
         call display(this%tec_name,un)
         write(un,*) 'd                     = ',this%d
         write(un,*) 'd_data_dt             = ',this%d_data_dt
         write(un,*) 'd_amax                = ',this%d_amax
         write(un,*) 't                     = ',this%t
         write(un,*) 'abs_d_data_dt         = ',this%abs_d_data_dt
         write(un,*) 'abs_d_data_dt_by_dmax = ',this%abs_d_data_dt_by_dmax
         write(un,*) 'un                    = ',this%un
         write(un,*) 'cols                  = ',this%cols
         write(un,*) 'n_step                = ',this%n_step
         write(un,*) 'restart               = ',this%restart
         write(un,*) 'simple                = ',this%simple
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_probe(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         call display(this%tec_dir,un)
         call display(this%tec_name,un)
         write(un,*) 'd                     = ',this%d
         write(un,*) 'd_data_dt             = ',this%d_data_dt
         write(un,*) 'd_amax                = ',this%d_amax
         write(un,*) 't                     = ',this%t
         write(un,*) 'abs_d_data_dt         = ',this%abs_d_data_dt
         write(un,*) 'abs_d_data_dt_by_dmax = ',this%abs_d_data_dt_by_dmax
         write(un,*) 'un                    = ',this%un
         write(un,*) 'cols                  = ',this%cols
         write(un,*) 'n_step                = ',this%n_step
         write(un,*) 'restart               = ',this%restart
         write(un,*) 'simple                = ',this%simple
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_wrap_probe(this,dir,name)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_probe(this)
         implicit none
         type(probe),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_probe(this)
         implicit none
         type(probe),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_probe(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
         call export(this%tec_dir,un)
         call export(this%tec_name,un)
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_probe(this,un)
         implicit none
         type(probe),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
         call import(this%tec_dir,un)
         call import(this%tec_name,un)
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine export_primitives_probe(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'd                      = ';write(un,*) this%d
         write(un,*) 'd_data_dt              = ';write(un,*) this%d_data_dt
         write(un,*) 'd_amax                 = ';write(un,*) this%d_amax
         write(un,*) 't                      = ';write(un,*) this%t
         write(un,*) 'abs_d_data_dt          = ';write(un,*) this%abs_d_data_dt
         write(un,*) 'abs_d_data_dt_by_dmax  = ';write(un,*) this%abs_d_data_dt_by_dmax
         write(un,*) 'un                     = ';write(un,*) this%un
         write(un,*) 'cols                   = ';write(un,*) this%cols
         write(un,*) 'n_step                 = ';write(un,*) this%n_step
         write(un,*) 'restart                = ';write(un,*) this%restart
         write(un,*) 'simple                 = ';write(un,*) this%simple
       end subroutine

       subroutine import_primitives_probe(this,un)
         implicit none
         type(probe),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%d
         read(un,*); read(un,*) this%d_data_dt
         read(un,*); read(un,*) this%d_amax
         read(un,*); read(un,*) this%t
         read(un,*); read(un,*) this%abs_d_data_dt
         read(un,*); read(un,*) this%abs_d_data_dt_by_dmax
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%cols
         read(un,*); read(un,*) this%n_step
         read(un,*); read(un,*) this%restart
         read(un,*); read(un,*) this%simple
       end subroutine

       subroutine export_wrap_probe(this,dir,name)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_probe(this,dir,name)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine export_DN_probe(this)
         implicit none
         type(probe),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_probe(this)
         implicit none
         type(probe),intent(inout) :: this
         type(string) :: dir,name
         integer :: un
         call init(dir,this%dir)
         call init(name,this%name)
         un = open_to_read(str(dir),str(name))
         call import(this,un)
         call delete(dir)
         call delete(name)
         close(un)
       end subroutine

       subroutine export_structured_DN_probe(this)
         implicit none
         type(probe),intent(in) :: this
         integer :: un
         un = new_and_open(str(this%dir),'primitives')
         call export_primitives(this,un)
         call export_structured(this%tec_dir,&
         str(this%dir)//'tec_dir'//fortran_PS)
         call export_structured(this%tec_name,&
         str(this%dir)//'tec_name'//fortran_PS)
         call export_structured(this%dir,str(this%dir)//'dir'//fortran_PS)
         call export_structured(this%name,str(this%dir)//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_DN_probe(this)
         implicit none
         type(probe),intent(inout) :: this
         integer :: un
         un = open_to_read(str(this%dir),'primitives')
         call import_primitives(this,un)
         call import_structured(this%tec_dir,&
         str(this%dir)//'tec_dir'//fortran_PS)
         call import_structured(this%tec_name,&
         str(this%dir)//'tec_name'//fortran_PS)
         call import_structured(this%dir,str(this%dir)//'dir'//fortran_PS)
         call import_structured(this%name,str(this%dir)//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine set_IO_dir_probe(this,dir)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call set_IO_dir(this%tec_dir,dir//'tec_dir'//fortran_PS)
         call set_IO_dir(this%tec_name,dir//'tec_name'//fortran_PS)
         call set_IO_dir(this%dir,dir//'dir'//fortran_PS)
         call set_IO_dir(this%name,dir//'name'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_probe(this,dir)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call make_IO_dir(this%tec_dir,dir//'tec_dir'//fortran_PS)
         call make_IO_dir(this%tec_name,dir//'tec_name'//fortran_PS)
         call make_IO_dir(this%dir,dir//'dir'//fortran_PS)
         call make_IO_dir(this%name,dir//'name'//fortran_PS)
       end subroutine

       subroutine export_folder_structure_probe(this,dir)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         call export_structured(this%tec_dir,dir//'tec_dir'//fortran_PS)
         call export_structured(this%tec_name,dir//'tec_name'//fortran_PS)
         call export_structured(this%dir,dir//'dir'//fortran_PS)
         call export_structured(this%name,dir//'name'//fortran_PS)
       end subroutine

       subroutine export_structured_D_probe(this,dir)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%tec_dir,dir//'tec_dir'//fortran_PS)
         call export_structured(this%tec_name,dir//'tec_name'//fortran_PS)
         call export_structured(this%dir,dir//'dir'//fortran_PS)
         call export_structured(this%name,dir//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_probe(this,dir)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%tec_dir,dir//'tec_dir'//fortran_PS)
         call import_structured(this%tec_name,dir//'tec_name'//fortran_PS)
         call import_structured(this%dir,dir//'dir'//fortran_PS)
         call import_structured(this%name,dir//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_probe(this)
         implicit none
         type(probe),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module