       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_logicals_mod
       use IO_tools_mod
       implicit none

       private
       public :: export_logicals
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_ex;    end interface
       interface delete;       module procedure delete_ex;       end interface
       interface display;      module procedure display_ex;      end interface
       interface display_short;module procedure display_short_ex;end interface
       interface display;      module procedure display_wrap_ex; end interface
       interface print;        module procedure print_ex;        end interface
       interface print_short;  module procedure print_short_ex;  end interface
       interface export;       module procedure export_ex;       end interface
       interface import;       module procedure import_ex;       end interface
       interface export;       module procedure export_wrap_ex;  end interface
       interface import;       module procedure import_wrap_ex;  end interface

       type export_logicals
         logical :: export_analytic = .false.
         logical :: export_meshes = .false.
         logical :: export_vort_SF = .false.
         logical :: export_mat_props = .false.
         logical :: export_cell_volume = .false.
         logical :: export_ICs = .false.
         logical :: export_planar = .false.
         logical :: export_symmetric = .false.
         logical :: export_mesh_block = .false.
         logical :: export_soln_only = .false.
         logical :: defined = .false.
       end type

       contains

       subroutine init_copy_ex(this,that)
         implicit none
         type(export_logicals),intent(inout) :: this
         type(export_logicals),intent(in) :: that
         call delete(this)
         this%export_analytic = that%export_analytic
         this%export_meshes = that%export_meshes
         this%export_vort_SF = that%export_vort_SF
         this%export_mat_props = that%export_mat_props
         this%export_cell_volume = that%export_cell_volume
         this%export_ICs = that%export_ICs
         this%export_planar = that%export_planar
         this%export_symmetric = that%export_symmetric
         this%export_mesh_block = that%export_mesh_block
         this%export_soln_only = that%export_soln_only
         this%defined = that%defined
       end subroutine

       subroutine delete_ex(this)
         implicit none
         type(export_logicals),intent(inout) :: this
         this%export_analytic = .false.
         this%export_meshes = .false.
         this%export_vort_SF = .false.
         this%export_mat_props = .false.
         this%export_cell_volume = .false.
         this%export_ICs = .false.
         this%export_planar = .false.
         this%export_symmetric = .false.
         this%export_mesh_block = .false.
         this%export_soln_only = .false.
         this%defined = .false.
       end subroutine

       subroutine display_ex(this,un)
         implicit none
         type(export_logicals),intent(in) :: this
         integer,intent(in) :: un
       end subroutine

       subroutine display_short_ex(this,un)
         implicit none
         type(export_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_analytic    = ',this%export_analytic
         write(un,*) 'export_meshes      = ',this%export_meshes
         write(un,*) 'export_vort_SF     = ',this%export_vort_SF
         write(un,*) 'export_mat_props   = ',this%export_mat_props
         write(un,*) 'export_cell_volume = ',this%export_cell_volume
         write(un,*) 'export_ICs         = ',this%export_ICs
         write(un,*) 'export_planar      = ',this%export_planar
         write(un,*) 'export_symmetric   = ',this%export_symmetric
         write(un,*) 'export_mesh_block  = ',this%export_mesh_block
         write(un,*) 'export_soln_only   = ',this%export_soln_only
         write(un,*) 'defined            = ',this%defined
       end subroutine

       subroutine print_ex(this)
         implicit none
         type(export_logicals),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_ex(this)
         implicit none
         type(export_logicals),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_ex(this,un)
         implicit none
         type(export_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_analytic     = ';write(un,*) this%export_analytic
         write(un,*) 'export_meshes       = ';write(un,*) this%export_meshes
         write(un,*) 'export_vort_SF      = ';write(un,*) this%export_vort_SF
         write(un,*) 'export_mat_props    = ';write(un,*) this%export_mat_props
         write(un,*) 'export_cell_volume  = ';write(un,*) this%export_cell_volume
         write(un,*) 'export_ICs          = ';write(un,*) this%export_ICs
         write(un,*) 'export_planar       = ';write(un,*) this%export_planar
         write(un,*) 'export_symmetric    = ';write(un,*) this%export_symmetric
         write(un,*) 'export_mesh_block   = ';write(un,*) this%export_mesh_block
         write(un,*) 'export_soln_only    = ';write(un,*) this%export_soln_only
         write(un,*) 'defined             = ';write(un,*) this%defined
       end subroutine

       subroutine import_ex(this,un)
         implicit none
         type(export_logicals),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%export_analytic
         read(un,*); read(un,*) this%export_meshes
         read(un,*); read(un,*) this%export_vort_SF
         read(un,*); read(un,*) this%export_mat_props
         read(un,*); read(un,*) this%export_cell_volume
         read(un,*); read(un,*) this%export_ICs
         read(un,*); read(un,*) this%export_planar
         read(un,*); read(un,*) this%export_symmetric
         read(un,*); read(un,*) this%export_mesh_block
         read(un,*); read(un,*) this%export_soln_only
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine display_wrap_ex(this,dir,name)
         implicit none
         type(export_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_ex(this,dir,name)
         implicit none
         type(export_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_ex(this,dir,name)
         implicit none
         type(export_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module