     module export_logicals_mod
     implicit none

     private
     public :: export_logicals
     public :: init,delete,display,print,export,import

     type export_logicals
       logical :: export_analytic = .false.
       logical :: export_meshes = .false.
       logical :: export_mat_props = .false.
       logical :: export_cell_volume = .false.
       logical :: export_ICs = .false.
       logical :: export_planar = .false.
       logical :: export_symmetric = .false.
       logical :: export_mesh_block = .false.
       logical :: export_soln_only = .false.
     end type

     interface init;    module procedure init_copy_EL; end interface
     interface delete;  module procedure delete_EL;    end interface
     interface display; module procedure display_EL;   end interface
     interface print;   module procedure print_EL;     end interface
     interface export;  module procedure export_EL;    end interface
     interface import;  module procedure import_EL;    end interface

     contains

     subroutine init_copy_EL(EL,EL_in)
       implicit none
       type(export_logicals),intent(inout) :: EL
       type(export_logicals),intent(in) :: EL_in
       EL%export_analytic        = EL_in%export_analytic
       EL%export_meshes          = EL_in%export_meshes
       EL%export_mat_props       = EL_in%export_mat_props
       EL%export_ICs             = EL_in%export_ICs
       EL%export_cell_volume     = EL_in%export_cell_volume
       EL%export_planar          = EL_in%export_planar
       EL%export_symmetric       = EL_in%export_symmetric
       EL%export_mesh_block      = EL_in%export_mesh_block
       EL%export_soln_only       = EL_in%export_soln_only
     end subroutine

     subroutine delete_EL(EL)
       implicit none
       type(export_logicals),intent(inout) :: EL
       EL%export_analytic        = .false.
       EL%export_meshes          = .false.
       EL%export_mat_props       = .false.
       EL%export_ICs             = .false.
       EL%export_cell_volume     = .false.
       EL%export_planar          = .false.
       EL%export_symmetric       = .false.
       EL%export_mesh_block      = .false.
       EL%export_soln_only       = .false.
     end subroutine

     subroutine display_EL(EL,un)
       implicit none
       type(export_logicals),intent(in) :: EL
       integer,intent(in) :: un
       write(un,*) 'export_analytic    = ',EL%export_analytic
       write(un,*) 'export_meshes      = ',EL%export_meshes
       write(un,*) 'export_mat_props   = ',EL%export_mat_props
       write(un,*) 'export_ICs         = ',EL%export_ICs
       write(un,*) 'export_cell_volume = ',EL%export_cell_volume
       write(un,*) 'export_planar      = ',EL%export_planar
       write(un,*) 'export_symmetric   = ',EL%export_symmetric
       write(un,*) 'export_mesh_block  = ',EL%export_mesh_block
       write(un,*) 'export_soln_only   = ',EL%export_soln_only
     end subroutine

     subroutine print_EL(EL)
       implicit none
       type(export_logicals),intent(in) :: EL
       call display(EL,6)
     end subroutine

     subroutine export_EL(EL,un)
       implicit none
       type(export_logicals),intent(in) :: EL
       integer,intent(in) :: un
       write(un,*) EL%export_analytic
       write(un,*) EL%export_meshes
       write(un,*) EL%export_mat_props
       write(un,*) EL%export_ICs
       write(un,*) EL%export_cell_volume
       write(un,*) EL%export_planar
       write(un,*) EL%export_symmetric
       write(un,*) EL%export_mesh_block
       write(un,*) EL%export_soln_only
     end subroutine

     subroutine import_EL(EL,un)
       implicit none
       type(export_logicals),intent(inout) :: EL
       integer,intent(in) :: un
       read(un,*) EL%export_analytic
       read(un,*) EL%export_meshes
       read(un,*) EL%export_mat_props
       read(un,*) EL%export_ICs
       read(un,*) EL%export_cell_volume
       read(un,*) EL%export_planar
       read(un,*) EL%export_symmetric
       read(un,*) EL%export_mesh_block
       read(un,*) EL%export_soln_only
     end subroutine

     end module