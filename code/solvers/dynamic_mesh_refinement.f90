     module dynamic_mesh_refinement_mod
     use current_precision_mod
     use IO_tools_mod
     implicit none

     private
     public :: dynamic_mesh_refinement
     public :: init,delete,display,print,export,import

     type dynamic_mesh_refinement
       logical :: dynamic_refinement = .false.
       integer :: n_max_refinements = 0
       integer :: n_history = 0
       real(cp) :: SS_tol = 0.0_cp
       real(cp) :: SS_tol_final = 0.0_cp
       real(cp) :: dt_reduction_factor = 0.0_cp
     end type

     interface init;    module procedure init_DMR;      end interface
     interface init;    module procedure init_DMR_copy; end interface
     interface delete;  module procedure delete_DMR;    end interface
     interface display; module procedure display_DMR;   end interface
     interface print;   module procedure print_DMR;     end interface
     interface export;  module procedure export_DMR;    end interface
     interface import;  module procedure import_DMR;    end interface

     contains

     subroutine init_DMR(DMR,dynamic_refinement,&
       n_max_refinements,n_history,SS_tol,SS_tol_final,dt_reduction_factor)
       implicit none
       type(dynamic_mesh_refinement),intent(inout) :: DMR
       logical,intent(in) :: dynamic_refinement
       integer,intent(in) :: n_max_refinements,n_history
       real(cp),intent(in) :: SS_tol,SS_tol_final,dt_reduction_factor
       DMR%dynamic_refinement     = dynamic_refinement
       DMR%n_max_refinements      = n_max_refinements
       DMR%n_history              = n_history
       DMR%SS_tol                 = SS_tol
       DMR%SS_tol_final           = SS_tol_final
       DMR%dt_reduction_factor    = dt_reduction_factor
      end subroutine

     subroutine init_DMR_copy(DMR,DMR_in)
       implicit none
       type(dynamic_mesh_refinement),intent(inout) :: DMR
       type(dynamic_mesh_refinement),intent(in) :: DMR_in
       DMR%dynamic_refinement     = DMR_in%dynamic_refinement
       DMR%n_max_refinements      = DMR_in%n_max_refinements
       DMR%n_history              = DMR_in%n_history
       DMR%SS_tol                 = DMR_in%SS_tol
       DMR%SS_tol_final           = DMR_in%SS_tol_final
       DMR%dt_reduction_factor    = DMR_in%dt_reduction_factor
      end subroutine

     subroutine delete_DMR(DMR)
       implicit none
       type(dynamic_mesh_refinement),intent(inout) :: DMR
       DMR%dynamic_refinement     = .true.
       DMR%n_max_refinements      = 0
       DMR%n_history              = 0
       DMR%SS_tol                 = 0.0_cp
       DMR%SS_tol_final           = 0.0_cp
       DMR%dt_reduction_factor    = 0.0_cp
      end subroutine

     subroutine display_DMR(DMR,un)
       implicit none
       type(dynamic_mesh_refinement),intent(in) :: DMR
       integer,intent(in) :: un
       write(un,*) 'dynamic_refinement=',DMR%dynamic_refinement
       write(un,*) 'n_max_refinements=',DMR%n_max_refinements
       write(un,*) 'n_history=',DMR%n_history
       write(un,*) 'SS_tol=',DMR%SS_tol
       write(un,*) 'SS_tol_final=',DMR%SS_tol_final
       write(un,*) 'dt_reduction_factor=',DMR%dt_reduction_factor
      end subroutine

     subroutine print_DMR(DMR)
       implicit none
       type(dynamic_mesh_refinement),intent(in) :: DMR
       call display(DMR,6)
      end subroutine

     subroutine export_DMR(DMR,un)
       implicit none
       type(dynamic_mesh_refinement),intent(in) :: DMR
       integer,intent(in) :: un
       write(un,*) 'dynamic_refinement='; write(un,*) DMR%dynamic_refinement
       write(un,*) 'n_max_refinements=';  write(un,*) DMR%n_max_refinements
       write(un,*) 'n_history=';          write(un,*) DMR%n_history
       write(un,*) 'SS_tol=';             write(un,*) DMR%SS_tol
       write(un,*) 'SS_tol_final=';       write(un,*) DMR%SS_tol_final
       write(un,*) 'dt_reduction_factor=';write(un,*) DMR%dt_reduction_factor
      end subroutine

     subroutine import_DMR(DMR,un)
       implicit none
       type(dynamic_mesh_refinement),intent(inout) :: DMR
       integer,intent(in) :: un
       read(un,*); read(un,*) DMR%dynamic_refinement
       read(un,*); read(un,*) DMR%n_max_refinements
       read(un,*); read(un,*) DMR%n_history
       read(un,*); read(un,*) DMR%SS_tol
       read(un,*); read(un,*) DMR%SS_tol_final
       read(un,*); read(un,*) DMR%dt_reduction_factor
      end subroutine

     end module