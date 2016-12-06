       module dynamic_refine_mesh_mod
       use current_precision_mod
       use sim_params_mod
       use VF_mod
       use IO_tools_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use stop_clock_mod
       use print_export_mod
       use export_now_mod
       use refine_mesh_mod
       use kill_switch_mod
       use probe_mod

       use time_marching_params_mod
       use energy_mod
       use momentum_mod
       use induction_mod
       use energy_aux_mod
       use induction_aux_mod
       implicit none

       private
       public :: dynamic_refine_mesh

       contains

       subroutine dynamic_refine_mesh(nrg,mom,ind,DT,SP,coupled,sc,F,PE,RM,KS,&
         refine_mesh_now_all)
         implicit none
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(inout) :: coupled
         type(stop_clock),intent(inout) :: sc
         type(VF),intent(inout) :: F
         type(print_export),intent(inout) :: PE
         type(refine_mesh),intent(inout) :: RM
         type(kill_switch),intent(inout) :: KS
         logical,intent(inout) :: refine_mesh_now_all
         logical,dimension(3) :: steady_solution
         logical :: continue_refinement
         logical :: suppress_warning
         suppress_warning = nrg%suppress_warning
         suppress_warning = mom%suppress_warning
         suppress_warning = ind%suppress_warning

         continue_refinement = RM%i_level.lt.SP%n_max_refinements
         steady_solution(1) = steady(mom%probe_KE).or.(.not.SP%solveMomentum)
         steady_solution(2) = steady(ind%ME_fluid(3)).or.(.not.SP%solveInduction)
         steady_solution(3) = steady(ind%JE_fluid).or.(.not.SP%solveInduction)
         refine_mesh_now_all = all(steady_solution).and.continue_refinement

         if (PE%info) then
           write(*,*) 'steady_solution = ',steady_solution
           write(*,*) 'continue_refinement = ',continue_refinement
           write(*,*) 'RM%i_level = ',RM%i_level
           write(*,*) 'SP%n_max_refinements = ',SP%n_max_refinements
         endif

         if (refine_mesh_now_all.or.RM%any_next) then
           write(*,*) 'steady_solution = ',steady_solution
           write(*,*) 'continue_refinement = ',continue_refinement
           write(*,*) 'RM%i_level = ',RM%i_level
           write(*,*) 'SP%n_max_refinements = ',SP%n_max_refinements
           call prolongate(RM)
           ! call prolongate(nrg%TMP); call prolongate(nrg,DT,RM,refine_mesh_now_all)
           call prolongate(mom%TMP); call prolongate(mom,F,DT,RM,refine_mesh_now_all)
           call prolongate(ind%TMP); call prolongate(ind,DT,RM,refine_mesh_now_all)
           call prolongate(coupled)
           call reset_Nmax(sc,coupled%n_step_stop-coupled%n_step)
         endif

         steady_solution(1) = steady_final(mom%probe_KE).or.(.not.SP%solveMomentum)
         steady_solution(2) = steady_final(ind%ME_fluid(3)).or.(.not.SP%solveInduction)
         steady_solution(3) = steady_final(ind%JE_fluid).or.(.not.SP%solveInduction)

         if (all(steady_solution).and.(.not.continue_refinement)) then
           KS%terminate_loop = .true.
         endif
       end subroutine

       end module