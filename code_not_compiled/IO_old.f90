
       subroutine export_momentum(mom,SP,DT)
         implicit none
         type(momentum),intent(in) :: mom
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'export_momentum at n_step = ',SP%VS%U%TMP%n_step
         call export_raw(mom%m,mom%U    ,str(DT%U%restart),'U',0)
         call export_raw(mom%m,mom%Ustar,str(DT%U%restart),'Ustar',0)
         call export_raw(mom%m,mom%Unm1 ,str(DT%U%restart),'Unm1',0)
         call export_raw(mom%m,mom%p    ,str(DT%p%restart),'p',0)
         call export_raw(mom%m,mom%F    ,str(DT%U%restart),'F_external',0)
         call export_raw(mom%m,mom%Fnm1 ,str(DT%U%restart),'Fnm1_external',0)
         call export_raw(mom%m,mom%L    ,str(DT%U%restart),'L_external',0)
         call export_raw(mom%m,mom%PCG_U%r,str(DT%U%restart),'r_PCG_U',0)
         call export_raw(mom%m,mom%PCG_P%r,str(DT%P%restart),'r_PCG_P',0)
         call export(mom%PCG_U,str(DT%U%restart),'PCG_U')
         call export(mom%PCG_P,str(DT%P%restart),'PCG_P')
         call export(mom%probe_divU,str(DT%U%restart),'probe_divU')
         call export(mom%probe_KE,str(DT%U%restart),'probe_KE')
         call export(mom%probe_Q,str(DT%U%restart),'probe_Q')
         if (mom%m%MP%plane_any) call export(mom%probe_KE_2C,str(DT%U%restart),'probe_KE_2C')
       end subroutine

       subroutine import_momentum(mom,SP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'import_momentum at n_step = ',SP%VS%U%TMP%n_step
         call import_raw(mom%m,mom%U    ,str(DT%U%restart),'U',0)
         call import_raw(mom%m,mom%Ustar,str(DT%U%restart),'Ustar',0)
         call import_raw(mom%m,mom%Unm1 ,str(DT%U%restart),'Unm1',0)
         call import_raw(mom%m,mom%p    ,str(DT%p%restart),'p',0)
         call import_raw(mom%m,mom%F    ,str(DT%U%restart),'F_external',0)
         call import_raw(mom%m,mom%Fnm1 ,str(DT%U%restart),'Fnm1_external',0)
         call import_raw(mom%m,mom%L    ,str(DT%U%restart),'L_external',0)
         call import_raw(mom%m,mom%PCG_U%r,str(DT%U%restart),'r_PCG_U',0)
         call import_raw(mom%m,mom%PCG_P%r,str(DT%P%restart),'r_PCG_P',0)
         call import(mom%PCG_U,str(DT%U%restart),'PCG_U')
         call import(mom%PCG_P,str(DT%P%restart),'PCG_P')
         call import(mom%probe_divU,str(DT%U%restart),'probe_divU')
         call import(mom%probe_KE,str(DT%U%restart),'probe_KE')
         call import(mom%probe_Q,str(DT%U%restart),'probe_Q')
         if (mom%m%MP%plane_any) call import(mom%probe_KE_2C,str(DT%U%restart),'probe_KE_2C')
       end subroutine

       subroutine export_induction(ind,SP,DT)
         implicit none
         type(induction),intent(in) :: ind
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'export_induction at n_step = ',SP%VS%B%TMP%n_step
         call export_raw(ind%m,ind%B,str(DT%B%restart),'B',0)
         call export_raw(ind%m,ind%Bnm1,str(DT%B%restart),'Bnm1',0)
         call export_raw(ind%m,ind%B0,str(DT%B%restart),'B0',0)
         call export_raw(ind%m,ind%F,str(DT%B%restart),'F',0)
         call export_raw(ind%m,ind%Fnm1,str(DT%B%restart),'Fnm1',0)
         call export_raw(ind%m,ind%L,str(DT%B%restart),'L',0)
         call export_raw(ind%m,ind%Bstar,str(DT%B%restart),'Bstar',0)
         call export_raw(ind%m,ind%phi,str(DT%phi%restart),'phi',0)
         call export_raw(ind%m,ind%J,str(DT%J%restart),'J',0)
         if (SP%IT%unsteady_B0%add) then
           call export(ind%probe_dB0dt,str(DT%B%restart),'probe_dB0dt')
           call export(ind%probe_B0   ,str(DT%B%restart),'probe_B0')
         endif
         call export(ind%probe_divB,str(DT%B%restart),'probe_divB')
         call export(ind%probe_divJ,str(DT%J%restart),'probe_divJ')
         call export(ind%JE,        str(DT%J%restart),'JE')
         call export(ind%JE_fluid,  str(DT%J%restart),'JE_fluid')
         call export(ind%ME          ,str(DT%B%restart),'ME')
         call export(ind%ME_fluid    ,str(DT%B%restart),'ME_fluid')
         call export(ind%ME_conductor,str(DT%B%restart),'ME_conductor')
       end subroutine

       subroutine import_induction(ind,SP,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'import_induction at n_step = ',SP%VS%B%TMP%n_step
         call import_raw(ind%m,ind%B,str(DT%B%restart),'B',0)
         call import_raw(ind%m,ind%Bnm1,str(DT%B%restart),'Bnm1',0)
         call import_raw(ind%m,ind%B0,str(DT%B%restart),'B0',0)
         call import_raw(ind%m,ind%F,str(DT%B%restart),'F',0)
         call import_raw(ind%m,ind%Fnm1,str(DT%B%restart),'Fnm1',0)
         call import_raw(ind%m,ind%L,str(DT%B%restart),'L',0)
         call import_raw(ind%m,ind%Bstar,str(DT%B%restart),'Bstar',0)
         call import_raw(ind%m,ind%phi,str(DT%phi%restart),'phi',0)
         call import_raw(ind%m,ind%J,str(DT%J%restart),'J',0)
         if (SP%IT%unsteady_B0%add) then
           call import(ind%probe_dB0dt,str(DT%B%restart),'probe_dB0dt')
           call import(ind%probe_B0   ,str(DT%B%restart),'probe_B0')
         endif
         call import(ind%probe_divB,str(DT%B%restart),'probe_divB')
         call import(ind%probe_divJ,str(DT%J%restart),'probe_divJ')
         call import(ind%JE,        str(DT%J%restart),'JE')
         call import(ind%JE_fluid,  str(DT%J%restart),'JE_fluid')
         call import(ind%ME          ,str(DT%B%restart),'ME')
         call import(ind%ME_fluid    ,str(DT%B%restart),'ME_fluid')
         call import(ind%ME_conductor,str(DT%B%restart),'ME_conductor')
       end subroutine

       subroutine export_energy(nrg,SP,DT)
         implicit none
         type(energy),intent(in) :: nrg
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'export_energy at n_step = ',SP%VS%T%TMP%n_step
         call export_raw(nrg%m,nrg%T,str(DT%T%field),'T',0)
         call export_raw(nrg%m,nrg%Tnm1,str(DT%T%field),'Tnm1',0)
         call export_raw(nrg%m,nrg%F,str(DT%T%field),'F',0)
         call export_raw(nrg%m,nrg%Fnm1,str(DT%T%field),'Fnm1',0)
         call export_raw(nrg%m,nrg%L,str(DT%T%field),'L',0)
       end subroutine

       subroutine import_energy(nrg,SP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'import_energy at n_step = ',SP%VS%T%TMP%n_step
         call import_raw(nrg%m,nrg%T,str(DT%T%field),'T',0)
         call import_raw(nrg%m,nrg%Tnm1,str(DT%T%field),'Tnm1',0)
         call import_raw(nrg%m,nrg%F,str(DT%T%field),'F',0)
         call import_raw(nrg%m,nrg%Fnm1,str(DT%T%field),'Fnm1',0)
         call import_raw(nrg%m,nrg%L,str(DT%T%field),'L',0)
       end subroutine

