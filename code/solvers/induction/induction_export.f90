       subroutine export_induction(ind,m,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         if (restartB.and.(.not.solveInduction)) then
           ! This preserves the initial data
         else
           if (solveInduction) then
             write(*,*) 'Exporting Solutions for B'
             call export_raw(m,ind%B0   ,dir//'Bfield/','B0',0)
             call export_raw(m,ind%B    ,dir//'Bfield/','B',0)
             call export_raw(m,ind%J_cc ,dir//'Jfield/','J',0)
             call export_raw(m,ind%sigma,dir//'material/','sigma',0)
             call export_raw(m,ind%divB,dir//'Bfield/','divB',0)
             call export_raw(m,ind%divJ,dir//'Jfield/','divJ',0)
             call export_raw(m,ind%U_cct,dir//'Bfield/','U',0)

             call export_raw(m,ind%U_E%x%x ,dir//'Bfield/','U',0)
             call export_raw(m,ind%U_E%y%y ,dir//'Bfield/','V',0)
             call export_raw(m,ind%U_E%z%z ,dir//'Bfield/','W',0)

             call export_processed(m,ind%B0   ,dir//'Bfield/','B0',0)
             call export_processed(m,ind%B    ,dir//'Bfield/','B',0)
             call export_processed(m,ind%J_cc ,dir//'Jfield/','J',0)
             call export_processed(m,ind%sigma,dir//'material/','sigma',0)
             call export_processed(m,ind%divB,dir//'Bfield/','divB',0)
             call export_processed(m,ind%divJ,dir//'Jfield/','divJ',0)
             write(*,*) '     finished'
           endif
         endif
       end subroutine