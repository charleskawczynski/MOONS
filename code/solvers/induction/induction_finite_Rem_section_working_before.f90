         if (lowRem) then
           call cellCenter2Face(ind%temp_F,ind%B0,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative

           call add(ind%curlUCrossB,ind%B_face)
           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,ind%m,5,getExportErrors(ss_MHD))

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         endif
         if (ind%finite_Rem) then
           ! call Finite_Rem_CG_implicit(ind%B,ind%B0,ind%U_E,ind%sigmaInv_edge,&
           ! ind%m,5,getExportErrors(ss_MHD),ind%temp_F,ind%temp_E_TF,ind%temp_E)

           call add(ind%Bstar,ind%B0,ind%B) ! Finite Rem
           call cellCenter2Face(ind%temp_F,ind%Bstar,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative (in divergence form)

           call add(ind%curlUCrossB,ind%B_face)
           call solve(ind%PCG_B,ind%B_face,ind%curlUCrossB,ind%m,1000,getExportErrors(ss_MHD))
           ! call solve(ind%PCG_B,ind%B_face,ind%curlUCrossB,ind%m,&
           ! ind%B%x%numEl+ind%B%y%numEl+ind%B%z%numEl,getExportErrors(ss_MHD))

           ! Clean B
           call div(ind%divB,ind%B_face,ind%m)
           call solve(ind%PCG_cleanB,ind%phi,ind%divB,ind%m,5,getExportErrors(ss_MHD))
           call grad(ind%temp_F,ind%phi,ind%m)
           call subtract(ind%B_face,ind%temp_F)
           call apply_BCs(ind%B_face,ind%m)

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         endif
         if (semi_implicit) then

           ! call Finite_Rem_CG_semi(ind%B,ind%B0,ind%U_E,ind%sigmaInv_edge,&
           ! ind%m,5,ind%dTime,ind%Rem,ind%theta,getExportErrors(ss_MHD),&
           ! ind%temp_F,ind%temp_F2,ind%temp_E_TF,ind%temp_E)

           call add(ind%Bstar,ind%B0,ind%B) ! Finite Rem
           call cellCenter2Face(ind%temp_F,ind%Bstar,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative (in divergence form)

           call add(ind%curlUCrossB,ind%B_face)

           call curl(ind%temp_E,ind%B_face,ind%m)
           call multiply(ind%temp_E,ind%sigmaInv_edge)
           call curl(ind%temp_F,ind%temp_E,ind%m)
           call multiply(ind%temp_F,ind%dTime/ind%Rem*(1.0_cp-ind%theta))
           call add(ind%curlUCrossB,ind%temp_F)

           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,ind%m,5,getExportErrors(ss_MHD))

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         endif
