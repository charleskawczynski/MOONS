       module ops_embedExtract_surface_mod
       ! Not all embed / extract routines are used for each method. For example,
       ! the CT method only uses embedEdge, and not embedCC or embedFace.
       ! 
       ! Pre-processor directives: (_PARALLELIZE_EMBEDEXTRACT_)

       use current_precision_mod
       use mesh_mod
       use domain_mod
       use RF_mod
       use SF_mod
       use VF_mod

       implicit none

       private

       public :: embed_N_surface
       public :: extract_N_surface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ***************************** LOW-LEVEL ROUTINE *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine embedExtract_RF(RF_out,RF_in,out1,out2,in1,in2)
         ! This is the embed/extract (EE) routine.
         implicit none
         type(realField),intent(in) :: RF_in
         type(realField),intent(inout) :: RF_out
         integer,dimension(3),intent(in) :: out1,out2,in1,in2
#ifdef _PARALLELIZE_EMBEDEXTRACT_
         integer :: i,j,k
         integer,dimension(3) :: suppress_warning
         suppress_warning = in2 ! in2 is not needed for parallel computations
         !$OMP PARALLEL DO
         do k=out1(3),out2(3);do j=out1(2),out2(2);do i=out1(1),out2(1)
         RF_out%f(i,j,k) = &
         RF_in %f(in1(1)+(i-out1(1)),in1(2)+(j-out1(2)),in1(3)+(k-out1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
#else
         RF_out%f(out1(1):out2(1),out1(2):out2(2),out1(3):out2(3)) = &
         RF_in %f( in1(1): in2(1), in1(2): in2(2), in1(3): in2(3))
#endif
       end subroutine

       ! *********************************************************************************
       ! *************************** CASE SPECIFIC ROUTINES ******************************
       ! *********************************************************************************

       subroutine embed_N_surface(N_t,N_i,D) ! For surface mesh testing
         implicit none
         type(SF),intent(inout) :: N_t
         type(SF),intent(in) :: N_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(N_t%RF(D%sd(i)%g_tot_id),N_i%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% NB1(1),D%sd(i)% NB1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% NB2(1),D%sd(i)% NB2(2),D%sd(i)% NB2(3)/),&
            (/D%sd(i)%TNB1(1),D%sd(i)%TNB1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TNB2(1),D%sd(i)%TNB2(2),D%sd(i)%TNB2(3)/))
         enddo
       end subroutine

       subroutine extract_N_surface(N_i,N_t,D) ! For surface mesh testing
         implicit none
         type(SF),intent(inout) :: N_i
         type(SF),intent(in) :: N_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(N_i%RF(D%sd(i)%g_in_id),N_t%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TNB1(1),D%sd(i)%TNB1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TNB2(1),D%sd(i)%TNB2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)% NB1(1),D%sd(i)% NB1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% NB2(1),D%sd(i)% NB2(2),D%sd(i)% NB2(3)/))
         enddo
       end subroutine

       end module