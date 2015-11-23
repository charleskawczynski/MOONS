      module CG_ind_mod
      use IO_tools_mod
      use IO_SF_mod
      use IO_VF_mod
      use mesh_mod
      use apply_BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use BCs_mod
      use SF_mod
      use VF_mod
      use ops_interp_mod
      use matrix_mod
      implicit none

      private
      public :: CG_ind
      ! public :: CG_ind,CG_ind_SS,compute_Ax

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      interface pre_multiply_D;   module procedure pre_multiply_D_SF;     end interface
      interface pre_multiply_D;   module procedure pre_multiply_D_VF;     end interface

      interface compute_Ax;       module procedure compute_Ax_SS;         end interface
      interface compute_Ax;       module procedure compute_Ax_transient;  end interface
      ! interface CG_ind_transient; module procedure CG_ind_transient_few;  end interface
      ! interface CG_ind_transient; module procedure CG_ind_transient_all;  end interface

      interface dotProduct;       module procedure dotProduct_SF;         end interface
      interface dotProduct;       module procedure dotProduct_VF;         end interface

      contains

       subroutine exportLap(x,m,dir,name)
         implicit none
         type(SF),intent(in) :: x
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir,name
         type(SF) :: un,Aun,temp
         type(VF) :: temp_F
         integer :: i,newU
         call init(un,x); call init(Aun,x); call init(temp,Aun)
         call init_Face(temp_F,m)
         newU = newAndOpen(dir,name)
         call assign(un,0.0_cp)
         do i=1,un%numEl
           call unitVector(un,i)
           call zeroGhostPoints(un)
           if (sum(un).gt.0.5_cp) then
             call grad(temp_F,un,m)
             call div(temp,temp_F,m)
             ! call lap(temp,un,m)
             call pre_multiply_D(Aun,temp,m)
             call export_vector_transpose_SF(Aun,newU) ! Export rows of A
           endif
         enddo
         call delete(un)
         call delete(Aun)
         call delete(temp)
         call delete(temp_F)
         close(newU)
       end subroutine

       subroutine get_diag(D,m)
         implicit none
         type(SF),intent(inout) :: D
         type(mesh),intent(in) :: m
         type(SF) :: un,Aun,temp
         type(VF) :: temp_F
         integer :: i
         call init(un,D); call init(Aun,D); call init(temp,D)
         call init_Face(temp_F,m)
         call assign(un,0.0_cp)
         do i=1,un%numEl
           call unitVector(un,i)
           call zeroGhostPoints(un)
           if (sum(un).gt.0.5_cp) then
             call grad(temp_F,un,m)
             call div(temp,temp_F,m)
             call pre_multiply_D(Aun,temp,m)
             call define_ith_diag(D,Aun,i)
           endif
         enddo
         call delete(un)
         call delete(Aun)
         call delete(temp)
         call delete(temp_F)
       end subroutine

       subroutine define_ith_diag(D,Aun,ii)
         implicit none
         type(SF),intent(inout) :: D
         type(SF),intent(inout) :: Aun
         integer,intent(in) :: ii
         integer :: i,j,k,t,m
         m = 1
         do t=1,D%s; do k=2,D%RF(t)%s(3)-1; do j=2,D%RF(t)%s(2)-1; do i=2,D%RF(t)%s(1)-1
         if (m.eq.ii) D%RF(t)%f(i,j,k) = Aun%RF(t)%f(i,j,k)
         m = m + 1
         enddo; enddo; enddo; enddo
       end subroutine

       subroutine export_vector_transpose_SF(U,un)
         implicit none
         type(SF),intent(in) :: U
         integer,intent(in) :: un
         integer :: i,j,k,t
         do t=1,U%s; do k=2,U%RF(t)%s(3)-1; do j=2,U%RF(t)%s(2)-1; do i=2,U%RF(t)%s(1)-1
         write(un,'(F20.13,T2)',advance='no') U%RF(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         write(un,*) ''
       end subroutine

       subroutine exportA(x,sigmaInv,dt,Rem,m,dir,name)
         implicit none
         type(VF),intent(in) :: x,sigmaInv
         real(cp),intent(in) :: dt,Rem
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir,name
         type(VF) :: un,Aun
         integer :: i,newU
         call init(un,x); call init(Aun,x)
         newU = newAndOpen(dir,name)
         write(*,*) 'System size = ',un%x%numEl + un%y%numEl + un%z%numEl
         call print(m)
         call assign(un,0.0_cp)
         do i=1,un%x%numEl
           call unitVector(un%x,i)
           call zeroGhostPoints(un)
           if (sum(un%x).gt.0.5_cp) then
             call compute_Ax(Aun,un,sigmaInv,dt,Rem,m)
             call export_vector_transpose(Aun,newU) ! Export rows of A
           endif
         enddo
         call assign(un,0.0_cp)
         do i=1,un%y%numEl
           call unitVector(un%y,i)
           call zeroGhostPoints(un)
           if (sum(un%y).gt.0.5_cp) then
             call compute_Ax(Aun,un,sigmaInv,dt,Rem,m)
             call export_vector_transpose(Aun,newU) ! Export rows of A
           endif
         enddo
         call assign(un,0.0_cp)
         do i=1,un%z%numEl
           call unitVector(un%z,i)
           call zeroGhostPoints(un)
           if (sum(un%z).gt.0.5_cp) then
             call compute_Ax(Aun,un,sigmaInv,dt,Rem,m)
             call export_vector_transpose(Aun,newU) ! Export rows of A
           endif
         enddo
         close(newU)
       end subroutine

       subroutine export_vector_transpose(U,un)
         implicit none
         type(VF),intent(in) :: U
         integer,intent(in) :: un
         integer :: i,j,k,t
         do t=1,U%x%s; do k=2,U%x%RF(t)%s(3)-1; do j=2,U%x%RF(t)%s(2)-1; do i=2,U%x%RF(t)%s(1)-1
         write(un,'(F15.8,T2)',advance='no') U%x%RF(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         do t=1,U%y%s; do k=2,U%y%RF(t)%s(3)-1; do j=2,U%y%RF(t)%s(2)-1; do i=2,U%y%RF(t)%s(1)-1
         write(un,'(F15.8,T2)',advance='no') U%y%RF(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         do t=1,U%z%s; do k=2,U%z%RF(t)%s(3)-1; do j=2,U%z%RF(t)%s(2)-1; do i=2,U%z%RF(t)%s(1)-1
         write(un,'(F15.8,T2)',advance='no') U%z%RF(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         write(un,*) ''
       end subroutine

      subroutine check_symmetry(x,sigmaInv,dt,Rem,m)
        ! Computes Ax = Rem^-1 curl(sigmaInv curl(x)) + dx/dt
        implicit none
        type(VF),intent(in) :: x,sigmaInv
        real(cp),intent(in) :: dt,Rem
        type(mesh),intent(in) :: m
        type(VF) :: u,v,Au,Av,temp,sigmaTemp
        type(VF) :: temp_F
        type(SF) :: uSF,vSF,lapU,lapV,tempSF
        real(cp) :: d_L,d_R
        call init(u,x); call init(Au,x); call init(temp,x)
        call init(v,x); call init(Av,x); call init(sigmaTemp,sigmaInv)

        call init(uSF,x%x); call init(lapU,x%x)
        call init(vSF,x%x); call init(lapV,x%x)
        call init(tempSF,x%x)
        call init_Face(temp_F,m)

        write(*,*) 'System size = ',x%x%numEl+x%y%numEl+x%z%numEl
        write(*,*) 'dt = ',dt
        write(*,*) 'Rem = ',Rem

        write(*,*) ' ------------------- Laplacian operator ------------------- '
        call noise(uSF); call zeroGhostPoints(uSF)
        call noise(vSF); call zeroGhostPoints(vSF)

        call grad(temp_F,uSF,m); call div(tempSF,temp_F,m); call pre_multiply_D(lapU,tempSF,m)
        call grad(temp_F,vSF,m); call div(tempSF,temp_F,m); call pre_multiply_D(lapV,tempSF,m)

        ! call lap(tempSF,uSF,m); call pre_multiply_D(lapU,tempSF,m)
        ! call lap(tempSF,vSF,m); call pre_multiply_D(lapV,tempSF,m)

        d_L = dotProduct(vSF,lapU,m,x%x,tempSF)
        d_R = dotProduct(uSF,lapV,m,x%x,tempSF)
        write(*,*) 'd_L = ',d_L
        write(*,*) 'd_R = ',d_R
        write(*,*) 'Error(lap) = ',abs(d_L-d_R)
        write(*,*) 'Error(lap)/size = ',abs(d_L-d_R)/real(x%x%numEl,cp)

        write(*,*) ' ------------------- curl-curl operator ------------------- '
        call assign(sigmaTemp,1.0_cp)
        ! call assign(sigmaTemp,sigmaInv)
        call noise(u); call zeroGhostPoints(u)
        call noise(v); call zeroGhostPoints(v)

        ! call compute_Ax(Au,u,sigmaTemp,dt,Rem,m)
        ! call compute_Ax(Av,v,sigmaTemp,dt,Rem,m)

        call compute_Ax(temp,u,sigmaTemp,dt,Rem,m); call pre_multiply_D(Au,temp,m)
        call compute_Ax(temp,v,sigmaTemp,dt,Rem,m); call pre_multiply_D(Av,temp,m)

        d_L = dotProduct(v,Au,m,x,temp)
        d_R = dotProduct(u,Av,m,x,temp)
        write(*,*) 'd_L = ',d_L
        write(*,*) 'd_R = ',d_R
        write(*,*) 'Error(curl-curl) = ',abs(d_L-d_R)
        write(*,*) 'Error(curl-curl)/size = ',abs(d_L-d_R)/real(x%x%numEl,cp)
        write(*,*) ' ---------------------------------------------------------- '

        ! call export_3D_3C(m,u,'out/LDC/','u',0)
        ! call export_3D_3C(m,v,'out/LDC/','v',0)
        call exportA(x,sigmaTemp,dt,Rem,m,'out/LDC/','A_curlcurl')
        call exportLap(x%x,m,'out/LDC/','A_lap')
        call delete(temp)
        call delete(u); call delete(Au)
        call delete(v); call delete(Av)

        call delete(uSF); call delete(lapU)
        call delete(vSF); call delete(lapV)
        call delete(tempSF); call delete(temp_F)
        stop 'Done'
      end subroutine

      subroutine pre_multiply_D_SF(DAx,Ax,m)
        ! Computes Ax = Rem^-1 curl(sigmaInv curl(x)) + dx/dt
        implicit none
        type(SF),intent(inout) :: DAx
        type(SF),intent(in) :: Ax
        type(mesh),intent(in) :: m
        type(SF) :: vol
        call init(vol,Ax)
        call volume(vol,m)
        call multiply(DAx,Ax,vol)
        ! call export_3D_1C(m,vol,'out/LDC/','volume_R',0)
        call zeroGhostPoints(DAx)
        call delete(vol)
      end subroutine

      subroutine pre_multiply_D_VF(DAx,Ax,m)
        ! Computes Ax = Rem^-1 curl(sigmaInv curl(x)) + dx/dt
        implicit none
        type(VF),intent(inout) :: DAx
        type(VF),intent(in) :: Ax
        type(mesh),intent(in) :: m
        type(SF) :: vol
        call init(vol,Ax%x)
        call volume(vol,m)
        ! call export_3D_1C(m,vol,'out/LDC/','volume',0)
        call multiply(DAx,Ax,vol)
        ! call multiply(DAx%x,Ax%x,vol)
        ! call multiply(DAx%y,Ax%y,vol)
        ! call multiply(DAx%z,Ax%z,vol)
        call delete(vol)
      end subroutine

      subroutine compute_Ax_transient(Ax,x,sigmaInv,dt,Rem,m)
        ! Computes Ax = Rem^-1 curl(sigmaInv curl(x)) + dx/dt
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,sigmaInv
        real(cp),intent(in) :: dt,Rem
        type(mesh),intent(in) :: m
        type(VF) :: temp,tempAx
        call init(temp,sigmaInv); call init(tempAx,Ax)
        call assign(temp,0.0_cp)
        call curlcurl(tempAx,x,sigmaInv,temp,m) ! Symmetric version of curl-curl operator
        call multiply(tempAx,dt/Rem)
        call add(tempAx,x)
        call zeroGhostPoints(tempAx)
        call delete(temp)
        ! call assign(Ax,tempAx)
        call pre_multiply_D(Ax,tempAx,m)
        call delete(tempAx)
      end subroutine

      subroutine compute_A(Ax,x,m)
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x
        type(mesh),intent(in) :: m
        type(VF) :: temp_F
        type(SF) :: vol
        call init_Face(temp_F,m)
        call init(vol,x)
        call volume(vol,m)
        call grad(temp_F,x,m)
        call div(Ax,temp_F,m)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call delete(vol)
        call delete(temp_F)
      end subroutine

      subroutine CG_ind(x,b,sigmaInv,dt,Rem,m,n,norm,displayTF)
        implicit none
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,sigmaInv
        real(cp),intent(in) :: dt,Rem
        type(mesh),intent(in) :: m
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        type(VF) :: temp,Ax,r,p
        call init(Ax,x)
        call init(r,x)
        call init(p,x)
        call init(temp,x)
        call CG_ind_transient_all(x,b,sigmaInv,dt,Rem,m,n,norm,displayTF,temp,Ax,r,p)
        call delete(Ax)
        call delete(r)
        call delete(p)
        call delete(temp)
      end subroutine

      subroutine CG_ind_transient_all(x,b,sigmaInv,dt,Rem,m,n,norm,displayTF,temp,Ax,r,p)
        implicit none
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,sigmaInv
        real(cp),intent(in) :: dt,Rem
        type(mesh),intent(in) :: m
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        type(VF),intent(inout) :: temp,Ax,r,p
        integer :: i,k
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call apply_BCs(x,m)
        ! call check_symmetry(x,sigmaInv,0.01_cp,1.0_cp,m)
        call test_symmetry(x%x,compute_A,m)
        stop 'Done'
        call compute_Ax(Ax,x,sigmaInv,dt,Rem,m)                    ! Compute Ax

        call pre_multiply_D(r,b,m)                     ! Compute Ax
        call subtract(r,Ax)

        ! call subtract(r,b,Ax)                                      ! r = b - Ax

        ! call export_3D_3C(m,b,'out/LDC/','b',0)
        ! call export_3D_3C(m,r,'out/LDC/','r',0)
        ! stop 'Done'
        call zeroGhostPoints(r)                                    ! Input ghost cells should not matter
        call zeroWall(r,m)                                         ! Dirichlet boundary residual is zero
        call assign(p,r)
        rsold = dotProduct(r,r,m,x,temp)                           ! rsold = r^T*r
        if (rsold.gt.0.0_cp) then
        do i=1,minval((/n,m%N_cells_tot/))
          call compute_Ax(Ax,p,sigmaInv,dt,Rem,m)                  ! Compute Ax / alpha
          alpha_temp = dotProduct(p,Ax,m,x,temp)                   ! Compute Ax / alpha
          alpha = rsold/alpha_temp                                 ! Compute Ax / alpha
          call assign(temp,p)                                      ! Update x
          call multiply(temp,alpha)                                ! Update x
          call add(x,temp)                                         ! Update x
          call apply_BCs(x,m)                                      ! Update x
          call assign(temp,Ax)                                     ! Update r
          call multiply(temp,alpha)                                ! Update r
          call subtract(r,temp)                                    ! Update r
          call zeroGhostPoints(r)                                  ! Update r
          call zeroWall(r,m)                                       ! Update r
          rsnew = dotProduct(r,r,m,x,temp)   
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif
          ! ------------------------ My residual computation ------------------
          ! call compute_Ax(Ax,x,sigmaInv,dt,Rem,m)
          ! call subtract(temp,b,Ax)
          ! call zeroGhostPoints(temp)
          ! call zeroWall(temp,m)
          ! call compute(norm,temp,m)
          ! if (i.eq.i_stop) call export_3D_1C(m,temp,'out/','r_mine',0)
          ! write(*,*) 'Residual (CG,mine) = ',sqrt(rsnew),norm%Linf
          ! -------------------------------------------------------------------
          call assign(temp,p)                                       ! Update p
          call multiply(temp,rsnew/rsold)                           ! Update p
          call add(p,r,temp)                                        ! Update p
          call zeroGhostPoints(p)                                   ! Update p
          rsold = rsnew
        enddo
        endif
        if (x%x%all_Neumann) call subtract(x%x,mean(x%x))
        if (x%y%all_Neumann) call subtract(x%y,mean(x%y))
        if (x%z%all_Neumann) call subtract(x%z,mean(x%z))
        call apply_BCs(x,m)
        if (displayTF) then
          call compute_Ax(Ax,x,sigmaInv,dt,Rem,m)
          call subtract(temp,b,Ax)
          call zeroGhostPoints(temp)
          call zeroWall(temp,m)
          call compute(norm(1),temp%x,m)
          call compute(norm(2),temp%y,m)
          call compute(norm(3),temp%z,m)
          write(*,*) 'Number of CG iterations = ',n
          write(*,*) 'Iterations (input/max) = ',(/n,m%N_cells_tot/)
          call print(norm(1),'CG Residuals (x)')
          call print(norm(2),'CG Residuals (y)')
          call print(norm(3),'CG Residuals (z)')
        endif
      end subroutine


      subroutine compute_Ax_SS(Ax,x,sigmaInv,m)
        ! Computes Ax = curl(sigmaInv curl(x))
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,sigmaInv
        type(mesh),intent(in) :: m
        type(VF) :: temp
        call init(temp,sigmaInv)
        call assign(temp,0.0_cp)
        call curlcurl(Ax,x,sigmaInv,temp,m)
        call delete(temp)
      end subroutine

      subroutine CG_ind_SS(x,b,sigmaInv,m,n,norm,displayTF,temp,Ax,r,p)
        implicit none
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,sigmaInv
        type(mesh),intent(in) :: m
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        type(VF),intent(inout) :: temp,Ax,r,p
        integer :: i
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call apply_BCs(x,m)
        call compute_Ax(Ax,x,sigmaInv,m)                           ! Compute Ax
        call subtract(r,b,Ax)                                      ! r = b - Ax
        call zeroGhostPoints(r)                                    ! Input ghost cells should not matter
        call zeroWall_conditional(r,m,x)                           ! Dirichlet boundary residual is zero
        call assign(p,r)
        rsold = dotProduct(r,r,m,x,temp)                           ! rsold = r^T*r
        do i=1,minval((/n,m%N_cells_tot/))
          call compute_Ax(Ax,p,sigmaInv,m)                         ! Compute Ax / alpha
          alpha_temp = dotProduct(p,Ax,m,x,temp)                   ! Compute Ax / alpha
          alpha = rsold/alpha_temp                                 ! Compute Ax / alpha
          call assign(temp,p)                                      ! Update x
          call multiply(temp,alpha)                                ! Update x
          call add(x,temp)                                         ! Update x
          call apply_BCs(x,m)                                      ! Update x
          call assign(temp,Ax)                                     ! Update r
          call multiply(temp,alpha)                                ! Update r
          call subtract(r,temp)                                    ! Update r
          call zeroGhostPoints(r)                                  ! Update r
          call zeroWall_conditional(r,m,x)                         ! Update r
          rsnew = dotProduct(r,r,m,x,temp)   
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif
          ! ------------------------ My residual computation ------------------
          ! call compute_Ax(Ax,x,sigmaInv,m)
          ! call subtract(temp,b,Ax)
          ! call zeroGhostPoints(temp)
          ! call zeroWall(temp,m)
          ! call compute(norm,temp,m)
          ! if (i.eq.i_stop) call export_3D_1C(m,temp,'out/','r_mine',0)
          ! write(*,*) 'Residual (CG,mine) = ',sqrt(rsnew),norm%Linf
          ! -------------------------------------------------------------------
          call assign(temp,p)                                       ! Update p
          call multiply(temp,rsnew/rsold)                           ! Update p
          call add(p,r,temp)                                        ! Update p
          call zeroGhostPoints(p)                                   ! Update p
          rsold = rsnew
        enddo
        if (x%x%all_Neumann) call subtract(x%x,mean(x%x))
        if (x%y%all_Neumann) call subtract(x%y,mean(x%y))
        if (x%z%all_Neumann) call subtract(x%z,mean(x%z))
        call apply_BCs(x,m)
        if (displayTF) then
          call compute_Ax(Ax,x,sigmaInv,m)
          call subtract(temp,b,Ax)
          call zeroGhostPoints(temp)
          call zeroWall(temp,m)
          call compute(norm(1),temp%x,m)
          call compute(norm(2),temp%y,m)
          call compute(norm(3),temp%z,m)
          write(*,*) 'Number of CG iterations = ',n
          write(*,*) 'Iterations (input/max) = ',(/n,m%N_cells_tot/)
          call print(norm(1),'CG Residuals (x)')
          call print(norm(2),'CG Residuals (y)')
          call print(norm(3),'CG Residuals (z)')
        endif
      end subroutine

      function dotProduct_VF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: A,B,x
        type(VF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp%x) + sum(temp%y) + sum(temp%z)
      end function

      function dotProduct_SF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: A,B,x
        type(SF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp)
      end function

      subroutine zeroSecondIndex(f)
        ! For debugging...
        implicit none
        type(SF),intent(inout) :: f
        integer :: i
        do i=1,f%s
          f%RF(i)%f(2,:,:) = 0.0_cp
          f%RF(i)%f(:,2,:) = 0.0_cp
          f%RF(i)%f(:,:,2) = 0.0_cp

          f%RF(i)%f(f%RF(i)%s(1)-1,:,:) = 0.0_cp
          f%RF(i)%f(:,f%RF(i)%s(2)-1,:) = 0.0_cp
          f%RF(i)%f(:,:,f%RF(i)%s(3)-1) = 0.0_cp
        enddo
      end subroutine

      end module