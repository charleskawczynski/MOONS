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
      use TF_mod
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

      interface compute_Ax;       module procedure compute_Ax_SS;         end interface
      interface compute_Ax;       module procedure compute_Ax_transient;  end interface
      ! interface CG_ind_transient; module procedure CG_ind_transient_few;  end interface
      ! interface CG_ind_transient; module procedure CG_ind_transient_all;  end interface

      contains

      subroutine compute_Ax_transient(Ax,x,sigmaInv,dt,Rem,m)
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,sigmaInv
        real(cp),intent(in) :: dt,Rem
        type(mesh),intent(in) :: m
        type(VF) :: temp
        type(VF) :: vol
        call init(temp,sigmaInv)
        call assign(temp,0.0_cp)
        call init(vol,x)
        call volume(vol,m)
        call curlcurl(Ax,x,sigmaInv,temp,m) ! Symmetric version of curl-curl operator
        call multiply(Ax,dt/Rem)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
        call delete(temp)
        call delete(vol)
      end subroutine

      subroutine compute_A_diffusion_not_SPD(Ax,x,m)
        ! Interp to face, curl about CC, interp to face, curl about CC
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(mesh),intent(in) :: m
        real(cp) :: dt,Rem
        type(VF) :: temp
        type(SF) :: vol
        type(TF) :: temp_F
        dt = 0.01_cp; Rem = 1.0_cp
        call init(temp,x)
        call init_Face(temp_F,m)
        call init(vol,x%x)

        call assign(temp,0.0_cp)
        call volume(vol,m)
        call cellcenter2face(temp_F,x,m)

        call curl(temp,temp_F,m)
        call multiply(temp,vol)
        ! call multiply(temp,sigmaInv)

        call cellcenter2face(temp_F,temp,m)
        call curl(Ax,temp_F,m)

        call add(Ax,x)
        call zeroGhostPoints(Ax)
        call multiply(Ax,dt/Rem)
        call multiply(Ax,vol)

        call delete(temp)
        call delete(vol)
        call delete(temp_F)
      end subroutine

      subroutine compute_A_diffusion_good_for_uniform(Ax,x,m)
        ! Interp to face, curl about edge, curl about face, interp to CC
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(mesh),intent(in) :: m
        real(cp) :: dt,Rem
        type(VF) :: temp,temp_E,face_VF
        type(SF) :: vol
        type(TF) :: temp_F
        dt = 0.01_cp; Rem = 1.0_cp
        call init(temp,x)
        call init_Face(face_VF,m)
        call init_Face(temp_F,m)
        call init_Edge(temp_E,m)
        call init(vol,x%x)

        call assign(temp,0.0_cp)
        call volume(vol,m)
        call cellcenter2face(temp_F,x,m)
        call curl(temp_E,temp_F,m)
        ! call multiply(temp,sigmaInv)
        call curl(face_VF,temp_E,m)

        call face2cellcenter(Ax,face_VF,m)

        call add(Ax,x)
        call zeroGhostPoints(Ax)
        call multiply(Ax,dt/Rem)
        call multiply(Ax,vol)

        call delete(temp)
        call delete(vol)
        call delete(temp_F)
        call delete(temp_E)
        call delete(face_VF)
      end subroutine

      subroutine compute_A_diffusion(Ax,x,m)
        ! Interp to face, curl about edge, curl about face, interp to CC
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(mesh),intent(in) :: m
        real(cp) :: dt,Rem
        type(VF) :: temp,temp_E,face_VF
        type(SF) :: vol
        type(TF) :: temp_F
        dt = 0.01_cp; Rem = 1.0_cp
        call init(temp,x)
        call init_Face(face_VF,m)
        call init_Face(temp_F,m)
        call init_Edge(temp_E,m)
        call init(vol,x%x)

        call assign(temp,0.0_cp)
        call volume(vol,m)
        call curl(temp,x,m) ! Collocated
        call multiply(temp,vol)
        call cellcenter2face(temp_F,temp,m)

        call curl(Ax,temp_F,m)
        call multiply(Ax,vol)

        call add(Ax,x)
        call zeroGhostPoints(Ax)
        call multiply(Ax,dt/Rem)

        call delete(temp)
        call delete(vol)
        call delete(temp_F)
        call delete(temp_E)
        call delete(face_VF)
      end subroutine

      subroutine compute_A_lap(Ax,x,m)
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

      subroutine compute_A_VF(Ax,x,m)
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(mesh),intent(in) :: m
        type(VF) :: temp
        type(SF) :: vol
        real(cp) :: dt,Rem
        dt = 0.01_cp
        Rem = 1.0_cp
        call init(vol,x%x); call init(temp,x)
        call volume(vol,m)
        call curl(temp,x,m)
        call curl(Ax,x,m)
        call multiply(Ax,dt/Rem)
        call add(Ax,x)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call delete(vol)
        call delete(temp)
      end subroutine

      subroutine compute_A_diff_collocated(Ax,x,m)
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(mesh),intent(in) :: m
        type(VF) :: temp
        type(SF) :: vol
        real(cp) :: dt,Rem
        dt = 0.01_cp
        Rem = 1.0_cp
        call init(vol,x%x); call init(temp,x)
        call volume(vol,m)
        call curl(temp,x,m)
        call curl(Ax,x,m)
        call multiply(Ax,dt/Rem)
        call add(Ax,x)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call delete(vol)
        call delete(temp)
      end subroutine

      subroutine compute_A_diff_staggered_works_non_uniform(Ax,x,m)
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(mesh),intent(in) :: m
        type(VF) :: temp_E
        type(VF) :: vol
        real(cp) :: dt,Rem
        dt = 0.01_cp
        Rem = 1.0_cp
        call init(vol,x)
        call init_Edge(temp_E,m)
        call volume(vol,m)
        
        call curl(temp_E,x,m)
        call curl(Ax,temp_E,m)
        call multiply(Ax,dt/Rem)
        call add(Ax,x)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call delete(vol)
        call delete(temp_E)
      end subroutine

      subroutine compute_A_diff(Ax,x,m)
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(mesh),intent(in) :: m
        type(VF) :: temp_E,temp_F
        type(VF) :: vol
        real(cp) :: dt,Rem
        dt = 0.01_cp
        Rem = 1.0_cp
        call init(vol,x)
        call init_Face(temp_F,m)
        call init_Edge(temp_E,m)

        call volume(vol,m)
        call cellcenter2face(temp_F,x,m)
        
        call curl(temp_E,temp_F,m)
        call curl(temp_F,temp_E,m)
        call face2cellcenter(Ax,temp_F,m)
        call multiply(Ax,dt/Rem)
        call add(Ax,x)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call delete(temp_F)
        call delete(vol)
        call delete(temp_E)
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
        type(VF) :: temp_F
        type(VF) :: D_lap
        integer :: i,k
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call apply_BCs(x,m)

        call init(D_lap,x)
        ! call init_Face(temp_F,m)

        call test_symmetry(compute_A_lap,x%x,sigmaInv,r,m,'lap_SF')
        call export_operator(compute_A_lap,x%x,sigmaInv,r,m,'out/LDC/','lap_SF')

        call get_diagonal(compute_A_lap,D_lap%x,sigmaInv,r,m)
        call export_matrix(D_lap%x,m,'out/LDC/','diag(lap_SF)')

        call test_symmetry(compute_A_diff_collocated,x,sigmaInv,r,m,'diff_VF')
        call export_operator(compute_A_diff_collocated,x,sigmaInv,r,m,'out/LDC/','diff_VF')

        call get_diagonal(compute_A_diff_collocated,D_lap,sigmaInv,r,m)
        call export_matrix(D_lap,m,'out/LDC/','diag(diff_VF)')

        call delete(D_lap)
        ! call delete(temp_F)

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
        rsold = dot_product(r,r,m,x,temp)                           ! rsold = r^T*r
        if (rsold.gt.0.0_cp) then
        do i=1,minval((/n,m%N_cells_tot/))
          call compute_Ax(Ax,p,sigmaInv,dt,Rem,m)                  ! Compute Ax / alpha
          alpha_temp = dot_product(p,Ax,m,x,temp)                   ! Compute Ax / alpha
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
          rsnew = dot_product(r,r,m,x,temp)   
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
        rsold = dot_product(r,r,m,x,temp)                           ! rsold = r^T*r
        do i=1,minval((/n,m%N_cells_tot/))
          call compute_Ax(Ax,p,sigmaInv,m)                         ! Compute Ax / alpha
          alpha_temp = dot_product(p,Ax,m,x,temp)                   ! Compute Ax / alpha
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
          rsnew = dot_product(r,r,m,x,temp)   
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

      end module