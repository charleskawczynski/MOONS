       module ops_aux_mod
       !
       ! Directions are frequently used in this code.
       ! For clarity, some diagrams here show how the
       ! directions are defined.
       !
       ! faceDir = 1 (x)
       !
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !      faceDir --->  \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       !
       !
       !
       ! edgeDir = 1 (x)
       !
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !                    \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       !                        -------> edgeDir
       !
       !
       use current_precision_mod
       use bctype_mod
       use face_edge_corner_indexing_mod
       use ops_del_mod
       use grid_mod
       use mesh_mod
       use mesh_domain_mod
       use ops_embedExtract_mod
       use export_raw_processed_mod
       use GF_mod
       use VF_mod
       use SF_mod
       use index_mapping_mod

       implicit none

       private

       ! ----------------------------------- OTHER ROUTINES ------------------------------------

       public :: dot_product
       interface dot_product;             module procedure dot_product_SF;            end interface
       interface dot_product;             module procedure dot_product_VF;            end interface

       public :: flux
       interface flux;                    module procedure flux_VF;                   end interface
       interface flux;                    module procedure flux_VF_SD;                end interface

       public :: subtract_physical_mean
       interface subtract_physical_mean;  module procedure subtract_phys_mean_SF;     end interface
       interface subtract_physical_mean;  module procedure subtract_phys_mean_vol_SF; end interface

       public :: physical_mean
       interface physical_mean;           module procedure phys_mean_vol_SF;          end interface
       interface physical_mean;           module procedure phys_mean_SF;              end interface

       public :: stabilityTerms
       interface stabilityTerms;          module procedure stabilityTerms_GF;         end interface
       interface stabilityTerms;          module procedure stabilityTerms_SF;         end interface
       interface stabilityTerms;          module procedure stabilityTerms_VF;         end interface

       public :: check_symmetry_x
       interface check_symmetry_x;        module procedure check_symmetry_x_SF;       end interface
       interface check_symmetry_x;        module procedure check_symmetry_x_VF;       end interface
       public :: check_symmetry_y
       interface check_symmetry_y;        module procedure check_symmetry_y_SF;       end interface
       interface check_symmetry_y;        module procedure check_symmetry_y_VF;       end interface
       public :: check_symmetry_z
       interface check_symmetry_z;        module procedure check_symmetry_z_SF;       end interface
       interface check_symmetry_z;        module procedure check_symmetry_z_VF;       end interface

       public :: zeroGhostPoints
       interface zeroGhostPoints;         module procedure zeroGhostPoints_SF;        end interface
       interface zeroGhostPoints;         module procedure zeroGhostPoints_VF;        end interface

       public :: zeroWall
       interface zeroWall;                module procedure zeroWall_SF;               end interface
       interface zeroWall;                module procedure zeroWall_VF;               end interface

       public :: zeroWall_conditional
       interface zeroWall_conditional;    module procedure zeroWall_conditional_SF;   end interface
       interface zeroWall_conditional;    module procedure zeroWall_conditional_VF;   end interface
       interface zeroWall_conditional;    module procedure zeroWall_conditional_SF2;  end interface
       interface zeroWall_conditional;    module procedure zeroWall_conditional_VF2;  end interface

       public :: treatInterface
       interface treatInterface;          module procedure treatInterface_GF;         end interface
       interface treatInterface;          module procedure treatInterface_VF;         end interface
       interface treatInterface;          module procedure treatInterface_SF;         end interface

       public :: displayPhysicalMinMax
       interface displayPhysicalMinMax;   module procedure displayPhysicalMinMax_SF;  end interface
       interface displayPhysicalMinMax;   module procedure displayPhysicalMinMax_VF;  end interface

       public :: displayGlobalMinMax
       interface displayGlobalMinMax;     module procedure displayGlobalMinMax_SF;    end interface
       interface displayGlobalMinMax;     module procedure displayGlobalMinMax_VF;    end interface

       public :: unitVector
       interface unitVector;              module procedure unitVector_SF;             end interface

       public :: deleteUnitVector
       interface deleteUnitVector;        module procedure deleteUnitVector_SF;       end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ********************************* REAL ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine stabilityTerms_GF(fo,fi,g,n,dir) ! Finished
         ! Computes
         !                     |  fi  |
         !    fo =  max( fo  , | ---- | )
         !                     | dh^n |
         !
         implicit none
         type(grid_field),intent(inout) :: fo
         type(grid_field),intent(in) :: fi
         type(grid),intent(in) :: g
         integer,intent(in) :: n,dir
         integer,dimension(3) :: p
         integer :: i,j,k,t
         p = eye_given_dir(dir)
         !$OMP PARALLEL DO
         do k=1,fo%s(3); do j=1,fo%s(2); do i=1,fo%s(1)
           t = i*p(1) + j*p(2) + k*p(3)
           fo%f(i,j,k) = maxval((/fo%f(i,j,k),abs(fi%f(i,j,k)/g%c(dir)%dhn(t)**real(n,cp))/))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       function flux_VF(u,m) result(BF) ! Computes: BF = ∫∫ u•n dA
         implicit none
         type(VF),intent(in) :: u
         type(mesh),intent(in) :: m
         real(cp) :: BF
         type(VF) :: temp
         integer :: t
         BF = 0.0_cp
         call init(temp,u); call assign(temp,u)
         call assign_ghost_XPeriodic(temp,0.0_cp)
         BF = boundary_flux(temp,m)
         call delete(temp)
       end function

       subroutine subtract_phys_mean_SF(u)
         ! Subtracts the physical mean from scalar field u
         !      u = u - mean(u)
         ! Where this mean operation is only in the interior
         implicit none
         type(SF),intent(inout) :: u
         real(cp) :: meanU
         meanU = physical_mean(u)
         call subtract(u,meanU)
         call zeroGhostPoints(u)
       end subroutine

       subroutine subtract_phys_mean_vol_SF(u,vol,temp)
         ! Subtracts the physical mean from scalar field u
         !      u = u - mean(u)
         ! Where this mean operation is only in the interior
         implicit none
         type(SF),intent(inout) :: u,temp
         type(SF),intent(in) :: vol
         real(cp) :: meanU
         meanU = physical_mean(u,vol,temp)
         call subtract(u,meanU)
         call zeroGhostPoints(u)
       end subroutine

       function phys_mean_vol_SF(u,vol,temp) result(meanU)
         ! Computes physical mean from scalar field u: mean(u*volume)
         implicit none
         type(SF),intent(inout) :: u,temp
         type(SF),intent(in) :: vol
         real(cp) :: meanU
         call multiply(temp,u,vol)
         meanU = sum(temp,1)/real(u%numPhysEl,cp)
       end function

       function phys_mean_SF(u) result(meanU)
         ! Computes physical mean from scalar field u: mean(u)
         implicit none
         type(SF),intent(in) :: u
         real(cp) :: meanU
         meanU = sum(u,1)/real(u%numPhysEl,cp)
       end function

       subroutine treatInterface_GF(f,s,take_high_value)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         logical,intent(in) :: take_high_value
         integer :: i,j,k
         real(cp) :: low_value,high_value,tol
         low_value = minval(f); high_value = maxval(f)
         ! Make interface property the min/max of
         ! fluid / wall depending on treatment
         tol = 10.0_cp**(-10.0_cp)
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
         if ((f(i,j,k).gt.low_value+tol).and.(f(i,j,k).lt.high_value-tol)) then
           if (take_high_value) then;  f(i,j,k) = high_value
           else;                       f(i,j,k) = low_value
           endif
         endif
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

      function dot_product_VF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: A,B,x
        type(VF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroWall_conditional(temp,m,x)
        call assign_wall_Dirichlet(temp,0.0_cp,x)
        dot = sum(temp%x,1) + sum(temp%y,1) + sum(temp%z,1)
      end function

      function dot_product_SF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: A,B,x
        type(SF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp,1)
      end function

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* SCALAR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

        subroutine check_symmetry_x_SF(m,u,dir,name,pad)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          character(len=*),intent(in) :: dir,name
          integer,intent(in) :: pad
          type(SF) :: temp
          real(cp) :: tol,e
          call init(temp,u)
          call assign(temp,u)
          call abs(temp) ! optional
          call symmetry_local_x(temp)
          tol = 10.0_cp**(-15.0_cp); e = amax(temp)
          if (e.gt.tol) then
            call export_raw(m,u,dir,name,pad)
            call export_raw(m,temp,dir,name//'_symm',pad)
            write(*,*) 'Symmetry broken in ',name, ' in check_symmetry_x_SF in SF.f90'
            write(*,*) 'Symmetry error,tol = ',e,tol
            call delete(temp)
            stop 'Done'
          else; call delete(temp)
          endif
        end subroutine

        subroutine check_symmetry_y_SF(m,u,dir,name,pad)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          character(len=*),intent(in) :: dir,name
          integer,intent(in) :: pad
          type(SF) :: temp
          real(cp) :: tol,e
          call init(temp,u)
          call assign(temp,u)
          call abs(temp) ! optional
          call symmetry_local_y(temp)
          tol = 10.0_cp**(-15.0_cp); e = amax(temp)
          if (e.gt.tol) then
            call export_raw(m,u,dir,name,pad)
            call export_raw(m,temp,dir,name//'_symm',pad)
            write(*,*) 'Symmetry broken in ',name, ' in check_symmetry_y_SF in SF.f90'
            write(*,*) 'Symmetry error,tol = ',e,tol
            call delete(temp)
            stop 'Done'
          else; call delete(temp)
          endif
        end subroutine

        subroutine check_symmetry_z_SF(m,u,dir,name,pad)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          character(len=*),intent(in) :: dir,name
          integer,intent(in) :: pad
          type(SF) :: temp
          real(cp) :: tol,e
          call init(temp,u)
          call assign(temp,u)
          call abs(temp) ! optional
          call symmetry_local_z(temp)
          tol = 10.0_cp**(-15.0_cp); e = amax(temp)
          if (e.gt.tol) then
            call export_raw(m,u,dir,name,pad)
            call export_raw(m,temp,dir,name//'_symm',pad)
            write(*,*) 'Symmetry broken in ',name, ' in check_symmetry_z_SF in SF.f90'
            write(*,*) 'Symmetry error,tol = ',e,tol
            call delete(temp)
            stop 'Done'
          else; call delete(temp)
          endif
        end subroutine

        subroutine check_symmetry_x_VF(m,A,dir,name,pad)
          implicit none
          type(mesh),intent(in) :: m
          type(VF),intent(in) :: A
          character(len=*),intent(in) :: dir,name
          integer,intent(in) :: pad
          call check_symmetry_x(m,A%x,dir,name//'_x',pad)
          call check_symmetry_x(m,A%y,dir,name//'_y',pad)
          call check_symmetry_x(m,A%z,dir,name//'_z',pad)
        end subroutine

        subroutine check_symmetry_y_VF(m,A,dir,name,pad)
          implicit none
          type(mesh),intent(in) :: m
          type(VF),intent(in) :: A
          character(len=*),intent(in) :: dir,name
          integer,intent(in) :: pad
          call check_symmetry_y(m,A%x,dir,name//'_x',pad)
          call check_symmetry_y(m,A%y,dir,name//'_y',pad)
          call check_symmetry_y(m,A%z,dir,name//'_z',pad)
        end subroutine

        subroutine check_symmetry_z_VF(m,A,dir,name,pad)
          implicit none
          type(mesh),intent(in) :: m
          type(VF),intent(in) :: A
          character(len=*),intent(in) :: dir,name
          integer,intent(in) :: pad
          call check_symmetry_z(m,A%x,dir,name//'_x',pad)
          call check_symmetry_z(m,A%y,dir,name//'_y',pad)
          call check_symmetry_z(m,A%z,dir,name//'_z',pad)
        end subroutine

       subroutine zeroGhostPoints_SF(f)
         implicit none
         type(SF),intent(inout) :: f
         call assign_ghost_XPeriodic(f,0.0_cp)
       end subroutine

       subroutine zeroWall_SF(f,m)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         call assign_wall_Dirichlet(f,0.0_cp)
       end subroutine

       subroutine zeroWall_conditional_SF(f,m)
         ! Sets wall coincident values to zero if
         ! boundary conditions of u are NOT Neumann (bctype=3)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         call assign_wall_Dirichlet(f,0.0_cp)
       end subroutine

       subroutine zeroWall_conditional_SF2(f,m,u)
         ! Sets wall coincident values to zero if
         ! boundary conditions of u are NOT Neumann (bctype=3)
         implicit none
         type(SF),intent(inout) :: f
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         call assign_wall_Dirichlet(f,0.0_cp,u)
       end subroutine

       subroutine treatInterface_SF(f,take_high_value)
         implicit none
         type(SF),intent(inout) :: f
         logical,intent(in) :: take_high_value
         integer :: i
         do i=1,f%s; call treatInterface(f%BF(i)%GF%f,f%BF(i)%GF%s,take_high_value); enddo
       end subroutine

       subroutine displayPhysicalMinMax_SF(U,name,un)
         implicit none
         type(SF),intent(in) :: U
         character(len=*),intent(in) :: name
         integer,intent(in) :: un
         write(un,*) 'Min/Max ('//name//') = ',min(u,1),max(u,1)
       end subroutine

       subroutine displayGlobalMinMax_SF(U,name,un)
         implicit none
         type(SF),intent(in) :: U
         character(len=*),intent(in) :: name
         integer,intent(in) :: un
         write(un,*) 'Min/Max ('//name//') = ',min(u),max(u)
       end subroutine

       subroutine unitVector_SF(U,un)
         implicit none
         type(SF),intent(inout) :: U
         integer,intent(in) :: un
         integer :: i,j,k,t
         if (un.lt.1) stop 'Error: un must > 0 in unitVector_SF in ops_aux.f90'
         if (un.gt.U%numEl) stop 'Error: un must < U%numEl in unitVector_SF in ops_aux.f90'
         call get_3D_index(i,j,k,t,U,un)
         u%BF(t)%GF%f(i,j,k) = 1.0_cp
       end subroutine

       subroutine deleteUnitVector_SF(U,un)
         implicit none
         type(SF),intent(inout) :: U
         integer,intent(in) :: un
         integer :: i,j,k,t
         if (un.lt.1) stop 'Error: un must > 0 in unitVector_consecutive_SF in ops_aux.f90'
         if (un.gt.U%numEl) stop 'Error: un must < U%numEl in unitVector_consecutive_SF in ops_aux.f90'
         call get_3D_index(i,j,k,t,U,un)
         u%BF(t)%GF%f(i,j,k) = 0.0_cp
       end subroutine

       subroutine stabilityTerms_SF(fo,fi,m,n,dir)
         implicit none
         type(SF),intent(inout) :: fo
         type(SF),intent(in) :: fi
         type(mesh),intent(in) :: m
         integer,intent(in) :: n,dir
         integer :: i
         call assign(fo,0.0_cp)
         do i=1,fi%s; call stabilityTerms(fo%BF(i)%GF,fi%BF(i)%GF,m%B(i)%g,n,dir); enddo
         call zeroGhostPoints(fo)
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine zeroWall_VF(V,m)
         implicit none
         type(VF),intent(inout) :: V
         type(mesh),intent(in) :: m
         call zeroWall(V%x,m); call zeroWall(V%y,m); call zeroWall(V%z,m)
       end subroutine

       subroutine zeroWall_conditional_VF2(V,m,U)
         implicit none
         type(VF),intent(inout) :: V
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call zeroWall_conditional(V%x,m,U%x)
         call zeroWall_conditional(V%y,m,U%y)
         call zeroWall_conditional(V%z,m,U%z)
       end subroutine

       subroutine zeroWall_conditional_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call zeroWall_conditional(U%x,m)
         call zeroWall_conditional(U%y,m)
         call zeroWall_conditional(U%z,m)
       end subroutine

       subroutine stabilityTerms_VF(fo,fi,m,n)
         implicit none
         type(SF),intent(inout) :: fo
         type(VF),intent(in) :: fi
         type(mesh),intent(in) :: m
         integer,intent(in) :: n
         call stabilityTerms(fo,fi%x,m,n,1)
         call stabilityTerms(fo,fi%y,m,n,2)
         call stabilityTerms(fo,fi%z,m,n,3)
       end subroutine

       function flux_VF_SD(f,m,MD) result(BF)
         implicit none
         type(VF),intent(in) :: f
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         real(cp) :: BF
         type(mesh) :: m_temp
         type(VF) :: temp
         if (.not.f%is_Face) stop 'Error: bad DL in flux_VF_SD in ops_aux.f90'
         call init_other(m_temp,m,MD)
         call init_Face(temp,m_temp)
         call extractFace(temp,f,MD)
         BF = flux(temp,m_temp)
         call delete(temp)
         call delete(m_temp)
       end function

       subroutine zeroGhostPoints_VF(f)
         implicit none
         type(VF),intent(inout) :: f
         call zeroGhostPoints(f%x)
         call zeroGhostPoints(f%y)
         call zeroGhostPoints(f%z)
       end subroutine

       subroutine treatInterface_VF(f,take_high_value)
         implicit none
         type(VF),intent(inout) :: f
         logical,intent(in) :: take_high_value
         call treatInterface(f%x,take_high_value)
         call treatInterface(f%y,take_high_value)
         call treatInterface(f%z,take_high_value)
       end subroutine

       subroutine displayPhysicalMinMax_VF(U,name,un)
         implicit none
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: name
         integer,intent(in) :: un
         call displayPhysicalMinMax(U%x,name//'_x',un)
         call displayPhysicalMinMax(U%y,name//'_y',un)
         call displayPhysicalMinMax(U%z,name//'_z',un)
       end subroutine

       subroutine displayGlobalMinMax_VF(U,name,un)
         implicit none
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: name
         integer,intent(in) :: un
         call displayGlobalMinMax(U%x,name//'_x',un)
         call displayGlobalMinMax(U%y,name//'_y',un)
         call displayGlobalMinMax(U%z,name//'_z',un)
       end subroutine

       end module