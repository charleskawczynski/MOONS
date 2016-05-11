       module export_analytic_mod
       use current_precision_mod
       use SF_mod
       use mesh_mod
       use profile_funcs_mod
       use export_raw_processed_mod
       use dir_tree_mod
       use string_mod
       implicit none

       private
       public :: export_Shercliff
       public :: export_Hunt
       public :: export_SH
       
       contains

       subroutine export_Shercliff(m,Ha,dir,DT)
         implicit none
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Ha
         integer,intent(in) :: dir
         type(dir_tree),intent(in) :: DT
         type(SF) :: temp
         real(cp) :: mu,dpdz
         integer :: i
         mu = 1.0_cp
         dpdz = -1.0_cp
         if (m%s.gt.1) stop 'Error: attempting export_Shercliff with m%s>1 in export_analytic.f90'
         call init_Face(temp,m,dir)
         select case (dir)
         case (1); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(i,:,:) = shercliff_profile(m%g(1)%c(2),m%g(1)%c(3),m%g(1)%c(2)%sc,m%g(1)%c(3)%sc,Ha,mu,dpdz)
         enddo
         case (2); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(:,i,:) = shercliff_profile(m%g(1)%c(1),m%g(1)%c(3),m%g(1)%c(1)%sc,m%g(1)%c(3)%sc,Ha,mu,dpdz)
         enddo
         case (3); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(:,:,i) = shercliff_profile(m%g(1)%c(1),m%g(1)%c(2),m%g(1)%c(1)%sc,m%g(1)%c(2)%sc,Ha,mu,dpdz)
         enddo
         case default; stop 'Error: dir must = 1,2,3 in export_Shercliff in export_analytic.f90'
         end select
         call export_raw(m,temp,str(DT%U),'shercliff_analytic',0)
         call export_processed(m,temp,str(DT%U),'shercliff_analytic',1)
       end subroutine

       subroutine export_Hunt(m,Ha,dir,DT)
         implicit none
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Ha
         integer,intent(in) :: dir
         type(dir_tree),intent(in) :: DT
         type(SF) :: temp
         real(cp) :: mu,dpdz,d_B
         integer :: i
         mu = 1.0_cp
         d_B = 1.0_cp
         dpdz = -1.0_cp
         if (m%s.gt.1) stop 'Error: attempting export_Hunt with m%s>1 in export_analytic.f90'
         call init_Face(temp,m,dir)
         select case (dir)
         case (1); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(i,:,:) = Hunt_profile(m%g(1)%c(2),m%g(1)%c(3),m%g(1)%c(2)%sc,m%g(1)%c(3)%sc,d_B,Ha,mu,dpdz)
         enddo
         case (2); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(:,i,:) = Hunt_profile(m%g(1)%c(1),m%g(1)%c(3),m%g(1)%c(1)%sc,m%g(1)%c(3)%sc,d_B,Ha,mu,dpdz)
         enddo
         case (3); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(:,:,i) = Hunt_profile(m%g(1)%c(1),m%g(1)%c(2),m%g(1)%c(1)%sc,m%g(1)%c(2)%sc,d_B,Ha,mu,dpdz)
         enddo
         case default; stop 'Error: dir must = 1,2,3 in export_Hunt in export_analytic.f90'
         end select
         call export_raw(m,temp,str(DT%U),'Hunt_analytic',0)
         call export_processed(m,temp,str(DT%U),'Hunt_analytic',1)
       end subroutine

       subroutine export_SH(m,u_numerical,Ha,d_B,dpdh,dir,DT)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: u_numerical
         real(cp),intent(in) :: Ha,d_B,dpdh
         integer,intent(in) :: dir
         type(dir_tree),intent(in) :: DT
         type(SF) :: temp
         real(cp) :: mu,dpdz
         integer :: i
         mu = 1.0_cp
         if (m%s.gt.1) stop 'Error: attempting export_SH with m%s>1 in export_analytic.f90'
         call init_Face(temp,m,dir)
         select case (dir)
         case (1); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(i,:,:) = SH_profile(m%g(1)%c(2),m%g(1)%c(3),m%g(1)%c(2)%sc,m%g(1)%c(3)%sc,d_B,Ha,mu,dpdh)
         enddo
         case (2); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(:,i,:) = SH_profile(m%g(1)%c(1),m%g(1)%c(3),m%g(1)%c(1)%sc,m%g(1)%c(3)%sc,d_B,Ha,mu,dpdh)
         enddo
         case (3); do i=1,m%g(1)%c(dir)%sn
         temp%RF(1)%f(:,:,i) = SH_profile(m%g(1)%c(1),m%g(1)%c(2),m%g(1)%c(1)%sc,m%g(1)%c(2)%sc,d_B,Ha,mu,dpdh)
         enddo
         case default; stop 'Error: dir must = 1,2,3 in export_SH in export_analytic.f90'
         end select
         call export_raw(m,temp,str(DT%U),'SH_analytic',0)
         call export_raw(m,u_numerical,str(DT%U),'SH_numerical',0)
         call export_processed(m,temp,str(DT%U),'SH_analytic',1)
         call export_processed(m,u_numerical,str(DT%U),'SH_numerical',1)
         call delete(temp)
       end subroutine



       end module