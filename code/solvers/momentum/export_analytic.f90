       module export_analytic_mod
       use current_precision_mod
       use SF_mod
       use VF_mod
       use IO_tools_mod
       use mesh_mod
       use profile_funcs_mod
       use export_raw_processed_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       implicit none

       private
       public :: export_Shercliff
       public :: export_Hunt
       public :: export_SH
       public :: export_numerical_flow_rate

       contains

       subroutine export_Shercliff(m,u_numerical,Ha,dpdh,dir,DT)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: u_numerical
         real(cp),intent(in) :: Ha,dpdh
         integer,intent(in) :: dir
         type(dir_tree),intent(in) :: DT
         type(SF) :: temp
         real(cp) :: mu
         integer :: i
         mu = 1.0_cp
         if (m%s.gt.1) stop 'Error: attempting export_SH with m%s>1 in export_analytic.f90'
         call init_Face(temp,m,dir)
         select case (dir)
         case (1); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(i,:,:) = Shercliff_profile(m%B(1)%g%c(2),m%B(1)%g%c(3),m%B(1)%g%c(2)%sc,m%B(1)%g%c(3)%sc,Ha,mu,dpdh)
         enddo
         case (2); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(:,i,:) = Shercliff_profile(m%B(1)%g%c(1),m%B(1)%g%c(3),m%B(1)%g%c(1)%sc,m%B(1)%g%c(3)%sc,Ha,mu,dpdh)
         enddo
         case (3); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(:,:,i) = Shercliff_profile(m%B(1)%g%c(1),m%B(1)%g%c(2),m%B(1)%g%c(1)%sc,m%B(1)%g%c(2)%sc,Ha,mu,dpdh)
         enddo
         case default; stop 'Error: dir must = 1,2,3 in export_SH in export_analytic.f90'
         end select
         call export_raw(m,temp,str(DT%U%field),'Shercliff_analytic',0)
         call export_raw(m,u_numerical,str(DT%U%field),'Shercliff_numerical',0)
         call export_processed(m,temp,str(DT%U%field),'Shercliff_analytic',1)
         call export_processed(m,u_numerical,str(DT%U%field),'Shercliff_numerical',1)
         call delete(temp)
       end subroutine


       subroutine export_Hunt(m,u_numerical,Ha,dpdh,dir,DT)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: u_numerical
         real(cp),intent(in) :: Ha,dpdh
         integer,intent(in) :: dir
         type(dir_tree),intent(in) :: DT
         type(SF) :: temp
         real(cp) :: mu
         integer :: i
         mu = 1.0_cp
         if (m%s.gt.1) stop 'Error: attempting export_SH with m%s>1 in export_analytic.f90'
         call init_Face(temp,m,dir)
         select case (dir)
         case (1); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(i,:,:) = Hunt_profile(m%B(1)%g%c(2),m%B(1)%g%c(3),m%B(1)%g%c(2)%sc,m%B(1)%g%c(3)%sc,Ha,mu,dpdh)
         enddo
         case (2); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(:,i,:) = Hunt_profile(m%B(1)%g%c(1),m%B(1)%g%c(3),m%B(1)%g%c(1)%sc,m%B(1)%g%c(3)%sc,Ha,mu,dpdh)
         enddo
         case (3); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(:,:,i) = Hunt_profile(m%B(1)%g%c(1),m%B(1)%g%c(2),m%B(1)%g%c(1)%sc,m%B(1)%g%c(2)%sc,Ha,mu,dpdh)
         enddo
         case default; stop 'Error: dir must = 1,2,3 in export_SH in export_analytic.f90'
         end select
         call export_raw(m,temp,str(DT%U%field),'Hunt_analytic',0)
         call export_raw(m,u_numerical,str(DT%U%field),'Hunt_numerical',0)
         call export_processed(m,temp,str(DT%U%field),'Hunt_analytic',1)
         call export_processed(m,u_numerical,str(DT%U%field),'Hunt_numerical',1)
         call delete(temp)
       end subroutine

       subroutine export_SH(m,u_numerical,Ha,d_B,dpdh,dir,DT)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: u_numerical
         real(cp),intent(in) :: Ha,d_B,dpdh
         integer,intent(in) :: dir
         type(dir_tree),intent(in) :: DT
         type(SF) :: temp
         real(cp) :: mu
         integer :: i
         mu = 1.0_cp
         if (m%s.gt.1) stop 'Error: attempting export_SH with m%s>1 in export_analytic.f90'
         call init_Face(temp,m,dir)
         select case (dir)
         case (1); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(i,:,:) = SH_profile(m%B(1)%g%c(2),m%B(1)%g%c(3),m%B(1)%g%c(2)%sc,m%B(1)%g%c(3)%sc,d_B,Ha,mu,dpdh)
         enddo
         case (2); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(:,i,:) = SH_profile(m%B(1)%g%c(1),m%B(1)%g%c(3),m%B(1)%g%c(1)%sc,m%B(1)%g%c(3)%sc,d_B,Ha,mu,dpdh)
         enddo
         case (3); do i=1,m%B(1)%g%c(dir)%sn
         temp%BF(1)%GF%f(:,:,i) = SH_profile(m%B(1)%g%c(1),m%B(1)%g%c(2),m%B(1)%g%c(1)%sc,m%B(1)%g%c(2)%sc,d_B,Ha,mu,dpdh)
         enddo
         case default; stop 'Error: dir must = 1,2,3 in export_SH in export_analytic.f90'
         end select
         call export_raw(m,temp,str(DT%U%field),'SH_analytic',0)
         call export_raw(m,u_numerical,str(DT%U%field),'SH_numerical',0)
         call export_processed(m,temp,str(DT%U%field),'SH_analytic',1)
         call export_processed(m,u_numerical,str(DT%U%field),'SH_numerical',1)
         call delete(temp)
       end subroutine

       subroutine export_numerical_flow_rate(m,u_numerical,Re,DT,u_temp)
         implicit none
         type(SF),intent(in) :: u_numerical
         real(cp),intent(in) :: Re
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(SF),intent(inout) :: u_temp
         real(cp) :: Q
         integer :: un
         un = new_and_open(str(DT%LDC),'numerical_flow_rate')
         ! call boundary_flux(Q,u_numerical,m,u_temp) ! Cancels in and output!
         ! Just need outflux, not net flux
         Q = plane_sum_x(u_numerical%BF(1)%GF,m%B(1)%g,2,1.0_cp)/Re
         write(*,*) 'Numerical flow rate = ',Q
         write(un,*) 'Numerical flow rate = ',Q
         call close_and_message(un,str(DT%LDC),'numerical_flow_rate')
       end subroutine

       end module