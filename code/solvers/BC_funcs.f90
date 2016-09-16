       module BC_funcs_mod
       use current_precision_mod
       use mesh_mod
       use BCs_mod
       use SF_mod
       use VF_mod
       implicit none

       private
       public :: Dirichlet_BCs
       public :: Neumann_BCs
       public :: make_periodic

       interface Dirichlet_BCs;   module procedure Dirichlet_BCs_SF;   end interface
       interface Dirichlet_BCs;   module procedure Dirichlet_BCs_VF;   end interface

       interface Neumann_BCs;     module procedure Neumann_BCs_SF;     end interface
       interface Neumann_BCs;     module procedure Neumann_BCs_VF;     end interface

       interface make_periodic;   module procedure make_periodic_SF;   end interface
       interface make_periodic;   module procedure make_periodic_VF;   end interface

       contains

       subroutine Dirichlet_BCs_SF(f)
         implicit none
         type(SF),intent(inout) :: f
         integer :: i
         do i=1,f%s
           call init_Dirichlet(f%RF(i)%b); call init(f%RF(i)%b,0.0_cp)
         enddo
       end subroutine

       subroutine Dirichlet_BCs_VF(f)
         implicit none
         type(VF),intent(inout) :: f
         integer :: i
         do i=1,f%x%s
           call init_Dirichlet(f%x%RF(i)%b); call init(f%x%RF(i)%b,0.0_cp)
           call init_Dirichlet(f%y%RF(i)%b); call init(f%y%RF(i)%b,0.0_cp)
           call init_Dirichlet(f%z%RF(i)%b); call init(f%z%RF(i)%b,0.0_cp)
         enddo
       end subroutine

       subroutine Neumann_BCs_SF(f)
         implicit none
         type(SF),intent(inout) :: f
         integer :: i
         do i=1,f%s
           call init_Neumann(f%RF(i)%b); call init(f%RF(i)%b,0.0_cp)
         enddo
       end subroutine

       subroutine Neumann_BCs_VF(f)
         implicit none
         type(VF),intent(inout) :: f
         integer :: i
         do i=1,f%x%s
           call init_Neumann(f%x%RF(i)%b); call init(f%x%RF(i)%b,0.0_cp)
           call init_Neumann(f%y%RF(i)%b); call init(f%y%RF(i)%b,0.0_cp)
           call init_Neumann(f%z%RF(i)%b); call init(f%z%RF(i)%b,0.0_cp)
         enddo
       end subroutine

       subroutine make_periodic_SF(f,periodic_dir)
         implicit none
         type(SF),intent(inout) :: f
         integer,dimension(3),intent(in) :: periodic_dir
         integer :: i,k
         do k=1,3
           if ((periodic_dir(k).ne.1).and.(periodic_dir(k).ne.0)) then
            stop 'Error: periodic_dir must = 1,0 in make_periodic_SF in BC_funcs.f90'
           endif
         enddo
         do i=1,f%s; do k=1,3
           if (periodic_dir(k).eq.1) call makePeriodic_SF(f%RF(i)%b,k)
         enddo; enddo
       end subroutine

       subroutine make_periodic_VF(f,periodic_dir)
         implicit none
         type(VF),intent(inout) :: f
         integer,dimension(3),intent(in) :: periodic_dir
         integer :: i,k
         do k=1,3
           if ((periodic_dir(k).ne.1).and.(periodic_dir(k).ne.0)) then
            stop 'Error: periodic_dir must = 1,0 in make_periodic_VF in BC_funcs.f90'
           endif
         enddo
         do i=1,f%x%s; do k=1,3
           if (periodic_dir(k).eq.1) call makePeriodic_VF(f%x%RF(i)%b,f%y%RF(i)%b,f%z%RF(i)%b,k)
         enddo; enddo
       end subroutine

       subroutine makePeriodic_SF(f_bcs,dir)
         implicit none
         type(BCs),intent(inout) :: f_bcs
         integer,intent(in) :: dir
         select case (dir)
         case (1); call init_periodic(f_bcs,1)
                   call init_periodic(f_bcs,2)
         case (2); call init_periodic(f_bcs,3)
                   call init_periodic(f_bcs,4)
         case (3); call init_periodic(f_bcs,5)
                   call init_periodic(f_bcs,6)
         case default; stop 'Error: dir must = 1,2,3 in makePeriodic_SF in BC_funcs.f90'
         end select
       end subroutine

       subroutine makePeriodic_VF(fx_BCs,fy_BCs,fz_BCs,dir)
         implicit none
         type(BCs),intent(inout) :: fx_BCs,fy_BCs,fz_BCs
         integer,intent(in) :: dir
         select case (dir)
         case (1);call init_periodic(fx_BCs,1)
                  call init_periodic(fy_BCs,1)
                  call init_periodic(fz_BCs,1)
                  call init_periodic(fx_BCs,2)
                  call init_periodic(fy_BCs,2)
                  call init_periodic(fz_BCs,2)
         case (2);call init_periodic(fx_BCs,3)
                  call init_periodic(fy_BCs,3)
                  call init_periodic(fz_BCs,3)
                  call init_periodic(fx_BCs,4)
                  call init_periodic(fy_BCs,4)
                  call init_periodic(fz_BCs,4)
         case (3);call init_periodic(fx_BCs,5)
                  call init_periodic(fy_BCs,5)
                  call init_periodic(fz_BCs,5)
                  call init_periodic(fx_BCs,6)
                  call init_periodic(fy_BCs,6)
                  call init_periodic(fz_BCs,6)
         case default; stop 'Error: dir must = 1,2,3 in makePeriodic_VF in BC_funcs.f90'
         end select
       end subroutine

       end module