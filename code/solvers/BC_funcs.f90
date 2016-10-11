       module BC_funcs_mod
       use current_precision_mod
       use mesh_mod
       use block_mod
       use boundary_conditions_mod
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

       subroutine Dirichlet_BCs_SF(f,m)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,f%s
           call init_Dirichlet(f%BF(i)%BCs,m%B(i)); call init(f%BF(i)%BCs,0.0_cp)
         enddo
       end subroutine

       subroutine Dirichlet_BCs_VF(f,m)
         implicit none
         type(VF),intent(inout) :: f
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,f%x%s
           call init_Dirichlet(f%x%BF(i)%BCs,m%B(i)); call init(f%x%BF(i)%BCs,0.0_cp)
           call init_Dirichlet(f%y%BF(i)%BCs,m%B(i)); call init(f%y%BF(i)%BCs,0.0_cp)
           call init_Dirichlet(f%z%BF(i)%BCs,m%B(i)); call init(f%z%BF(i)%BCs,0.0_cp)
         enddo
       end subroutine

       subroutine Neumann_BCs_SF(f,m)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,f%s
           call init_Neumann(f%BF(i)%BCs,m%B(i)); call init(f%BF(i)%BCs,0.0_cp)
         enddo
       end subroutine

       subroutine Neumann_BCs_VF(f,m)
         implicit none
         type(VF),intent(inout) :: f
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,f%x%s
           call init_Neumann(f%x%BF(i)%BCs,m%B(i)); call init(f%x%BF(i)%BCs,0.0_cp)
           call init_Neumann(f%y%BF(i)%BCs,m%B(i)); call init(f%y%BF(i)%BCs,0.0_cp)
           call init_Neumann(f%z%BF(i)%BCs,m%B(i)); call init(f%z%BF(i)%BCs,0.0_cp)
         enddo
       end subroutine

       subroutine make_periodic_SF(f,m,periodic_dir)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         integer,dimension(3),intent(in) :: periodic_dir
         integer :: i,k
         do k=1,3
           if ((periodic_dir(k).ne.1).and.(periodic_dir(k).ne.0)) then
            stop 'Error: periodic_dir must = 1,0 in make_periodic_SF in BC_funcs.f90'
           endif
         enddo
         do i=1,m%s; do k=1,3
           if (periodic_dir(k).eq.1) call makePeriodic_SF(f%BF(i)%BCs,m%B(i),k)
         enddo; enddo
       end subroutine

       subroutine make_periodic_VF(f,m,periodic_dir)
         implicit none
         type(VF),intent(inout) :: f
         type(mesh),intent(in) :: m
         integer,dimension(3),intent(in) :: periodic_dir
         integer :: i,k
         do k=1,3
           if ((periodic_dir(k).ne.1).and.(periodic_dir(k).ne.0)) then
            stop 'Error: periodic_dir must = 1,0 in make_periodic_VF in BC_funcs.f90'
           endif
         enddo
         do i=1,m%s; do k=1,3
           if (periodic_dir(k).eq.1) then
            call makePeriodic_VF(f%x%BF(i)%BCs,f%y%BF(i)%BCs,f%z%BF(i)%BCs,m%B(i),k)
           endif
         enddo; enddo
       end subroutine

       subroutine makePeriodic_SF(f_bcs,B,dir)
         implicit none
         type(boundary_conditions),intent(inout) :: f_bcs
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         select case (dir)
         case (1); call init_periodic(f_bcs,B,1)
                   call init_periodic(f_bcs,B,2)
         case (2); call init_periodic(f_bcs,B,3)
                   call init_periodic(f_bcs,B,4)
         case (3); call init_periodic(f_bcs,B,5)
                   call init_periodic(f_bcs,B,6)
         case default; stop 'Error: dir must = 1,2,3 in makePeriodic_SF in BC_funcs.f90'
         end select
       end subroutine

       subroutine makePeriodic_VF(fx_BCs,fy_BCs,fz_BCs,B,dir)
         implicit none
         type(boundary_conditions),intent(inout) :: fx_BCs,fy_BCs,fz_BCs
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         select case (dir)
         case (1);call init_periodic(fx_BCs,B,1)
                  call init_periodic(fy_BCs,B,1)
                  call init_periodic(fz_BCs,B,1)
                  call init_periodic(fx_BCs,B,2)
                  call init_periodic(fy_BCs,B,2)
                  call init_periodic(fz_BCs,B,2)
         case (2);call init_periodic(fx_BCs,B,3)
                  call init_periodic(fy_BCs,B,3)
                  call init_periodic(fz_BCs,B,3)
                  call init_periodic(fx_BCs,B,4)
                  call init_periodic(fy_BCs,B,4)
                  call init_periodic(fz_BCs,B,4)
         case (3);call init_periodic(fx_BCs,B,5)
                  call init_periodic(fy_BCs,B,5)
                  call init_periodic(fz_BCs,B,5)
                  call init_periodic(fx_BCs,B,6)
                  call init_periodic(fy_BCs,B,6)
                  call init_periodic(fz_BCs,B,6)
         case default; stop 'Error: dir must = 1,2,3 in makePeriodic_VF in BC_funcs.f90'
         end select
       end subroutine

       end module