       module mesh_simple_geometries_mod
       use current_precision_mod
       use grid_init_mod
       use grid_extend_mod
       use grid_connect_mod
       use grid_distribution_funcs_mod
       use grid_mod
       use domain_mod
       use mesh_mod
       implicit none

       private
       public :: BC_sim_mom,BC_sim_ind
       public :: BC_sim_mom_sheet,BC_sim_ind_sheet
       public :: cube_uniform,extend_cube_uniform
       public :: cube,extend_cube
       public :: straight_duct_fluid
       public :: Hunt_duct_magnetic
       public :: Shercliff_duct_magnetic
       public :: duct_with_vacuum
       public :: matrix_export_mesh

       real(cp),parameter :: PI = 3.141592653589793238462643383279502884197169399375105820974_cp

       contains

       subroutine BC_sim_mom(m,Ha)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: Ha
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m)
         if (low_Ha(Ha)) then
           N = (/30,30,30/); hmin = -1.0_cp; hmax = 1.0_cp ! For Ha = 20
           beta = hartmannBL(20.0_cp,hmin,hmax) ! For Ha = 20
         elseif (high_Ha(Ha)) then
           N = (/32,32,32/); hmin = -1.0_cp; hmax = 1.0_cp ! For Ha = 100
           beta = hartmannBL(100.0_cp,hmin,hmax) ! For Ha = 100
         else; stop 'Error: bad input to geometry in BC_sim_mom in mesh_simple_geometries.f90'
         endif
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m,g)

         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine BC_sim_ind(m_ind,m_mom,D_sigma,Ha,tw,include_vacuum)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(domain),intent(inout) :: D_sigma
         real(cp),intent(in) :: Ha,tw
         logical,intent(in) :: include_vacuum
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tf
         real(cp) :: Gamma_v
         integer :: N_w,N_v,N_extra
         call delete(m_ind)
         call init(g,m_mom%g(1))

         Gamma_v = 7.0_cp
         tf = 1.0_cp

         if (low_Ha(Ha)) then
         elseif (high_Ha(Ha)) then
         else; write(*,*) 'Ha,tw=',Ha,tw
            stop 'Error: 1 bad input to geometry in BC_sim_ind in mesh_simple_geometries.f90'
         endif

         if (low_tw(tw)) then
           if (low_Ha(Ha)) then;      N_w = 4 ! For Ha = 20
           elseif (high_Ha(Ha)) then; N_w = 6 ! For Ha = 100
           else; write(*,*) 'Ha,tw=',Ha,tw
            stop 'Error: 2 bad input to geometry in BC_sim_ind in mesh_simple_geometries.f90'
           endif
         elseif (high_tw(tw)) then
           if (low_Ha(Ha)) then;      N_w = 8 ! For Ha = 20
           elseif (high_Ha(Ha)) then; N_w = 10 ! For Ha = 100
           else; write(*,*) 'Ha,tw=',Ha,tw
            stop 'Error: 3 bad input to geometry in BC_sim_ind in mesh_simple_geometries.f90'
           endif
         else; write(*,*) 'Ha,tw=',Ha,tw
            stop 'Error: 4 bad input to geometry in BC_sim_ind in mesh_simple_geometries.f90'
         endif

         N_v = 12
         N_extra = 6 ! since no wall domain above lid

         ! Wall
         call ext_Roberts_B_IO(g,tw,N_w,1)
         call ext_Roberts_B_IO(g,tw,N_w,3)
         call ext_prep_Roberts_B_IO(g,tw,N_w,2)

         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)

         ! Vacuum
         ! Remove the following 4 lines for vacuum-absent case
         if (include_vacuum) then
           call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,1) ! x-direction
           call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,3) ! z-direction
           call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v,2) ! y-direction
           call ext_app_Roberts_L_IO (g,Gamma_v - tf,N_v+N_extra,2) ! y-direction
         endif

         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       function low_Ha(Ha) result(L)
         implicit none
         real(cp),intent(in) :: Ha
         real(cp) :: Ha_target,tol
         logical :: L
         Ha_target = 20.0_cp; tol = 10.0_cp**(-10.0_cp)
         L = (Ha.gt.Ha_target-tol).and.(Ha.lt.Ha_target+tol)
       end function
       function high_Ha(Ha) result(L)
         implicit none
         real(cp),intent(in) :: Ha
         real(cp) :: Ha_target,tol
         logical :: L
         Ha_target = 100.0_cp; tol = 10.0_cp**(-10.0_cp)
         L = (Ha.gt.Ha_target-tol).and.(Ha.lt.Ha_target+tol)
       end function
       function low_tw(tw) result(L)
         implicit none
         real(cp),intent(in) :: tw
         real(cp) :: tw_target,tol
         logical :: L
         tw_target = 0.05_cp; tol = 10.0_cp**(-10.0_cp)
         L = (tw.gt.tw_target-tol).and.(tw.lt.tw_target+tol)
       end function
       function high_tw(tw) result(L)
         implicit none
         real(cp),intent(in) :: tw
         real(cp) :: tw_target,tol
         logical :: L
         tw_target = 0.5_cp; tol = 10.0_cp**(-10.0_cp)
         L = (tw.gt.tw_target-tol).and.(tw.lt.tw_target+tol)
       end function


       subroutine BC_sim_mom_sheet(m,Ha)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: Ha
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m)
         hmin = -1.0_cp; hmax = 1.0_cp
         if (low_Ha(Ha)) then
           N = (/30,1,30/) ! For Ha = 20
           beta = hartmannBL(20.0_cp,hmin,hmax) ! For Ha = 20
         elseif (high_Ha(Ha)) then
           N = (/32,1,32/) ! For Ha = 100
           beta = hartmannBL(100.0_cp,hmin,hmax) ! For Ha = 100
         else; stop 'Error: bad input to geometry in BC_sim_mom in mesh_simple_geometries.f90'
         endif
         ! hmin(2) = -0.5_cp
         ! hmax(2) = 0.5_cp
         hmin(2) = -0.005_cp
         hmax(2) = 0.005_cp
         ! hmin(2) = 0.0_cp; hmax(2) = 0.0_cp; N(2) = 0
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m,g)

         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine BC_sim_ind_sheet(m_ind,m_mom,D_sigma,Ha,tw,include_vacuum)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(domain),intent(inout) :: D_sigma
         real(cp),intent(in) :: Ha,tw
         logical,intent(in) :: include_vacuum
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tf
         real(cp) :: Gamma_v
         integer :: N_w,N_v,N_extra
         integer :: i
         call delete(m_ind)
         call init(g,m_mom%g(1))

         Gamma_v = 7.0_cp
         tf = 1.0_cp

         if (low_Ha(Ha)) then
         elseif (high_Ha(Ha)) then
         else; write(*,*) 'Ha,tw=',Ha,tw
            stop 'Error: 1 bad input to geometry in BC_sim_ind in mesh_simple_geometries.f90'
         endif

         if (low_tw(tw)) then
           if (low_Ha(Ha)) then;      N_w = 4 ! For Ha = 20
           elseif (high_Ha(Ha)) then; N_w = 6 ! For Ha = 100
           else; write(*,*) 'Ha,tw=',Ha,tw
            stop 'Error: 2 bad input to geometry in BC_sim_ind in mesh_simple_geometries.f90'
           endif
         elseif (high_tw(tw)) then
           if (low_Ha(Ha)) then;      N_w = 8 ! For Ha = 20
           elseif (high_Ha(Ha)) then; N_w = 10 ! For Ha = 100
           else; write(*,*) 'Ha,tw=',Ha,tw
            stop 'Error: 3 bad input to geometry in BC_sim_ind in mesh_simple_geometries.f90'
           endif
         else; write(*,*) 'Ha,tw=',Ha,tw
            stop 'Error: 4 bad input to geometry in BC_sim_ind in mesh_simple_geometries.f90'
         endif

         N_v = 12
         N_extra = 6 ! since no wall domain above lid

         ! Wall
         i = 1; call ext_Roberts_B_IO(g,tw,N_w,i)
         i = 3; call ext_Roberts_B_IO(g,tw,N_w,i)

         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)

         ! Vacuum
         ! Remove the following 4 lines for vacuum-absent case
         if (include_vacuum) then
           i=1; call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,i) ! x-direction
           i=3; call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,i) ! z-direction
           i=2; call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v+N_extra,i)
         endif

         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine


       subroutine duct_with_vacuum(m_ind,m_mom,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tw,tf
         real(cp) :: Gamma_f,Gamma_w,Gamma_v
         integer :: N_w,N_v
         call delete(m_ind)
         call init(g,m_mom%g(1))

         Gamma_f = 1.0_cp
         Gamma_w = Gamma_f + tw
         Gamma_v = 7.0_cp
         tf = 1.0_cp

         tw = 0.1_cp
         N_w = 10
         N_v = 15

         ! Wall
         call ext_Roberts_B_IO(g,tw,N_w,2)
         call ext_Roberts_B_IO(g,tw,N_w,3)

         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)

         ! Vacuum
         ! Remove the following 4 lines for vacuum-absent case
         ! call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,2) ! y-direction
         ! call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,3) ! z-direction

         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       subroutine cube_uniform(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer :: i
         integer,dimension(3) :: N
         call delete(m)
         N = (/45,45,45/)
         ! N = (/67,67,27/)
         hmin = -1.0_cp; hmax = 1.0_cp
         ! hmin = -0.5_cp; hmax = 0.5_cp
         ! hmin(1) = -1.0_cp; hmax(1) = 1.0_cp
         ! hmin(3) = -0.5_cp; hmax(3) = 0.5_cp

         i= 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i= 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i= 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine cube(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Ha,Re
         Ha = 20.0_cp; Re = 1000.0_cp
         call delete(m)
         N = (/30,30,30/); hmin = -1.0_cp; hmax = 1.0_cp
         ! beta = reynoldsBL(Re,hmin,hmax)
         beta = hartmannBL(Ha,hmin,hmax)

         ! i= 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         ! i= 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         ! i= 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)

         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         ! i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         ! i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine extend_cube_uniform(m,m_in)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh),intent(in) :: m_in
         type(grid) :: g
         integer :: i
         integer,dimension(3) :: N
         call delete(m)
         call init(g,m_in%g(1))
         N = 11
         i = 1; call ext_uniform_IO(g,N(i),i)
         i = 2; call ext_uniform_IO(g,N(i),i)
         i = 3; call ext_uniform_IO(g,N(i),i)
         call init(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine extend_cube(m,m_in)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh),intent(in) :: m_in
         type(grid) :: g
         integer :: i
         integer,dimension(3) :: N
         real(cp),dimension(3) :: L
         call delete(m)
         call init(g,m_in%g(1))
         N = 2; L = 1.0_cp
         i = 1; call ext_Roberts_near_IO(g,L(i),N(i),i)
         i = 2; call ext_Roberts_near_IO(g,L(i),N(i),i)
         i = 3; call ext_Roberts_near_IO(g,L(i),N(i),i)
         call init(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine straight_duct_fluid(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Ha,Re
         Ha = 60.0_cp; Re = 10.0_cp**(7.0_cp)
         call delete(m)
         N = (/256,200,1/); hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp; hmax(1) = 60.0_cp
         ! beta = reynoldsBL(Re,hmin,hmax)
         beta = hartmannBL(Ha,hmin,hmax)

         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         ! i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)

         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine matrix_export_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: N
         integer :: i
         call delete(m)
         N = (/3,3,3/); hmin = -1.0_cp; hmax = 1.0_cp
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine Hunt_duct_magnetic(m_ind,m_mom,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tw
         integer :: N_w
         call delete(m_ind)
         call init(g,m_mom%g(1))
         tw = 0.01_cp
         N_w = 3
         ! Wall
         call ext_Roberts_near_IO(g,tw,N_w,2) ! Comment / uncomment for Shercliff / Hunt flow
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)
         ! Define domain for magnetic field domain
         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       subroutine Shercliff_duct_magnetic(m_ind,m_mom,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         integer :: N_w
         call delete(m_ind)
         call init(g,m_mom%g(1))
         N_w = 3
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)
         ! Define domain for magnetic field domain
         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       end module