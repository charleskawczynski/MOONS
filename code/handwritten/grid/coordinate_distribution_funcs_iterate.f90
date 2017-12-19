       module coordinate_distribution_funcs_iterate_mod
       use current_precision_mod
       use array_mod
       use array_extend_mod
       use mesh_quality_params_mod
       use coordinate_distribution_funcs_mod
       implicit none

       private
       public :: uniform_iterate
       public :: linspace_iterate
       public :: uniformLeft_iterate
       public :: uniformRight_iterate
       public :: robertsLeft_iterate
       public :: robertsRight_iterate
       public :: robertsBoth_iterate
       public :: cluster_iterate
       public :: needs_more_points
       interface uniform_iterate;      module procedure uniform_A;              end interface
       interface linspace_iterate;     module procedure linspace_A;             end interface
       interface uniformLeft_iterate;  module procedure uniformLeft_A;          end interface
       interface uniformRight_iterate; module procedure uniformRight_A;         end interface
       interface robertsLeft_iterate;  module procedure robertsLeft_A;          end interface
       interface robertsRight_iterate; module procedure robertsRight_A;         end interface
       interface robertsBoth_iterate;  module procedure robertsBoth_A;          end interface
       interface cluster_iterate;      module procedure cluster_A;              end interface
       interface needs_more_points;    module procedure needs_more_points_A;    end interface
       interface needs_more_points;    module procedure needs_more_points_func; end interface

       contains

       function uniform_A(hmin,hmax,N) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),intent(in) :: hmin,hmax
         type(array) :: hn
         call init(hn,uniform(hmin,hmax,N))
       end function

       function linspace_A(hmin,hmax,N) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),intent(in) :: hmin,hmax
         type(array) :: hn
         call init(hn,linspace(hmin,hmax,N))
       end function

       function uniformLeft_A(hstart,dh,N) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),intent(in) :: hstart,dh
         type(array) :: hn
         call init(hn,uniformLeft(hstart,dh,N))
       end function

       function uniformRight_A(hstart,dh,N) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),intent(in) :: hstart,dh
         type(array) :: hn
         call init(hn,uniformRight(hstart,dh,N))
       end function

       function robertsLeft_A(hmin,hmax,N,beta,MQP) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),intent(in) :: hmin,hmax,beta
         type(mesh_quality_params),intent(in) :: MQP
         type(array) :: hn
         integer :: i,temp
         temp = N
         do i=1,MQP%N_iter
           call init(hn,robertsLeft(hmin,hmax,temp,beta))
           if (needs_more_points(hn,MQP)) then; temp = temp+1
           else; exit
           endif
         enddo
       end function

       function robertsRight_A(hmin,hmax,N,beta,MQP) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),intent(in) :: hmin,hmax,beta
         type(mesh_quality_params),intent(in) :: MQP
         type(array) :: hn
         integer :: i,temp
         temp = N
         do i=1,MQP%N_iter
           call init(hn,robertsRight(hmin,hmax,temp,beta))
           if (needs_more_points(hn,MQP)) then; temp = temp+1
           else; exit
           endif
         enddo
       end function

       function robertsBoth_A(hmin,hmax,N,beta,MQP) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),intent(in) :: hmin,hmax,beta
         type(mesh_quality_params),intent(in) :: MQP
         type(array) :: hn
         integer :: i,temp
         temp = N
         do i=1,MQP%N_iter
           call init(hn,robertsBoth(hmin,hmax,temp,beta))
           if (needs_more_points(hn,MQP)) then; temp = temp+1
           else; exit
           endif
         enddo
       end function

       function cluster_A(hmin,hmax,N,yc,tau,MQP) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),intent(in) :: hmin,hmax,yc,tau
         type(mesh_quality_params),intent(in) :: MQP
         type(array) :: hn
         integer :: i,temp
         temp = N
         do i=1,MQP%N_iter
           call init(hn,cluster(hmin,hmax,temp,yc,tau))
           if (needs_more_points(hn,MQP)) then; temp = temp+1
           else; exit
           endif
         enddo
       end function

       function needs_more_points_A(hn,MQP) result(L)
         implicit none
         type(array),intent(in) :: hn
         type(mesh_quality_params),intent(in) :: MQP
         logical :: L
         L = needs_more_points(hn%f,hn%N,MQP)
       end function

       function needs_more_points_func(hn,N,MQP) result(L)
         implicit none
         real(cp),dimension(N),intent(in) :: hn
         type(mesh_quality_params),intent(in) :: MQP
         integer,intent(in) :: N
         real(cp),dimension(N-1) :: dhn
         real(cp),dimension(N-2) :: R
         integer :: i
         logical :: L
         L = .false.
         if (N.gt.2) then
           do i=1,N-1; dhn(i) = hn(i+1)-hn(i); enddo
           do i=1,N-2; R(i) = maxval((/dhn(i)/dhn(i+1),dhn(i+1)/dhn(i)/)); enddo
           L = maxval(R).gt.MQP%max_mesh_stretch_ratio
         endif
       end function

       end module