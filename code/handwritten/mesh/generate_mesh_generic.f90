       module generate_mesh_generic_mod
       use current_precision_mod
       use mesh_params_mod
       use grid_init_mod
       use grid_continue_mod
       use grid_connect_mod
       use coordinate_stretch_parameters_mod
       use dimensionless_params_mod
       use string_mod
       use grid_mod
       use mesh_extend_mod
       use mesh_quality_params_mod
       implicit none

       private
       public :: generate_mesh_generic

       contains

       subroutine generate_mesh_generic(m,MP,DP,caller)
         implicit none
         type(mesh),intent(inout) :: m
         type(dimensionless_params),intent(in) :: DP
         type(mesh_params),intent(in) :: MP
         character(len=*),intent(in) :: caller
         type(string) :: d
         type(mesh_quality_params) :: MQP
         type(grid) :: g
         real(cp) :: a,b,L,beta,tau,yc,buffer
         integer :: j,N_cells,dir
         call init(MQP,MP%MQP)
         if (MP%n_base.gt.0) then
           do j=1,MP%n_base
             call init(d,MP%s_base(j)%distribution)
             a = MP%s_base(j)%hmin
             b = MP%s_base(j)%hmax
             N_cells = MP%s_base(j)%N_cells
             dir = MP%s_base(j)%dir
             tau = MP%s_base(j)%tau
             yc = MP%s_base(j)%yc
             buffer = MP%s_base(j)%buffer
             beta = Re_Ha_BL_1D(DP%Re*buffer,DP%Ha*buffer,a,b)
             if (N_cells.gt.0) then

                 if (str(d).eq.'grid_uniform') then;            call grid_uniform(g,a,b,N_cells,dir)
             elseif (str(d).eq.'grid_Roberts_L') then;          call grid_Roberts_L(g,a,b,N_cells,beta,dir,MQP)
             elseif (str(d).eq.'grid_Roberts_R') then;          call grid_Roberts_R(g,a,b,N_cells,beta,dir,MQP)
             elseif (str(d).eq.'grid_Roberts_B') then;          call grid_Roberts_B(g,a,b,N_cells,beta,dir,MQP)
             elseif (str(d).eq.'grid_cluster') then;            call grid_cluster(g,a,b,N_cells,yc,tau,dir,MQP)
             else
             write(*,*) 'Error: bad distribution in mesh base '//caller// ' for mesh '//str(d)
             stop 'Done in generate_mesh_generic.f90'
             endif

             endif
           enddo
         endif
         if (MP%n_ext.gt.0) then
           do j=1,MP%n_ext
             call init(d,MP%s_ext(j)%distribution)
             a = MP%s_ext(j)%hmin
             b = MP%s_ext(j)%hmax
             L = MP%s_ext(j)%L
             N_cells = MP%s_ext(j)%N_cells
             tau = MP%s_ext(j)%tau
             buffer = MP%s_ext(j)%buffer
             beta = Re_Ha_BL_1D(DP%Re*buffer,DP%Ha*buffer,a,b)
             dir = MP%s_ext(j)%dir
             if (N_cells.gt.0) then
                   if (str(d).eq.'ext_uniform_IO') then;          call ext_uniform_IO(g,N_cells,dir)
               elseif (str(d).eq.'ext_Roberts_near_IO') then;     call ext_Roberts_near_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_Roberts_far_IO') then;      call ext_Roberts_far_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_Roberts_B_IO') then;        call ext_Roberts_B_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_prep_uniform_IO') then;     call ext_prep_uniform_IO(g,N_cells,dir)
               elseif (str(d).eq.'ext_prep_Roberts_L_IO') then;   call ext_prep_Roberts_L_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_prep_Roberts_R_IO') then;   call ext_prep_Roberts_R_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_prep_Roberts_B_IO') then;   call ext_prep_Roberts_B_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_app_uniform_IO') then;      call ext_app_uniform_IO(g,N_cells,dir)
               elseif (str(d).eq.'ext_app_Roberts_L_IO') then;    call ext_app_Roberts_L_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_app_Roberts_R_IO') then;    call ext_app_Roberts_R_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_app_Roberts_B_IO') then;    call ext_app_Roberts_B_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_app_Roberts_C2F_IO') then;  call ext_app_Roberts_C2F_IO(g,L,N_cells,dir,MQP)
               elseif (str(d).eq.'ext_prep_Roberts_C2F_IO') then; call ext_prep_Roberts_C2F_IO(g,L,N_cells,dir,MQP)
               else
               write(*,*) 'Error: bad distribution in mesh extend '//caller// ' for mesh '//str(d)
               stop 'Done in generate_mesh_generic.f90'
               endif
             endif
           enddo
         endif
         call add(m,g)
         call init_props(m)
         call patch(m)
         call delete(g)
         call delete(MQP)
         call delete(d)
       end subroutine

       end module