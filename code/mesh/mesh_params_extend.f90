       module mesh_params_extend_mod
       use IO_tools_mod
       use mesh_quality_params_mod
       use segment_mod
       implicit none

       private
       public :: mesh_params
       public :: init,add

       interface init_base;   module procedure init_base_size;     end interface
       interface init_ext;    module procedure init_ext_size;      end interface
       interface add_base;    module procedure add_base_MP;        end interface
       interface add_ext;     module procedure add_ext_MP;         end interface

       contains

         call init(MP,1,MQP)
         call add_base(MP,seg_1d(1,'grid_Roberts_B'        ,10,-1.0_cp,1.0_cp))
         call add_base(MP,seg_1d(3,'grid_Roberts_B'        ,10,-1.0_cp,1.0_cp))
         call add_base(MP,seg_1d(2,'grid_Roberts_R'        ,10,-1.0_cp,1.0_cp))
         call add_ext(MP,seg_1d(2,'ext_prep_Roberts_C2F_IO',10,1.0_cp,1.0_cp))
         call add_ext(MP,seg_1d(1,'ext_Roberts_B_IO'       ,10,0.5_cp))
         call add_ext(MP,seg_1d(3,'ext_Roberts_B_IO'       ,10,0.5_cp))
         call add_ext(MP,seg_1d(2,'ext_prep_Roberts_B_IO'  ,10,0.5_cp))
         call add_ext(MP,seg_1d(1,'ext_Roberts_near_IO'    ,10,0.5_cp))
         call add_ext(MP,seg_1d(3,'ext_Roberts_near_IO'    ,10,0.5_cp))
         call add_ext(MP,seg_1d(2,'ext_prep_Roberts_R_IO'  ,10,0.5_cp))
         call add_ext(MP,seg_1d(2,'ext_app_Roberts_L_IO'   ,10,0.5_cp))

       subroutine init_base_size(MP,N,MQP)
         implicit none
         type(mesh_params),intent(inout) :: this
         type(mesh_params),intent(in) :: that
         integer :: i_iter
         call delete(this)
         call init(this%mqp,that%mqp)
         if (allocated(that%s_base)) then
           allocate(this%s_base(size(that%s_base)))
           do i_iter=1,size(this%s_base)
             call init(this%s_base(i_iter),that%s_base(i_iter))
           enddo
         endif
         if (allocated(that%s_ext)) then
           allocate(this%s_ext(size(that%s_ext)))
           do i_iter=1,size(this%s_ext)
             call init(this%s_ext(i_iter),that%s_ext(i_iter))
           enddo
         endif
         this%n_base = that%n_base
         this%n_ext = that%n_ext
       end subroutine

       end module