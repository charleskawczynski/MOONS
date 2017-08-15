       module mesh_params_extend_mod
       use IO_tools_mod
       use mesh_quality_params_mod
       use segment_mod
       use mesh_params_mod
       implicit none

       private
       public :: init
       public :: add_base
       public :: add_ext

       interface init;        module procedure init_MQP;           end interface
       interface add_base;    module procedure add_base_MP;        end interface
       interface add_ext;     module procedure add_ext_MP;         end interface

       contains

       subroutine init_MQP(MP,MQP)
         implicit none
         type(mesh_params),intent(inout) :: MP
         type(mesh_quality_params),intent(in) :: MQP
         call init(MP%MQP,MQP)
       end subroutine

       subroutine add_base_MP(MP,S)
         implicit none
         type(mesh_params),intent(inout) :: MP
         type(segment),intent(in) :: S
         type(mesh_params) :: temp
         integer :: i
         if (allocated(MP%s_base)) then
           if (MP%n_base.lt.1) then
             write(*,*) 'Error: allocated mesh_params but size<1 in add_base_MP'
             stop 'Done in mesh_params_extend.f90'
           endif
           call init(temp,MP)
           do i=1,size(MP%s_base)
             call delete(MP%s_base(i))
           enddo
           deallocate(MP%s_base)
           MP%n_base = temp%n_base + 1
           allocate(MP%s_base(MP%n_base))
           do i=1,temp%n_base; call init(MP%s_base(i),temp%s_base(i)); enddo
           call init(MP%s_base(MP%n_base),S)
           call delete(temp)
         else
           allocate(MP%s_base(1))
           call init(MP%s_base(1),S); MP%n_base = 1
         endif
       end subroutine

       subroutine add_ext_MP(MP,S)
         implicit none
         type(mesh_params),intent(inout) :: MP
         type(segment),intent(in) :: S
         type(mesh_params) :: temp
         integer :: i
         if (allocated(MP%s_ext)) then
           if (MP%n_ext.lt.1) then
             write(*,*) 'Error: allocated mesh_params but size<1 in add_ext_MP'
             stop 'Done in mesh_params_extend.f90'
           endif
           call init(temp,MP)
           do i=1,size(MP%s_ext)
             call delete(MP%s_ext(i))
           enddo
           deallocate(MP%s_ext)
           MP%n_ext = temp%n_ext + 1
           allocate(MP%s_ext(MP%n_ext))
           do i=1,temp%n_ext; call init(MP%s_ext(i),temp%s_ext(i)); enddo
           call init(MP%s_ext(MP%n_ext),S)
           call delete(temp)
         else
           allocate(MP%s_ext(1))
           call init(MP%s_ext(1),S); MP%n_ext = 1
         endif
       end subroutine

       end module