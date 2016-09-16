       module init_Tfield_mod
       use current_precision_mod
       use SF_mod
       use IO_SF_mod
       use mesh_mod
       implicit none

       private
       public :: initTfield
       integer :: preDefinedT_ICs = 1 ! NOTE: All cases use B_induced = 0
       !                              0 : User-defined case (no override)
       !                              1 : Uniform

       contains

       subroutine initTfield(T,m,restartT,dir)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         logical,intent(in) :: restartT
         character(len=*),intent(in) :: dir
         if (restartT) then
           call initRestartT(T,m,dir)
         elseif (preDefinedT_ICs.ne.0) then
           call initPreDefinedT(T)
         else
           call initUserTfield(T)
         endif
       end subroutine

       subroutine initRestartT(T,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: T
         type(mesh) :: temp
         call init(temp,m)
         call import_3D_1C(temp,T,dir,'Tc',1)
         call delete(temp)
       end subroutine

       subroutine initPreDefinedT(T)
         implicit none
         type(SF),intent(inout) :: T
         select case (preDefinedT_ICs)
         case (1); call uniformTfield(T)
         case default
           write(*,*) 'Incorrect preDefinedT_ICs case in initTfield.'; stop
         end select
       end subroutine

       subroutine uniformTfield(T)
         implicit none
         type(SF),intent(inout) :: T
         call assign(T,0.0_cp)
       end subroutine

       subroutine initUserTfield(T)
         implicit none
         type(SF),intent(inout) :: T
         call uniformTfield(T)
       end subroutine

       end module
