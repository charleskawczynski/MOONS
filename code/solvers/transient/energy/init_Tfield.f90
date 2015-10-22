       module init_Tfield_mod
       use SF_mod
       use IO_SF_mod
       use mesh_mod
       implicit none

       private
       public :: initTfield
       public :: restartT

       logical,parameter :: restartT = .false. ! (induced field)

       integer,parameter :: preDefinedT_ICs = 1 ! NOTE: All cases use B_induced = 0
       !                                      0 : User-defined case (no override)
       !                                      1 : Uniform


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine initTfield(T,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: T
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
         call import_3D_1C(temp,T,dir,'Tct',1)
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
