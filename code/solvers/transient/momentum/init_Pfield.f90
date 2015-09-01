       module init_Pfield_mod
       use IO_SF_mod
       use grid_mod
       use SF_mod
       use BCs_mod
       use init_Ufield_mod ! for restartU
       implicit none

       private
       public :: init_Pfield,restartP

       integer,parameter :: preDefinedP_ICs = 1
       logical,parameter :: restartP = restartU

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

       subroutine init_Pfield(p,g,dir)
         implicit none
         type(SF),intent(inout) :: p
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         if (restartP) then
               call initRestartPfield(p,g,dir)
         elseif (preDefinedP_ICs.ne.0) then
               call initPreDefinedPfield(p)
         else; call initUserPfield(p)
         endif
       end subroutine
       
       subroutine initRestartPfield(p,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         type(SF),intent(inout) :: p
         type(grid) :: temp
         call init(temp,g)
         call import_1C_SF(temp,p,dir,'pci',0)
         call delete(temp)
       end subroutine
       
       subroutine initPreDefinedPfield(p)
         implicit none
         type(SF),intent(inout) :: p
         call assign(p,0.0_cp)
       end subroutine

       subroutine initUserPfield(p)
         implicit none
         type(SF),intent(inout) :: p
         call assign(p,0.0_cp)
       end subroutine

       end module