       module timeMarchingMethods_mod
       use vectorField_mod
       use scalarField_mod
       use grid_mod
       
       implicit none
       private
       
       public :: explicitEuler,timeCentered,RK4


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type explicitEuler
         type(vectorField) :: fn
         type(scalarField) :: f_SF
       end type


       contains

       ! ******************* INIT/DELETE ***********************

       subroutine solveExplicitEuler(unp1,un,fn,dt)
         ! Solves 
         ! 
         !        n+1    n
         !       u    - u     n
         !       --------  = f
         !          dt
         implicit none
         type(vectorField),intent(inout) :: unp1,fn
         type(vectorField),intent(in) :: un
         real(cp),intent(in) :: dt
         call multiply(fn,dt)
         call subtract(unp1,un,fn)
       end subroutine

       subroutine solveExplicitEuler(unp1,un,f,dt)
         ! Solves 
         ! 
         !        n+1    n
         !       u    - u     n
         !       --------  = f
         !          dt
         implicit none
         routine,intent(in) :: f
         type(vectorField),intent(inout) :: unp1,fn
         type(vectorField),intent(in) :: un
         real(cp),intent(in) :: dt
         call f(un,dt)
         call multiply(f,dt)
         call subtract(unp1,un,f)
       end subroutine

       subroutine solveTimeCentered(unp1,un,fn,dt)
         ! Solves 
         ! 
         !        n+1    n
         !       u    - u     n
         !       --------  = f
         !          dt
         implicit none
         type(vectorField),intent(inout) :: unp1,fn
         type(vectorField),intent(in) :: un
         real(cp),intent(in) :: dt
         call multiply(fn,dt)
         call subtract(unp1,un,fn)
       end subroutine


       end module