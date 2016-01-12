       module exportA_mod
       use mesh_mod
       use IO_tools_mod
       use ops_aux_mod
       use SF_mod

       implicit none

       private
       public :: exportA

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

       subroutine exportA(x,sigmaInv,dt,Rem,m,dir,name)
         implicit none
         type(VF),intent(in) :: x,sigmaInv
         real(cp),intent(in) :: dt,Rem
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir,name
         type(SF) :: un,Aun
         integer :: i,newU
         call init(un,x)
         newU = newAndOpen(dir,name)
         write(*,*) 'System size = ',un%numEl
         do i=1,un%numEl
           call unitVector(un,i)
           call compute_Ax_transient(Aun,un,sigmaInv,dt,Rem,m)
           call export_vector_transpose(Aun,newU) ! Export rows of A
         enddo
         close(newU)
       end subroutine

       subroutine export_vector_transpose(U,un)
         implicit none
         type(SF),intent(in) :: U
         integer,intent(in) :: un
         integer :: i,j,k,t
         do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
         write(un,'(F5.3)',advance='no') U%RF(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         write(un,*) ''
       end subroutine

      subroutine compute_Ax_transient(Ax,x,sigmaInv,dt,Rem,m)
        ! Computes Ax = Rem^-1 curl(sigmaInv curl(x)) + dx/dt
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,sigmaInv
        real(cp),intent(in) :: dt,Rem
        type(mesh),intent(in) :: m
        type(VF) :: temp
        call init(temp,sigmaInv)
        call assign(temp,0.0_cp)
        call curlcurl(Ax,x,sigmaInv,temp,m)
        call divide(Ax,Rem)
        call multiply(Ax,dt)
        call add(Ax,x)
        call delete(temp)
      end subroutine

       end module