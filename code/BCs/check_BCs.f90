       module check_BCs_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: check_defined
       public :: check_dimensions

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

       subroutine check_defined(U,m)
         implicit none
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer :: i,k
         do i=1,m%s
           do k=1,6
             if (.not.U%RF(i)%b%f(k)%b%defined) stop 'Error: bad bctype in check_BCs in check_BCs.f90'
           enddo
           do k=1,12
             if (.not.U%RF(i)%b%e(k)%b%defined) stop 'Error: bad bctype in check_BCs in check_BCs.f90'
           enddo
           do k=1,6
             if (.not.U%RF(i)%b%c(k)%b%defined) stop 'Error: bad bctype in check_BCs in check_BCs.f90'
           enddo
         enddo
       end subroutine

       subroutine check_dimensions(f,g)
         implicit none
         real(cp),dimension(:,:),intent(in) :: f,g
         integer,dimension(2) :: sf,sg
         sf = shape(f); sg = shape(g)
         if ((sf(1).ne.sg(1)).or.(sf(2).ne.sg(2))) then
           write(*,*) 'sf = ',sf
           write(*,*) 'sg = ',sg
           stop 'Error: shapes do not match in check_dimensions in apply_BCs_faces.f90'
         endif
       end subroutine

       end module