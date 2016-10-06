       module check_BCs_mod
       use current_precision_mod
       use bctype_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: check_defined
       public :: check_dimensions

       contains

       subroutine check_defined(U)
         implicit none
         type(SF),intent(in) :: U
         integer :: i,k
         ! do i=1,U%s
         ! if (.not.defined(U%BF(i)%b)) stop 'Error: bad bctype in check_BCs in check_BCs.f90'
         ! enddo
         do i=1,U%s
           do k=1,6
             if (.not.is_defined(U%BF(i)%b%f(k)%b)) stop 'Error: bad bctype in check_BCs in check_BCs.f90'
           enddo
           do k=1,12
             if (.not.is_defined(U%BF(i)%b%e(k)%b)) stop 'Error: bad bctype in check_BCs in check_BCs.f90'
           enddo
           do k=1,6
             if (.not.is_defined(U%BF(i)%b%c(k)%b)) stop 'Error: bad bctype in check_BCs in check_BCs.f90'
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