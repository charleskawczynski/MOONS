      module vectorField_mod
        implicit none
        private

        type vectorField
          public
          integer,dimension(3) :: sx,sy,sz
          real(dpn),dimension(:,:,:),allocatable :: x,y,z
        end type

      contains

        subroutine allocateVectorField(field,Nx,Ny,Nz)
          implicit none
          type(vectorField),intent(inout) :: field
          integer,dimension(3),intent(in) :: Nx,Ny,Nz
          field%sx = shape(field%x)
          field%sy = shape(field%y)
          field%sz = shape(field%z)
          allocate(field%x(Nx(1),Nx(2),Nx(3)))
          allocate(field%x(Ny(1),Ny(2),Ny(3)))
          allocate(field%x(Nz(1),Nz(2),Nz(3)))
        end subroutine

        subroutine deallocateField(gf)
          implicit none
          type(vectorField),intent(inout) :: field
          deallocate(field%x,field%y,field%z)
        end subroutine

      end module