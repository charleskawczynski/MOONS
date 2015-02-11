      module scalarField_mod
        implicit none
        private

        type scalarField
          public
          integer,dimension(3) :: s
          real(dpn),dimension(:,:,:),allocatable :: phi
        end type

      contains

        subroutine allocateField(field,Nx,Ny,Nz)
          implicit none
          type(scalarField),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          field%s = shape(field%phi)
          allocate(field%phi(Nx,Ny,Nz))
        end subroutine

        subroutine deallocateField(gf)
          implicit none
          type(scalarField),intent(inout) :: field
          deallocate(field%phi)
        end subroutine

      end module
