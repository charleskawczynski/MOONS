      module IO_scalarFields_mod
      use grid_mod
      use IO_scalarBase_mod
      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


      private
      public :: writeToFile,writeScalarPhysical

      interface writeToFile;   module procedure write3DMeshToFileGrid;  end interface
      interface writeToFile;   module procedure write3DFieldToFileGrid; end interface

      contains

      subroutine write3DFieldToFileGrid(g,f,dir,name)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(grid),intent(in) :: g
        real(cp),dimension(:,:,:),intent(in) :: f
        integer,dimension(3) :: s
        integer :: i
        s = shape(f)
        if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then

        ! Node data
        call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,f,dir,name)

        elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then

        ! CC data
        call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,f,dir,name)

        ! Face data
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hn,g%c(2)%hc,g%c(3)%hc,f,dir,name)
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hc,g%c(2)%hn,g%c(3)%hc,f,dir,name)
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hn,f,dir,name)

        ! Edge Data
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hc,g%c(2)%hn,g%c(3)%hn,f,dir,name)
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hn,g%c(2)%hc,g%c(3)%hn,f,dir,name)
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hc,f,dir,name)
        else
          write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
          stop 'Done'
        endif
      end subroutine

      subroutine writeScalarPhysical(g,f,dir,name)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(grid),intent(in) :: g
        real(cp),dimension(:,:,:),intent(in) :: f
        integer,dimension(3) :: s
        integer :: i
        s = shape(f)
        if (all((/(s(i).eq.g%c(1)%sn, i=1,3)/))) then

        ! Node data
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hn(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)

        elseif (all((/(s(i).eq.g%c(1)%sc, i=1,3)/))) then

        ! CC data
        call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,f,dir,name)

        ! Face data
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hc,&
                         g%c(3)%hc,&
                         f(2:s(1)-1,:,:),dir,name)
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hc,&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hc,&
                         f(:,2:s(2)-1,:),dir,name)
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hc,&
                         g%c(2)%hc,&
                         g%c(3)%hn(2:s(3)-1),&
                         f(:,:,2:s(3)-1),dir,name)

        ! Edge Data
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hc,&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hn(2:s(3)-1),&
                         f(:,2:s(2)-1,2:s(3)-1),dir,name)
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hc,&
                         g%c(3)%hn(2:s(3)-1),&
                         f(2:s(1)-1,:,2:s(3)-1),dir,name)
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hc,&
                         f(2:s(1)-1,2:s(2)-1,:),dir,name)
        else
          write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
          stop 'Done'
        endif
      end subroutine

      subroutine write3DMeshToFileGrid(g,val,dir,name)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(grid),intent(in) :: g
        real(cp),intent(in) :: val
        call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,val,dir,name)
      end subroutine

      end module