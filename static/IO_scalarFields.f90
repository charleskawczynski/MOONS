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
      public :: writeToFile,readFromFile
      public :: writeScalarPhysical
      public :: writeScalarPlane
      public :: writeScalarPhysicalPlane

      interface readFromFile;  module procedure read3DFieldFromFile;    end interface
      interface writeToFile;   module procedure write3DMeshToFileGrid;  end interface
      interface writeToFile;   module procedure write3DFieldToFileGrid; end interface

      contains

      subroutine read3DFieldFromFile(g,arr,dir,name,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(grid),intent(inout) :: g
        logical,intent(in),optional :: headerTecplotTemp
        real(cp),dimension(:,:,:),intent(inout) :: arr
        real(cp),dimension(:),allocatable :: x,y,z
        integer,dimension(3) :: s
        s = shape(arr)

        allocate(x(s(1)))
        allocate(y(s(2)))
        allocate(z(s(3)))
        if (present(headerTecplotTemp)) then
          call readFromFile(x,y,z,arr,dir,name,headerTecplotTemp)
        else
          call readFromFile(x,y,z,arr,dir,name)
        endif
        call init(g,x,1,2)
        call init(g,y,2,2)
        call init(g,z,3,2)
        deallocate(x,y,z)
      end subroutine

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
        if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then

        ! Node data
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hn(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)

        elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then

        ! CC data
        call writeToFile(g%c(1)%hc(2:s(1)-1),&
                         g%c(2)%hc(2:s(2)-1),&
                         g%c(3)%hc(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)

        ! Face data
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hc(2:s(2)-1),&
                         g%c(3)%hc(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hc(2:s(1)-1),&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hc(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hc(2:s(1)-1),&
                         g%c(2)%hc(2:s(2)-1),&
                         g%c(3)%hn(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)

        ! Edge Data
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hc(2:s(1)-1),&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hn(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hc(2:s(2)-1),&
                         g%c(3)%hn(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hc(2:s(3)-1),&
                         f(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,name)
        else
          write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
          stop 'Done'
        endif
      end subroutine

      subroutine writeScalarPhysicalPlane(g,f,directory,name,ext,dir,p,n)
        implicit none
        character(len=*),intent(in) :: directory,name,ext
        type(grid),intent(in) :: g
        real(cp),dimension(:,:,:),intent(in) :: f
        integer,intent(in) :: dir,p,n
        integer,dimension(3) :: s
        integer :: i
        s = shape(f)
        select case(dir)
        case (1)
            if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then

            ! Node data
            call writeToFile(g%c(2)%hn(2:s(2)-1),&
                             g%c(3)%hn(2:s(3)-1),&
                             f(p,2:s(2)-1,2:s(3)-1),directory,name,ext,n)

            elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then

            ! CC data
            call writeToFile(g%c(2)%hc(2:s(2)-1),&
                             g%c(3)%hc(2:s(3)-1),&
                             f(p,2:s(2)-1,2:s(3)-1),directory,name,ext,n)

            ! Face data
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(2)%hc(2:s(2)-1),&
                             g%c(3)%hc(2:s(3)-1),&
                             f(p,2:s(2)-1,2:s(3)-1),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(2)%hn(2:s(2)-1),&
                             g%c(3)%hc(2:s(3)-1),&
                             f(p,2:s(2)-1,2:s(3)-1),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(2)%hc(2:s(2)-1),&
                             g%c(3)%hn(2:s(3)-1),&
                             f(p,2:s(2)-1,2:s(3)-1),directory,name,ext,n)

            ! Edge Data
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(2)%hn(2:s(2)-1),&
                             g%c(3)%hn(2:s(3)-1),&
                             f(p,2:s(2)-1,2:s(3)-1),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(2)%hc(2:s(2)-1),&
                             g%c(3)%hn(2:s(3)-1),&
                             f(p,2:s(2)-1,2:s(3)-1),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(2)%hn(2:s(2)-1),&
                             g%c(3)%hc(2:s(3)-1),&
                             f(p,2:s(2)-1,2:s(3)-1),directory,name,ext,n)
            else
              write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
              stop 'Done'
            endif
        case (2)
            if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then

            ! Node data
            call writeToFile(g%c(1)%hn(2:s(1)-1),&
                             g%c(3)%hn(2:s(3)-1),&
                             f(2:s(1)-1,p,2:s(3)-1),directory,name,ext,n)

            elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then

            ! CC data
            call writeToFile(g%c(1)%hc(2:s(1)-1),&
                             g%c(3)%hc(2:s(3)-1),&
                             f(2:s(1)-1,p,2:s(3)-1),directory,name,ext,n)

            ! Face data
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hn(2:s(1)-1),&
                             g%c(3)%hc(2:s(3)-1),&
                             f(2:s(1)-1,p,2:s(3)-1),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hc(2:s(1)-1),&
                             g%c(3)%hc(2:s(3)-1),&
                             f(2:s(1)-1,p,2:s(3)-1),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hc(2:s(1)-1),&
                             g%c(3)%hn(2:s(3)-1),&
                             f(2:s(1)-1,p,2:s(3)-1),directory,name,ext,n)

            ! Edge Data
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hc(2:s(1)-1),&
                             g%c(3)%hn(2:s(3)-1),&
                             f(2:s(1)-1,p,2:s(3)-1),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hn(2:s(1)-1),&
                             g%c(3)%hn(2:s(3)-1),&
                             f(2:s(1)-1,p,2:s(3)-1),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hn(2:s(1)-1),&
                             g%c(3)%hc(2:s(3)-1),&
                             f(2:s(1)-1,p,2:s(3)-1),directory,name,ext,n)
            else
              write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
              stop 'Done'
            endif
        case (3)
            if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then

            ! Node data
            call writeToFile(g%c(1)%hn(2:s(1)-1),&
                             g%c(2)%hn(2:s(2)-1),&
                             f(2:s(1)-1,2:s(2)-1,p),directory,name,ext,n)

            elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then

            ! CC data
            call writeToFile(g%c(1)%hc(2:s(1)-1),&
                             g%c(2)%hc(2:s(2)-1),&
                             f(2:s(1)-1,2:s(2)-1,p),directory,name,ext,n)

            ! Face data
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hn(2:s(1)-1),&
                             g%c(2)%hc(2:s(2)-1),&
                             f(2:s(1)-1,2:s(2)-1,p),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hc(2:s(1)-1),&
                             g%c(2)%hn(2:s(2)-1),&
                             f(2:s(1)-1,2:s(2)-1,p),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hc(2:s(1)-1),&
                             g%c(2)%hc(2:s(2)-1),&
                             f(2:s(1)-1,2:s(2)-1,p),directory,name,ext,n)

            ! Edge Data
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hc(2:s(1)-1),&
                             g%c(2)%hn(2:s(2)-1),&
                             f(2:s(1)-1,2:s(2)-1,p),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hn(2:s(1)-1),&
                             g%c(2)%hc(2:s(2)-1),&
                             f(2:s(1)-1,2:s(2)-1,p),directory,name,ext,n)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hn(2:s(1)-1),&
                             g%c(2)%hn(2:s(2)-1),&
                             f(2:s(1)-1,2:s(2)-1,p),directory,name,ext,n)
            else
              write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
              stop 'Done'
            endif
        case default
        stop 'Error: dir must = 1,2,3 in writeScalarPhysicalPlane in IO_scalarFields.f90'
        end select

      end subroutine

      subroutine writeScalarPlane(g,f,directory,name,dir,p)
        implicit none
        character(len=*),intent(in) :: directory,name
        type(grid),intent(in) :: g
        real(cp),dimension(:,:,:),intent(in) :: f
        integer,intent(in) :: dir,p
        integer,dimension(3) :: s
        integer :: i
        s = shape(f)
        select case(dir)
        case (1)
            if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then
            ! Node data
            call writeToFile(g%c(2)%hn,g%c(3)%hn,f(p,:,:),directory,name)
            elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then
            ! CC data
            call writeToFile(g%c(2)%hc,g%c(3)%hc,f(p,:,:),directory,name)
            ! Face data
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(2)%hc,g%c(3)%hc,f(p,:,:),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(2)%hn,g%c(3)%hc,f(p,:,:),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(2)%hc,g%c(3)%hn,f(p,:,:),directory,name)
            ! Edge Data
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(2)%hn,g%c(3)%hn,f(p,:,:),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(2)%hc,g%c(3)%hn,f(p,:,:),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(2)%hn,g%c(3)%hc,f(p,:,:),directory,name)
            else
              write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
              stop 'Done'
            endif
        case (2)
            if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then
            ! Node data
            call writeToFile(g%c(1)%hn,g%c(3)%hn,f(:,p,:),directory,name)
            elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then
            ! CC data
            call writeToFile(g%c(1)%hc,g%c(3)%hc,f(:,p,:),directory,name)
            ! Face data
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hn,g%c(3)%hc,f(:,p,:),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hc,g%c(3)%hc,f(:,p,:),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hc,g%c(3)%hn,f(:,p,:),directory,name)
            ! Edge Data
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hc,g%c(3)%hn,f(:,p,:),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hn,g%c(3)%hn,f(:,p,:),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hn,g%c(3)%hc,f(:,p,:),directory,name)
            else
              write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
              stop 'Done'
            endif
        case (3)
            if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then
            ! Node data
            call writeToFile(g%c(1)%hn,g%c(2)%hn,f(:,:,p),directory,name)
            elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then
            ! CC data
            call writeToFile(g%c(1)%hc,g%c(2)%hc,f(:,:,p),directory,name)
            ! Face data
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hn,g%c(2)%hc,f(:,:,p),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hc,g%c(2)%hn,f(:,:,p),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hc,g%c(2)%hc,f(:,:,p),directory,name)
            ! Edge Data
            elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hc,g%c(2)%hn,f(:,:,p),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
            call writeToFile(g%c(1)%hn,g%c(2)%hc,f(:,:,p),directory,name)
            elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
            call writeToFile(g%c(1)%hn,g%c(2)%hn,f(:,:,p),directory,name)
            else
              write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
              stop 'Done'
            endif
        case default
        stop 'Error: dir must = 1,2,3 in writeScalarPhysicalPlane in IO_scalarFields.f90'
        end select

      end subroutine

      ! subroutine getGrid(f,g)
      !   ! Returns a grid, g, whose 'node' locations coincide with
      !   ! locations for f. This way, a simple
      !   ! 
      !   !     call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,f,dir,name)
      !   ! 
      !   ! can be used for any grid.
      !   implicit none
      !   real(cp),dimension(:,:,:),intent(in) :: f
      !   type(grid),intent(inout) :: g
      !   type(grid) :: temp
      !   integer,dimension(3) :: s
      !   s = shape(f)
      !   if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then ! Node data
      !   call init(temp,g)
      !   elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then ! CC data
      !   call init(temp,g%c(1)%hc,1,2)
      !   call init(temp,g%c(2)%hc,2,2)
      !   call init(temp,g%c(3)%hc,3,2)
      !   elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then ! Face data (x)
      !   call init(temp,g%c(1)%hn,1,2)
      !   call init(temp,g%c(2)%hc,2,2)
      !   call init(temp,g%c(3)%hc,3,2)
      !   elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then ! Face data (y)
      !   call init(temp,g%c(1)%hc,1,2)
      !   call init(temp,g%c(2)%hn,2,2)
      !   call init(temp,g%c(3)%hc,3,2)
      !   elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then ! Face data (z)
      !   call init(temp,g%c(1)%hc,1,2)
      !   call init(temp,g%c(2)%hc,2,2)
      !   call init(temp,g%c(3)%hn,3,2)
      !   elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then ! Edge Data (x)
      !   call init(temp,g%c(1)%hc,1,2)
      !   call init(temp,g%c(2)%hn,2,2)
      !   call init(temp,g%c(3)%hn,3,2)
      !   elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then ! Edge Data (y)
      !   call init(temp,g%c(1)%hn,1,2)
      !   call init(temp,g%c(2)%hc,2,2)
      !   call init(temp,g%c(3)%hn,3,2)
      !   elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then ! Edge Data (z)
      !   call init(temp,g%c(1)%hn,1,2)
      !   call init(temp,g%c(2)%hn,2,2)
      !   call init(temp,g%c(3)%hc,3,2)
      !   else
      !     write(*,*) 'Error: bad grid size compared to input field '//name//' in IO_scalarFields.f90.'
      !     stop 'Done'
      !   endif
      !   call init(g,temp)
      !   call delete(temp)
      ! end subroutine

      subroutine write3DMeshToFileGrid(g,dir,name)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(grid),intent(in) :: g
        call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,1.0_cp,dir,name)
      end subroutine

      end module