      module data_set_mod
      use current_precision_mod
      use string_mod
      use grid_mod
      use VF_mod
      use SF_mod
      use IO_tools_mod
      use export_mod
      implicit none
      ! Implementation:
      ! 
      !          call init(DS,'out/Bfield/','B',3,1)
      !          call export(m,DS,B)
      ! 

      private
      public :: data_set
      public :: init,delete
      ! public :: print,export

      interface init;     module procedure init_data_set;   end interface
      interface init;     module procedure init_name;       end interface
      interface delete;   module procedure delete_data_set; end interface

      interface export;   module procedure export_data_set; end interface
      interface print;    module procedure print_data_set;  end interface

       type data_set
        ! Example:
        ! TITLE = "3D Vector Field uni_phys,vni_phys,wni_phys"
        ! VARIABLES = "X","Y","Z","uni_phys","vni_phys","wni_phys"
        ! ZONE , I = 0000000065, J = 0000000065, K = 0000000002 DATAPACKING = POINT
        ! ZONE , T = "0000000065" I = 0000000065, J = 0000000065, K = 0000000002 DATAPACKING = POINT

         type(string) :: directory                       ! Directory
         type(string) :: filename                        ! Filename
         ! ----- Complete header info -----
         type(string) :: title                           ! Entire title line
         type(string) :: variables                       ! Entire variables line
         type(string) :: zone                            ! Entire zone line

         type(string) :: zoneFMT                         ! entire zone line format
         type(string) :: titleFMT                        ! entire title line format
         type(string) :: variablesFMT                    ! entire variables line format
         ! ----- Separater header info -----
         type(string) :: tit                             ! TITLE = 3D SCALAR FIELD OF | title |
         type(string),dimension(:),allocatable :: var    ! VARIABLES = 'X','Y','Z',|'Fx','Fy','Fz'|
         type(string),dimension(:),allocatable :: coord  ! VARIABLES = |'X','Y','Z'|,'Fx','Fy','Fz'
         type(string),dimension(:),allocatable :: ind    ! ZONE |I| = 01, |J| = 01, |K| = 01 DATAPACKING = POINT
         type(string),dimension(:),allocatable :: indVal ! ZONE I = |01|, J = |01|, K = |01| DATAPACKING = POINT

         integer :: dataType                             ! Data type (cellcenter,node,face,edge)
         integer :: un                                   ! Unit (for output)
         integer :: pad                                  ! Pad output (skip ghost cells)
         integer :: dim                                  ! Dimensions of data_set (1D,2D,3D)
         logical :: transient                            ! Transient data or snapshot (not yet used...)
         integer :: transientIndex                       ! Index for transient data (not yet used...)
       end type

      contains

      ! ********************************************************
      ! ************************* INIT *************************
      ! ********************************************************

      subroutine init_data_set(DS,directory,name,dimensions,components)
        implicit none
        type(data_set),intent(inout) :: DS
        character(len=*),intent(in) :: directory,name
        integer,intent(in) :: dimensions,components
        call init_dir_name(DS,directory,name)
        call init_tit_var_un(DS)

        call setCoord(DS,m,dimensions)
        call setVariables(DS)
        call setZone(DS)
        call setTypeReal(DS,m,U%x%RF(1)%s)

        call prepHeaderFMT(DS)
      end subroutine

      subroutine init_data_set(DS,m,U,directory,name,dimensions,components,pad)
        implicit none
        type(data_set),intent(inout) :: DS
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: U
        character(len=*),intent(in) :: directory,name
        integer,intent(in) :: dimensions,components

        call init(DS,directory,name)
        A%stot-2*pad

        call setCoord(DS,m,dimensions)
        call setVariables(DS)
        call setZone(DS)
        call setTypeReal(DS,m,U%x%RF(1)%s)

        call prepHeaderFMT(DS)
      end subroutine

      subroutine init_dir_name(DS,directory,name)
        ! Defines
        !              directory,filename,tit,var
        implicit none
        type(data_set),intent(inout) :: DS
        character(len=*),intent(in) :: directory,name
        call init(DS%directory,directory)
        call compress(DS%directory)
        call init(DS%filename,name)
        call compress(DS%filename)
        call init(DS%tit,DS%filename%s)
      end subroutine

      subroutine init_tit_var_un(DS)
        implicit none
        type(data_set),intent(inout) :: DS
        call init(DS%tit,'TITLE = '//str(DS%filename))
        call init(DS%tit,str(DS%filename))
        allocate(DS%var(3))
        call init(DS%var(1),str(DS%name)//'_x')
        call init(DS%var(2),str(DS%name)//'_y')
        call init(DS%var(3),str(DS%name)//'_z')
        DS%un = newAndOpen(str(DS%directory),str(DS%filename%s))
      end subroutine

      subroutine setCoord(DS,g,dim)
        implicit none
        type(data_set),intent(inout) :: DS
        type(grid),intent(in) :: g
        integer,intent(in) :: dim
        DS%dim = dim
        select case (DS%dim)
        case (1) ! 1D plot.
          call init(DS%title,'1D VECTOR FIELD OF ')
          call append(DS%title,DS%tit%s)
          allocate(DS%ind(1)); allocate(DS%coord(1))
          if ((g%c(2)%N.eq.1).and.(g%c(3)%N.eq.1)) then
            call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
          elseif ((g%c(1)%N.eq.1).and.(g%c(3)%N.eq.1)) then
            call init(DS%ind(1),'J'); call init(DS%coord(1),'Y')
          elseif ((g%c(1)%N.eq.1).and.(g%c(2)%N.eq.1)) then
            call init(DS%ind(1),'K'); call init(DS%coord(1),'Z')
          else
          stop 'Error: dim=1 but N_cells.ne.1 for two directions in setType in data_set.f90'
          endif
        case (2) ! 2D plot.
          call init(DS%title,'2D VECTOR FIELD OF ')
          call append(DS%title,DS%tit%s)
          allocate(DS%ind(2)); allocate(DS%coord(2))
          if (g%c(1)%N.eq.1) then
            call init(DS%ind(1),'J'); call init(DS%coord(1),'Y')
            call init(DS%ind(2),'K'); call init(DS%coord(2),'Z')
          elseif (g%c(2)%N.eq.1) then
            call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
            call init(DS%ind(2),'K'); call init(DS%coord(2),'Z')
          elseif (g%c(3)%N.eq.1) then
            call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
            call init(DS%ind(2),'J'); call init(DS%coord(2),'Y')
          else
          stop 'Error: dim=2 but N_cells.ne.1 for any directions in setType in data_set.f90'
          endif
        case (3) ! 3D plot.
          call init(DS%title,'3D VECTOR FIELD OF ')
          call append(DS%title,DS%tit%s)
          allocate(DS%ind(3)); allocate(DS%coord(3))
          call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
          call init(DS%ind(2),'J'); call init(DS%coord(2),'Y')
          call init(DS%ind(3),'K'); call init(DS%coord(3),'Z')
        case default
        stop 'Error: dim must = 1,2,3 in setType in data_set.f90'
        end select
      end subroutine

      subroutine setVariables(DS)
        implicit none
        type(data_set),intent(inout) :: DS
        integer :: i
        call init(DS%variables,'VARIABLES = "')

        if (size(DS%coord).gt.1) then
          do i=1,size(DS%coord)-1
            call append(DS%variables,DS%coord(i)%s//'","')
          enddo
          call append(DS%variables,DS%coord(size(DS%coord))%s//'",')
        endif

        if (size(DS%var).gt.1) then
          do i=1,size(DS%var)-1
            call append(DS%variables,DS%var(i)%s//'","')
          enddo
          call append(DS%variables,DS%var(size(DS%var))%s//'"')
        endif
      end subroutine

      subroutine setZone(DS)
        implicit none
        type(data_set),intent(inout) :: DS
        integer :: i
        call init(DS%zone,'ZONE , ')
        do i=1,size(DS%ind)-1
          call append(DS%zone,DS%ind(i)%s//' = ')
          call append(DS%zone,DS%indVal(i)%s//', ')
        enddo
        call append(DS%zone,DS%ind(size(DS%ind))%s//' = ')
        call append(DS%zone,DS%indVal(size(DS%ind))%s//' ')
        call append(DS%zone,'DATAPACKING = POINT')
      end subroutine

      subroutine prepHeaderFMT(DS)
        implicit none
        type(data_set),intent(inout) :: DS
        call init(DS%titleFMT,'(A'//int2str(len(DS%title%s))//')')
        call init(DS%variablesFMT,'(A'//int2str(len(DS%variables%s))//')')
        call init(DS%zoneFMT,'(A'//int2str(len(DS%zone%s))//')')
      end subroutine

      subroutine export_header(DS)
        implicit none
        type(data_set),intent(inout) :: DS
        write(DS%un,DS%titleFMT%s) DS%title%s
        write(DS%un,DS%variablesFMT%s) DS%variables%s
        write(DS%un,DS%zoneFMT%s) DS%zone%s
      end subroutine
      
!       subroutine export_DS(DS,g,u,headerTecplotTemp)
!         implicit none
!         type(data_set),intent(inout) :: DS
!         type(grid),intent(in) :: g
!         real(cp),dimension(:,:,:),intent(in) :: u
!         logical,intent(in),optional :: headerTecplotTemp
!         integer un,i,j,k,sx,sy,sz
!         sx = size(x); sy = size(y); sz = size(z)
!         un = newAndOpen(dir,trim(adjustl(namex))//','//trim(adjustl(namey))//','//trim(adjustl(namez)))
!          do k = 1,sz; do j = 1,sy; do i = 1,sx
!            write(un,'(6'//arrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
!          enddo; enddo; enddo
!         call closeAndMessage(un,dir,namex//','//namey//','//namez)
!       end subroutine
      
      subroutine delete_data_set(DS)
        implicit none
        type(data_set),intent(inout) :: DS
        integer :: i
        do i=1,size(DS%var);         call delete(DS%var(i));         enddo
        do i=1,size(DS%coord);       call delete(DS%coord(i));       enddo
        do i=1,size(DS%indVal);      call delete(DS%indVal(i));      enddo
        do i=1,size(DS%ind);         call delete(DS%ind(i));         enddo
        call delete(DS%directory);   call delete(DS%filename)
        call delete(DS%title);       call delete(DS%variables)
        call delete(DS%zone);        call delete(DS%zoneFMT)
        call delete(DS%titleFMT);    call delete(DS%variablesFMT)
        call delete(DS%tit)
      end subroutine


      end module