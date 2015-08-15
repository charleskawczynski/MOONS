      module dataSet_mod
      use varStr_mod
      use grid_mod
      use VF_mod
      use IO_tools_mod
      use export_mod
      implicit none
      ! Implementation:
      ! 
      !          call init(DS,dim,pad,'out/Bfield/','Bx','By','Bz','nt_phys')
      !          call export(DS,g,B)
      ! 
      !          call init(DS,3,1,'out/Ufield/','u','v','w','ni_phys')
      !          call export(DS,g,tempNVF)
      ! 
      !          call init(DS,2,1,'out/Ufield/','u','v','w','ni_phys')
      !          call setPlane(DS,2)
      !          call export(DS,g,tempNVF)
      ! 
      !          call init(DS,3,0,'out/Ufield/','u','v','w','fi')
      !          call export(DS,g,mom%U)
      ! 

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
      public :: dataSet

      interface init;     module procedure init_dataSet;   end interface
      interface init;     module procedure init_name;      end interface
      interface delete;   module procedure delete_dataSet; end interface

      ! interface export;   module procedure export_DS;      end interface

       type dataSet
        ! Example:
        ! TITLE = "3D Vector Field uni_phys,vni_phys,wni_phys"
        ! VARIABLES = "X","Y","Z","uni_phys","vni_phys","wni_phys"
        ! ZONE , I = 0000000065, J = 0000000065, K = 0000000002 DATAPACKING = POINT
        ! ZONE , T = "0000000065" I = 0000000065, J = 0000000065, K = 0000000002 DATAPACKING = POINT

         type(varstr) :: directory                       ! Directory
         type(varstr) :: filename                        ! Filename
         ! ----- Complete header info -----
         type(varstr) :: title                           ! Entire title line
         type(varstr) :: variables                       ! Entire variables line
         type(varstr) :: zone                            ! Entire zone line

         type(varstr) :: zoneFMT                         ! entire zone line format
         type(varstr) :: titleFMT                        ! entire title line format
         type(varstr) :: variablesFMT                    ! entire variables line format
         ! ----- Separater header info -----
         type(varstr) :: tit                             ! TITLE = 3D SCALAR FIELD OF | title |
         type(varstr),dimension(:),allocatable :: var    ! VARIABLES = 'X','Y','Z',|'Fx','Fy','Fz'|
         type(varstr),dimension(:),allocatable :: coord  ! VARIABLES = |'X','Y','Z'|,'Fx','Fy','Fz'
         type(varstr),dimension(:),allocatable :: ind    ! ZONE |I| = 01, |J| = 01, |K| = 01 DATAPACKING = POINT
         type(varstr),dimension(:),allocatable :: indVal ! ZONE I = |01|, J = |01|, K = |01| DATAPACKING = POINT

         integer :: dataType                             ! Data type (cellcenter,node,face,edge)
         integer :: un                                   ! Unit (for output)
         integer :: pad                                  ! Pad output (skip ghost cells)
         integer :: dim                                  ! Dimensions of dataSet (1D,2D,3D)
         logical :: transient                            ! Transient data or snapshot (not yet used...)
         integer :: transientIndex                       ! Index for transient data (not yet used...)
       end type

      contains

      ! ********************************************************
      ! ************************* INIT *************************
      ! ********************************************************

      subroutine init_dataSet(DS,g,U,directory,namex,namey,namez,ext,dim)
        implicit none
        type(dataSet),intent(inout) :: DS
        type(grid),intent(in) :: g
        type(VF),intent(in) :: U
        character(len=*),intent(in) :: directory,namex,namey,namez
        character(len=*),intent(in),optional :: ext
        integer,intent(in) :: dim

        if (present(ext)) then
              call init(DS,directory,namex,namey,namez,ext)
        else; call init(DS,directory,namex,namey,namez,'')
        endif
        

        call setCoord(DS,g,dim)
        call setVariables(DS)
        call setZone(DS)
        call setTypeReal(DS,g,shape(U%x))

        call prepHeaderFMT(DS)
      end subroutine

      subroutine init_name(DS,directory,namex,namey,namez,ext)
        ! Defines
        !              directory,filename,tit,var
        implicit none
        type(dataSet),intent(inout) :: DS
        character(len=*),intent(in) :: directory,namex,namey,namez,ext
        call init(DS%directory,directory)
        call compress(DS%directory)
        call init(DS%filename,namex//ext//','//namey//ext//','//namez//ext)
        call compress(DS%filename)
        call init(DS%tit,DS%filename%s)
        allocate(DS%var(3))
        call init(DS%var(1),namex//ext)
        call init(DS%var(2),namey//ext)
        call init(DS%var(3),namez//ext)
        DS%un = newAndOpen(DS%directory%s,DS%filename%s)
      end subroutine

      subroutine setCoord(DS,g,dim)
        implicit none
        type(dataSet),intent(inout) :: DS
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
          stop 'Error: dim=1 but N_cells.ne.1 for two directions in setType in dataSet.f90'
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
          stop 'Error: dim=2 but N_cells.ne.1 for any directions in setType in dataSet.f90'
          endif
        case (3) ! 3D plot.
          call init(DS%title,'3D VECTOR FIELD OF ')
          call append(DS%title,DS%tit%s)
          allocate(DS%ind(3)); allocate(DS%coord(3))
          call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
          call init(DS%ind(2),'J'); call init(DS%coord(2),'Y')
          call init(DS%ind(3),'K'); call init(DS%coord(3),'Z')
        case default
        stop 'Error: dim must = 1,2,3 in setType in dataSet.f90'
        end select
      end subroutine

      subroutine setVariables(DS)
        implicit none
        type(dataSet),intent(inout) :: DS
        integer :: i
        call init(DS%variables,'VARIABLES = "')

        if (size(DS%coord).gt.1) then
          do i=1,size(DS%coord)-1
            call append(DS%variables,DS%coord(i)%s//'","')
          enddo
        endif
        call append(DS%variables,DS%coord(size(DS%coord))%s//'",')

        if (size(DS%var).gt.1) then
          do i=1,size(DS%var)-1
            call append(DS%variables,DS%var(i)%s//'","')
          enddo
        endif
        call append(DS%variables,DS%var(size(DS%var))%s//'"')
      end subroutine

      subroutine setZone(DS)
        implicit none
        type(dataSet),intent(inout) :: DS
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
        type(dataSet),intent(inout) :: DS
        call init(DS%titleFMT,'(A'//int2str(len(DS%title%s))//')')
        call init(DS%variablesFMT,'(A'//int2str(len(DS%variables%s))//')')
        call init(DS%zoneFMT,'(A'//int2str(len(DS%zone%s))//')')
      end subroutine

      subroutine setTypeReal(DS,g,s)
        implicit none
        type(dataSet),intent(inout) :: DS
        type(grid),intent(in) :: g
        integer,dimension(3),intent(in) :: s
        DS%dataType = getType_3D(g,s,DS%tit%s)
      end subroutine

      subroutine export_header(DS)
        implicit none
        type(dataSet),intent(inout) :: DS
        write(DS%un,DS%titleFMT%s) DS%title%s
        write(DS%un,DS%variablesFMT%s) DS%variables%s
        write(DS%un,DS%zoneFMT%s) DS%zone%s
      end subroutine
      
!       subroutine export_DS(DS,g,u,headerTecplotTemp)
!         implicit none
!         type(dataSet),intent(inout) :: DS
!         type(grid),intent(in) :: g
!         real(cp),dimension(:,:,:),intent(in) :: u
!         logical,intent(in),optional :: headerTecplotTemp
!         integer un,i,j,k,sx,sy,sz
!         sx = size(x); sy = size(y); sz = size(z)
!         un = newAndOpen(dir,trim(adjustl(namex))//','//trim(adjustl(namey))//','//trim(adjustl(namez)))
!          do k = 1,sz; do j = 1,sy; do i = 1,sx
!            write(un,'(6'//arrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
!          enddo; enddo; enddo
!         call closeAndMessage(un,trim(adjustl(namex))//','//trim(adjustl(namey))//','//trim(adjustl(namez)),dir)
!       end subroutine
      
      subroutine delete_dataSet(DS)
        implicit none
        type(dataSet),intent(inout) :: DS
        integer :: i
        do i=1,size(DS%var);         call delete(DS%var(i));         enddo
        do i=1,size(DS%coord);       call delete(DS%coord(i));       enddo
        do i=1,size(DS%indVal);      call delete(DS%indVal(i));      enddo
        do i=1,size(DS%ind);      call delete(DS%ind(i));      enddo
        call delete(DS%directory);   call delete(DS%filename)
        call delete(DS%title);       call delete(DS%variables)
        call delete(DS%zone);        call delete(DS%zoneFMT)
        call delete(DS%titleFMT);    call delete(DS%variablesFMT)
        call delete(DS%tit)
      end subroutine


      end module