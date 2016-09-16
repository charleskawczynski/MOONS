      module data_set_mod
      use current_precision_mod
      use string_mod
      use string_fmt_mod
      use VF_mod
      use SF_mod
      use IO_tools_mod
      implicit none
      !  call init(DS,m,dir,name)
      !  call export(DS,m,B)

      private
      public :: data_set
      public :: init,delete

      interface init;     module procedure init_data_set;   end interface
      interface init;     module procedure init_name;       end interface
      interface delete;   module procedure delete_data_set; end interface

      interface export;   module procedure export_data_set; end interface
      interface print;    module procedure print_data_set;  end interface

       type data_set
        ! Example:
        ! TITLE = "3D Vector Field"
        ! VARIABLES = "X","Y","Z","Unp_x","Unp_y","Unp_z"
        ! ZONE , I = 0000000065, J = 0000000065, K = 0000000002 DATAPACKING = POINT
        ! ZONE , T = "0000000065" I = 0000000065, J = 0000000065, K = 0000000002 DATAPACKING = POINT

         type(string) :: dir                         ! Directory
         type(string) :: name                        ! Filename
         type(string_fmt) :: title                   ! Entire title line
         type(string_fmt) :: variables               ! Entire variables line
         type(string_fmt) :: zone                    ! Entire zone line

         type(string),dimension(3) :: ind,coord      ! Filename

         integer :: dataType                             ! Data type (cellcenter,node,face,edge)
         integer :: un                                   ! Unit (for output)
         integer :: pad                                  ! Pad output (skip ghost cells)
         integer :: dim                                  ! Dimensions of data_set (1D,2D,3D)
       end type

      contains

      ! ********************************************************
      ! ************************* INIT *************************
      ! ********************************************************

      subroutine init_data_set(DS,dir,name,dimensions,components)
        implicit none
        type(data_set),intent(inout) :: DS
        character(len=*),intent(in) :: dir,name
        integer,intent(in) :: dimensions,components
        call init(DS%dir,dir)
        call init(DS%name,name)
        call init(DS%title,'TITLE = "'//str(DS%name)//'"')
        DS%un = new_and_open(str(DS%dir),str(DS%name))

        call setCoord(DS,m,dimensions)
        call setVariables(DS)
        call setZone(DS)
        call setTypeReal(DS,m,U%x%RF(1)%s)

        call init(DS%title%fmt,'(A'//int2str(len(DS%title%s))//')')
        call init(DS%variables%fmt,'(A'//int2str(len(DS%variables%s))//')')
        call init(DS%zone%fmt,'(A'//int2str(len(DS%zone%s))//')')
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

      end subroutine

      subroutine setCoord(DS,m,dim)
        implicit none
        type(data_set),intent(inout) :: DS
        type(mesh),intent(in) :: m
        integer,intent(in) :: dim
        DS%dim = dim
        select case (DS%dim)
        case (1) ! 1D plot.
              if (m%plane_y.and.m%plane_z) then; call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
          elseif (m%plane_x.and.m%plane_x) then; call init(DS%ind(1),'J'); call init(DS%coord(1),'Y')
          elseif (m%plane_x.and.m%plane_y) then; call init(DS%ind(1),'K'); call init(DS%coord(1),'Z')
          else;stop 'Error: dim=1, N_cells.ne.1 for two directions in setType in data_set.f90'
          endif
        case (2) ! 2D plot.
          if (m%plane_x) then;     call init(DS%ind(1),'J'); call init(DS%coord(1),'Y')
                                   call init(DS%ind(2),'K'); call init(DS%coord(2),'Z')
          elseif (m%plane_y) then; call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
                                   call init(DS%ind(2),'K'); call init(DS%coord(2),'Z')
          elseif (m%plane_z) then; call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
                                   call init(DS%ind(2),'J'); call init(DS%coord(2),'Y')
          else; stop 'Error: dim=2, N_cells.ne.1 for any directions in setType in data_set.f90'
          endif
        case (3); call init(DS%ind(1),'I'); call init(DS%coord(1),'X')
                  call init(DS%ind(2),'J'); call init(DS%coord(2),'Y')
                  call init(DS%ind(3),'K'); call init(DS%coord(3),'Z')
        case default; stop 'Error: dim must = 1,2,3 in setType in data_set.f90'
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

      subroutine export_header(DS)
        implicit none
        type(data_set),intent(inout) :: DS
        write(DS%un,DS%title%fmt) DS%title%s
        write(DS%un,DS%variables%fmt) DS%variables%s
        write(DS%un,DS%zone%fmt) DS%zone%s
      end subroutine
      
      subroutine export_DS(DS,m,u)
        implicit none
        type(data_set),intent(inout) :: DS
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        integer i,j,k,t
         DS%un = new_and_open(str(DS%dir),str(DS%name))
         do k = 1,sz; do j = 1,sy; do i = 1,sx
           write(un,DS%fmt) x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
         enddo; enddo; enddo
         close(DS%un)
      end subroutine
      
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