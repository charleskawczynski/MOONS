      module import_SF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use current_precision_mod
      use IO_tools_mod
      use mesh_mod
      use SF_mod
      use import_g_mod
      use string_mod
      use string_aux_mod
      implicit none

      private
      public :: imp_3D_1C
      public :: imp_2D_1C

      contains

      subroutine imp_3D_1C(m,pad,un,arrfmt,name,A)
        implicit none
        type(SF),intent(inout) :: A
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i,DT
        read(un,*);read(un,*) ! Read tecplot header
        DT = getType_3D(m%g(1),A%RF(1)%s,name)
        do i=1,m%s
          read(un,*) ! Read tecplot header
          call imp_3D_1C_g(m%g(i),DT,pad,un,arrfmt,A%RF(i)%s,A%RF(i)%f)
        enddo
      end subroutine

      subroutine dummy_depricated(m,un,arrfmt,name)
        implicit none
        type(mesh),intent(inout) :: m
        integer,intent(in) :: un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i,i_th_domain,pad,N_domains,DT
        integer,dimension(3) :: s
        type(string),dimension(3) :: header
        logical :: TF,continue_loop
        write(*,*) 'name = ',name
        call init(header(1),read_line(un,200))
        call init(header(2),read_line(un,200))
        DT = 1; pad = 0
        TF = match(header(2),'"x"'); write(*,*) 'x is in header!: ',TF
        TF = match(header(2),'"y"'); write(*,*) 'y is in header!: ',TF
        TF = match(header(2),'"z"'); write(*,*) 'z is in header!: ',TF
        continue_loop = .true.
        N_domains = 1
        do while (continue_loop)
          call init(header(3),read_line(un,200))
          call get_first_header_info(i_th_domain,header(3),'T = ')
          call get_first_header_info(s(1),header(3),'I = ')
          call get_first_header_info(s(2),header(3),'J = ')
          call get_first_header_info(s(3),header(3),'K = ')
          ! call imp_3D_1C_g(m%g(i),DT,pad,un,arrfmt)
          continue_loop = .false.
        enddo
        stop 'Done in import_mesh in import_SF'
      end subroutine

      subroutine get_first_header_info(i,header,s_match)
        ! Retrieves first instance of s_match in header and returns integer i.
        ! Example:
        ! call get_first_header_info(s(1),'x = ',header)
        implicit none
        integer,intent(inout) :: i
        type(string),intent(inout) :: header
        character(len=*),intent(in) :: s_match
        if (match(header,s_match)) then
          i = str2int(str(get_first_int(header)))
          header = remove_substring(header,str(get_first_int(header)))
        endif
      end subroutine

      subroutine imp_2D_1C(m,pad,un,arrfmt,name,A,dir)
        implicit none
        type(SF),intent(inout) :: A
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer,dimension(2) :: s
        integer :: i,DT
        read(un,*);read(un,*) ! Read tecplot header
        select case (dir)
        case(1); s = (/A%RF(1)%s(2),A%RF(1)%s(3)/); DT = getType_2D(m%g(1),s,name,dir)
        case(2); s = (/A%RF(1)%s(1),A%RF(1)%s(3)/); DT = getType_2D(m%g(1),s,name,dir)
        case(3); s = (/A%RF(1)%s(1),A%RF(1)%s(2)/); DT = getType_2D(m%g(1),s,name,dir)
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
        select case (dir)
        case(1); do i=1,m%s
                   read(un,*) ! Read tecplot header
                   call imp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%RF(i)%f(2,:,:))
                 enddo
        case(2); do i=1,m%s
                   read(un,*) ! Read tecplot header
                   call imp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%RF(i)%f(:,2,:))
                 enddo
        case(3); do i=1,m%s
                   read(un,*) ! Read tecplot header
                   call imp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%RF(i)%f(:,:,2))
                 enddo
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
      end subroutine

      end module