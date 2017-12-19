     module benchmark_cases_mod
     use IO_tools_mod
     use key_value_pair_mod
     use associative_array_mod
     use preset_ID_mod
     implicit none

     private
     public :: benchmark_cases
     public :: init,delete,display,print,export,import

     public :: get_geometry
     public :: get_BC
     public :: get_IC

     type benchmark_cases
       type(associative_array) :: BMC
     end type

     interface init;         module procedure init_BMC;           end interface
     interface init;         module procedure init_BMC_copy;      end interface
     interface delete;       module procedure delete_BMC;         end interface
     interface display;      module procedure display_BMC;        end interface
     interface print;        module procedure print_BMC;          end interface
     interface export;       module procedure export_BMC;         end interface
     interface export;       module procedure export_BMC_wrapper; end interface
     interface import;       module procedure import_BMC;         end interface

     interface get_geometry; module procedure get_geometry_BMC;   end interface
     interface get_BC;       module procedure get_BC_BMC;         end interface
     interface get_IC;       module procedure get_IC_BMC;         end interface

     contains

     subroutine init_BMC(B)
       implicit none
       type(benchmark_cases),intent(inout) :: B
       logical :: F,T
       call delete(B)
       F = .false.
       T = .true.
       ! PID(T,U,p,B,B0,phi)
       ! call init(KVP,key,value,IC,BC,geometry)
       call init(B%BMC,KVP('user_defined'           ,F,PID(0,0,0,0,0,0),PID(0,2,0,0,0,0),1))
       call  add(B%BMC,KVP('Hydro_2D_LDC_Ghia'      ,T,PID(0,0,0,0,0,0),PID(0,1,0,0,0,0),2))
       call  add(B%BMC,KVP('Hydro_3D_LDC_Guj_stella',F,PID(0,0,0,0,0,0),PID(0,4,0,0,0,0),3))
       call  add(B%BMC,KVP('Hydro_3D_duct'          ,F,PID(0,0,0,0,0,0),PID(0,6,0,0,0,0),4))
       call  add(B%BMC,KVP('kinetic_MHD_2D_Weiss'   ,F,PID(1,2,3,4,5,6),PID(1,7,3,4,5,6),5))
       call  add(B%BMC,KVP('kinetic_MHD_2D_Parker'  ,F,PID(1,2,3,4,5,6),PID(1,8,3,4,5,6),6))
       call  add(B%BMC,KVP('MHD_2D_Bandaru'         ,F,PID(1,2,3,4,5,6),PID(1,11,3,4,5,6),7))
       call  add(B%BMC,KVP('MHD_3D_LDC_Sergey'      ,F,PID(1,2,3,4,5,6),PID(1,13,3,4,5,6),8))
       call  add(B%BMC,KVP('MHD_3D_Shercliff'       ,F,PID(1,2,3,4,5,6),PID(1,15,3,4,5,6),9))
       call  add(B%BMC,KVP('MHD_3D_Hunt'            ,F,PID(1,2,3,4,5,6),PID(1,16,3,4,5,6),10))
       call  add(B%BMC,KVP('MHD_3D_LDC_RVBC_cw5em1' ,F,PID(1,2,3,4,5,6),PID(1,3,3,4,5,6),11))
       call  add(B%BMC,KVP('MHD_3D_LDC_RVBC_cw5em2' ,F,PID(1,2,3,4,5,6),PID(1,3,3,4,5,6),12))
       call print(B)
     end subroutine

     subroutine init_BMC_copy(BMCs,BMCs_in)
       implicit none
       type(benchmark_cases),intent(inout) :: BMCs
       type(benchmark_cases),intent(in) :: BMCs_in
       call init(BMCs%BMC,BMCs_in%BMC)
     end subroutine

     subroutine delete_BMC(BMCs)
       implicit none
       type(benchmark_cases),intent(inout) :: BMCs
       call delete(BMCs%BMC)
     end subroutine

     subroutine export_BMC(BMCs,un)
       implicit none
       type(benchmark_cases),intent(in) :: BMCs
       integer,intent(in) :: un
       call export(BMCs%BMC,un)
     end subroutine

     subroutine import_BMC(BMCs,un)
       implicit none
       type(benchmark_cases),intent(inout) :: BMCs
       integer,intent(in) :: un
       call import(BMCs%BMC,un)
     end subroutine

     subroutine export_BMC_wrapper(BMCs,dir,name)
       implicit none
       type(benchmark_cases),intent(in) :: BMCs
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(BMCs,un)
       call close_and_message(un,dir,name)
     end subroutine

     subroutine display_BMC(BMCs,un)
       implicit none
       type(benchmark_cases),intent(in) :: BMCs
       integer,intent(in) :: un
       call display(BMCs%BMC,un)
     end subroutine

     subroutine print_BMC(BMCs)
       implicit none
       type(benchmark_cases),intent(in) :: BMCs
       call display(BMCs,6)
     end subroutine

     function get_geometry_BMC(BMCs) result(val)
       implicit none
       type(benchmark_cases),intent(in) :: BMCs
       integer :: i,val
       val = 0
       if (BMCs%BMC%n.gt.0) then
         do i=1,BMCs%BMC%n
           if (get_value(BMCs%BMC%KVP(i))) val = BMCs%BMC%KVP(i)%geometry
         enddo
       endif
     end function

     function get_BC_BMC(BMCs) result(BC)
       implicit none
       type(benchmark_cases),intent(in) :: BMCs
       type(preset_ID) :: BC
       integer :: i
       BC = PID(0,0,0,0,0,0)
       if (BMCs%BMC%n.gt.0) then
         do i=1,BMCs%BMC%n
           if (get_value(BMCs%BMC%KVP(i))) call init(BC,BMCs%BMC%KVP(i)%BC)
         enddo
       endif
     end function

     function get_IC_BMC(BMCs) result(IC)
       implicit none
       type(benchmark_cases),intent(in) :: BMCs
       type(preset_ID) :: IC
       integer :: i
       IC = PID(0,0,0,0,0,0)
       if (BMCs%BMC%n.gt.0) then
         do i=1,BMCs%BMC%n
           if (get_value(BMCs%BMC%KVP(i))) call init(IC,BMCs%BMC%KVP(i)%IC)
         enddo
       endif
     end function

     end module