       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module probe_set_ind_mod
       use probe_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use string_mod
       use dir_manip_mod
       implicit none

       private
       public :: probe_set_ind
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_probe_set_ind;              end interface
       interface delete;                 module procedure delete_probe_set_ind;                 end interface
       interface display;                module procedure display_probe_set_ind;                end interface
       interface display_short;          module procedure display_short_probe_set_ind;          end interface
       interface display;                module procedure display_wrap_probe_set_ind;           end interface
       interface print;                  module procedure print_probe_set_ind;                  end interface
       interface print_short;            module procedure print_short_probe_set_ind;            end interface
       interface export;                 module procedure export_probe_set_ind;                 end interface
       interface export_primitives;      module procedure export_primitives_probe_set_ind;      end interface
       interface import;                 module procedure import_probe_set_ind;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_probe_set_ind;end interface
       interface export_structured;      module procedure export_structured_D_probe_set_ind;    end interface
       interface import_structured;      module procedure import_structured_D_probe_set_ind;    end interface
       interface import_primitives;      module procedure import_primitives_probe_set_ind;      end interface
       interface export;                 module procedure export_wrap_probe_set_ind;            end interface
       interface import;                 module procedure import_wrap_probe_set_ind;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_probe_set_ind;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_probe_set_ind;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_probe_set_ind;      end interface

       type probe_set_ind
         type(probe) :: probe_divB
         type(probe) :: probe_divJ
         type(probe) :: JE
         type(probe) :: JE_fluid
         type(probe),dimension(3) :: ME
         type(probe),dimension(3) :: ME_fluid
         type(probe),dimension(3) :: ME_conductor
         type(probe),dimension(3) :: probe_dB0dt
         type(probe),dimension(3) :: Bx
         type(probe),dimension(3) :: By
         type(probe),dimension(3) :: Bz
         type(probe),dimension(3) :: probe_B0
         type(probe),dimension(3) :: probe_B1
         type(probe),dimension(3) :: probe_Btot
         type(probe) :: amax_JxB_x
         type(probe) :: amax_JxB_y
         type(probe) :: amax_JxB_z
         type(probe) :: amax_JxB
         type(probe) :: amax_stress_xx
         type(probe) :: amax_stress_xy
         type(probe) :: amax_stress_xz
         type(probe) :: amax_stress_yx
         type(probe) :: amax_stress_yy
         type(probe) :: amax_stress_yz
         type(probe) :: amax_stress_zx
         type(probe) :: amax_stress_zy
         type(probe) :: amax_stress_zz
         type(probe) :: amax_stress_walls_xx
         type(probe) :: amax_stress_walls_xy
         type(probe) :: amax_stress_walls_xz
         type(probe) :: amax_stress_walls_yx
         type(probe) :: amax_stress_walls_yy
         type(probe) :: amax_stress_walls_yz
         type(probe) :: amax_stress_walls_zx
         type(probe) :: amax_stress_walls_zy
         type(probe) :: amax_stress_walls_zz
       end type

       contains

       subroutine init_copy_probe_set_ind(this,that)
         implicit none
         type(probe_set_ind),intent(inout) :: this
         type(probe_set_ind),intent(in) :: that
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         call delete(this)
         call init(this%probe_divB,that%probe_divB)
         call init(this%probe_divJ,that%probe_divJ)
         call init(this%JE,that%JE)
         call init(this%JE_fluid,that%JE_fluid)
         s_ME = size(that%ME)
         do i_ME=1,s_ME
           call init(this%ME(i_ME),that%ME(i_ME))
         enddo
         s_ME_fluid = size(that%ME_fluid)
         do i_ME_fluid=1,s_ME_fluid
           call init(this%ME_fluid(i_ME_fluid),that%ME_fluid(i_ME_fluid))
         enddo
         s_ME_conductor = size(that%ME_conductor)
         do i_ME_conductor=1,s_ME_conductor
           call init(this%ME_conductor(i_ME_conductor),&
           that%ME_conductor(i_ME_conductor))
         enddo
         s_probe_dB0dt = size(that%probe_dB0dt)
         do i_probe_dB0dt=1,s_probe_dB0dt
           call init(this%probe_dB0dt(i_probe_dB0dt),&
           that%probe_dB0dt(i_probe_dB0dt))
         enddo
         s_Bx = size(that%Bx)
         do i_Bx=1,s_Bx
           call init(this%Bx(i_Bx),that%Bx(i_Bx))
         enddo
         s_By = size(that%By)
         do i_By=1,s_By
           call init(this%By(i_By),that%By(i_By))
         enddo
         s_Bz = size(that%Bz)
         do i_Bz=1,s_Bz
           call init(this%Bz(i_Bz),that%Bz(i_Bz))
         enddo
         s_probe_B0 = size(that%probe_B0)
         do i_probe_B0=1,s_probe_B0
           call init(this%probe_B0(i_probe_B0),that%probe_B0(i_probe_B0))
         enddo
         s_probe_B1 = size(that%probe_B1)
         do i_probe_B1=1,s_probe_B1
           call init(this%probe_B1(i_probe_B1),that%probe_B1(i_probe_B1))
         enddo
         s_probe_Btot = size(that%probe_Btot)
         do i_probe_Btot=1,s_probe_Btot
           call init(this%probe_Btot(i_probe_Btot),&
           that%probe_Btot(i_probe_Btot))
         enddo
         call init(this%amax_JxB_x,that%amax_JxB_x)
         call init(this%amax_JxB_y,that%amax_JxB_y)
         call init(this%amax_JxB_z,that%amax_JxB_z)
         call init(this%amax_JxB,that%amax_JxB)
         call init(this%amax_stress_xx,that%amax_stress_xx)
         call init(this%amax_stress_xy,that%amax_stress_xy)
         call init(this%amax_stress_xz,that%amax_stress_xz)
         call init(this%amax_stress_yx,that%amax_stress_yx)
         call init(this%amax_stress_yy,that%amax_stress_yy)
         call init(this%amax_stress_yz,that%amax_stress_yz)
         call init(this%amax_stress_zx,that%amax_stress_zx)
         call init(this%amax_stress_zy,that%amax_stress_zy)
         call init(this%amax_stress_zz,that%amax_stress_zz)
         call init(this%amax_stress_walls_xx,that%amax_stress_walls_xx)
         call init(this%amax_stress_walls_xy,that%amax_stress_walls_xy)
         call init(this%amax_stress_walls_xz,that%amax_stress_walls_xz)
         call init(this%amax_stress_walls_yx,that%amax_stress_walls_yx)
         call init(this%amax_stress_walls_yy,that%amax_stress_walls_yy)
         call init(this%amax_stress_walls_yz,that%amax_stress_walls_yz)
         call init(this%amax_stress_walls_zx,that%amax_stress_walls_zx)
         call init(this%amax_stress_walls_zy,that%amax_stress_walls_zy)
         call init(this%amax_stress_walls_zz,that%amax_stress_walls_zz)
       end subroutine

       subroutine delete_probe_set_ind(this)
         implicit none
         type(probe_set_ind),intent(inout) :: this
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         call delete(this%probe_divB)
         call delete(this%probe_divJ)
         call delete(this%JE)
         call delete(this%JE_fluid)
         s_ME = size(this%ME)
         do i_ME=1,s_ME
           call delete(this%ME(i_ME))
         enddo
         s_ME_fluid = size(this%ME_fluid)
         do i_ME_fluid=1,s_ME_fluid
           call delete(this%ME_fluid(i_ME_fluid))
         enddo
         s_ME_conductor = size(this%ME_conductor)
         do i_ME_conductor=1,s_ME_conductor
           call delete(this%ME_conductor(i_ME_conductor))
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         do i_probe_dB0dt=1,s_probe_dB0dt
           call delete(this%probe_dB0dt(i_probe_dB0dt))
         enddo
         s_Bx = size(this%Bx)
         do i_Bx=1,s_Bx
           call delete(this%Bx(i_Bx))
         enddo
         s_By = size(this%By)
         do i_By=1,s_By
           call delete(this%By(i_By))
         enddo
         s_Bz = size(this%Bz)
         do i_Bz=1,s_Bz
           call delete(this%Bz(i_Bz))
         enddo
         s_probe_B0 = size(this%probe_B0)
         do i_probe_B0=1,s_probe_B0
           call delete(this%probe_B0(i_probe_B0))
         enddo
         s_probe_B1 = size(this%probe_B1)
         do i_probe_B1=1,s_probe_B1
           call delete(this%probe_B1(i_probe_B1))
         enddo
         s_probe_Btot = size(this%probe_Btot)
         do i_probe_Btot=1,s_probe_Btot
           call delete(this%probe_Btot(i_probe_Btot))
         enddo
         call delete(this%amax_JxB_x)
         call delete(this%amax_JxB_y)
         call delete(this%amax_JxB_z)
         call delete(this%amax_JxB)
         call delete(this%amax_stress_xx)
         call delete(this%amax_stress_xy)
         call delete(this%amax_stress_xz)
         call delete(this%amax_stress_yx)
         call delete(this%amax_stress_yy)
         call delete(this%amax_stress_yz)
         call delete(this%amax_stress_zx)
         call delete(this%amax_stress_zy)
         call delete(this%amax_stress_zz)
         call delete(this%amax_stress_walls_xx)
         call delete(this%amax_stress_walls_xy)
         call delete(this%amax_stress_walls_xz)
         call delete(this%amax_stress_walls_yx)
         call delete(this%amax_stress_walls_yy)
         call delete(this%amax_stress_walls_yz)
         call delete(this%amax_stress_walls_zx)
         call delete(this%amax_stress_walls_zy)
         call delete(this%amax_stress_walls_zz)
       end subroutine

       subroutine display_probe_set_ind(this,un)
         implicit none
         type(probe_set_ind),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         call display(this%probe_divB,un)
         call display(this%probe_divJ,un)
         call display(this%JE,un)
         call display(this%JE_fluid,un)
         s_ME = size(this%ME)
         do i_ME=1,s_ME
           call display(this%ME(i_ME),un)
         enddo
         s_ME_fluid = size(this%ME_fluid)
         do i_ME_fluid=1,s_ME_fluid
           call display(this%ME_fluid(i_ME_fluid),un)
         enddo
         s_ME_conductor = size(this%ME_conductor)
         do i_ME_conductor=1,s_ME_conductor
           call display(this%ME_conductor(i_ME_conductor),un)
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         do i_probe_dB0dt=1,s_probe_dB0dt
           call display(this%probe_dB0dt(i_probe_dB0dt),un)
         enddo
         s_Bx = size(this%Bx)
         do i_Bx=1,s_Bx
           call display(this%Bx(i_Bx),un)
         enddo
         s_By = size(this%By)
         do i_By=1,s_By
           call display(this%By(i_By),un)
         enddo
         s_Bz = size(this%Bz)
         do i_Bz=1,s_Bz
           call display(this%Bz(i_Bz),un)
         enddo
         s_probe_B0 = size(this%probe_B0)
         do i_probe_B0=1,s_probe_B0
           call display(this%probe_B0(i_probe_B0),un)
         enddo
         s_probe_B1 = size(this%probe_B1)
         do i_probe_B1=1,s_probe_B1
           call display(this%probe_B1(i_probe_B1),un)
         enddo
         s_probe_Btot = size(this%probe_Btot)
         do i_probe_Btot=1,s_probe_Btot
           call display(this%probe_Btot(i_probe_Btot),un)
         enddo
         call display(this%amax_JxB_x,un)
         call display(this%amax_JxB_y,un)
         call display(this%amax_JxB_z,un)
         call display(this%amax_JxB,un)
         call display(this%amax_stress_xx,un)
         call display(this%amax_stress_xy,un)
         call display(this%amax_stress_xz,un)
         call display(this%amax_stress_yx,un)
         call display(this%amax_stress_yy,un)
         call display(this%amax_stress_yz,un)
         call display(this%amax_stress_zx,un)
         call display(this%amax_stress_zy,un)
         call display(this%amax_stress_zz,un)
         call display(this%amax_stress_walls_xx,un)
         call display(this%amax_stress_walls_xy,un)
         call display(this%amax_stress_walls_xz,un)
         call display(this%amax_stress_walls_yx,un)
         call display(this%amax_stress_walls_yy,un)
         call display(this%amax_stress_walls_yz,un)
         call display(this%amax_stress_walls_zx,un)
         call display(this%amax_stress_walls_zy,un)
         call display(this%amax_stress_walls_zz,un)
       end subroutine

       subroutine display_short_probe_set_ind(this,un)
         implicit none
         type(probe_set_ind),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         call display(this%probe_divB,un)
         call display(this%probe_divJ,un)
         call display(this%JE,un)
         call display(this%JE_fluid,un)
         s_ME = size(this%ME)
         do i_ME=1,s_ME
           call display(this%ME(i_ME),un)
         enddo
         s_ME_fluid = size(this%ME_fluid)
         do i_ME_fluid=1,s_ME_fluid
           call display(this%ME_fluid(i_ME_fluid),un)
         enddo
         s_ME_conductor = size(this%ME_conductor)
         do i_ME_conductor=1,s_ME_conductor
           call display(this%ME_conductor(i_ME_conductor),un)
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         do i_probe_dB0dt=1,s_probe_dB0dt
           call display(this%probe_dB0dt(i_probe_dB0dt),un)
         enddo
         s_Bx = size(this%Bx)
         do i_Bx=1,s_Bx
           call display(this%Bx(i_Bx),un)
         enddo
         s_By = size(this%By)
         do i_By=1,s_By
           call display(this%By(i_By),un)
         enddo
         s_Bz = size(this%Bz)
         do i_Bz=1,s_Bz
           call display(this%Bz(i_Bz),un)
         enddo
         s_probe_B0 = size(this%probe_B0)
         do i_probe_B0=1,s_probe_B0
           call display(this%probe_B0(i_probe_B0),un)
         enddo
         s_probe_B1 = size(this%probe_B1)
         do i_probe_B1=1,s_probe_B1
           call display(this%probe_B1(i_probe_B1),un)
         enddo
         s_probe_Btot = size(this%probe_Btot)
         do i_probe_Btot=1,s_probe_Btot
           call display(this%probe_Btot(i_probe_Btot),un)
         enddo
         call display(this%amax_JxB_x,un)
         call display(this%amax_JxB_y,un)
         call display(this%amax_JxB_z,un)
         call display(this%amax_JxB,un)
         call display(this%amax_stress_xx,un)
         call display(this%amax_stress_xy,un)
         call display(this%amax_stress_xz,un)
         call display(this%amax_stress_yx,un)
         call display(this%amax_stress_yy,un)
         call display(this%amax_stress_yz,un)
         call display(this%amax_stress_zx,un)
         call display(this%amax_stress_zy,un)
         call display(this%amax_stress_zz,un)
         call display(this%amax_stress_walls_xx,un)
         call display(this%amax_stress_walls_xy,un)
         call display(this%amax_stress_walls_xz,un)
         call display(this%amax_stress_walls_yx,un)
         call display(this%amax_stress_walls_yy,un)
         call display(this%amax_stress_walls_yz,un)
         call display(this%amax_stress_walls_zx,un)
         call display(this%amax_stress_walls_zy,un)
         call display(this%amax_stress_walls_zz,un)
       end subroutine

       subroutine display_wrap_probe_set_ind(this,dir,name)
         implicit none
         type(probe_set_ind),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_probe_set_ind(this)
         implicit none
         type(probe_set_ind),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_probe_set_ind(this)
         implicit none
         type(probe_set_ind),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_probe_set_ind(this,un)
         implicit none
         type(probe_set_ind),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         call export(this%probe_divB,un)
         call export(this%probe_divJ,un)
         call export(this%JE,un)
         call export(this%JE_fluid,un)
         s_ME = size(this%ME)
         write(un,*) s_ME
         do i_ME=1,s_ME
           call export(this%ME(i_ME),un)
         enddo
         s_ME_fluid = size(this%ME_fluid)
         write(un,*) s_ME_fluid
         do i_ME_fluid=1,s_ME_fluid
           call export(this%ME_fluid(i_ME_fluid),un)
         enddo
         s_ME_conductor = size(this%ME_conductor)
         write(un,*) s_ME_conductor
         do i_ME_conductor=1,s_ME_conductor
           call export(this%ME_conductor(i_ME_conductor),un)
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         write(un,*) s_probe_dB0dt
         do i_probe_dB0dt=1,s_probe_dB0dt
           call export(this%probe_dB0dt(i_probe_dB0dt),un)
         enddo
         s_Bx = size(this%Bx)
         write(un,*) s_Bx
         do i_Bx=1,s_Bx
           call export(this%Bx(i_Bx),un)
         enddo
         s_By = size(this%By)
         write(un,*) s_By
         do i_By=1,s_By
           call export(this%By(i_By),un)
         enddo
         s_Bz = size(this%Bz)
         write(un,*) s_Bz
         do i_Bz=1,s_Bz
           call export(this%Bz(i_Bz),un)
         enddo
         s_probe_B0 = size(this%probe_B0)
         write(un,*) s_probe_B0
         do i_probe_B0=1,s_probe_B0
           call export(this%probe_B0(i_probe_B0),un)
         enddo
         s_probe_B1 = size(this%probe_B1)
         write(un,*) s_probe_B1
         do i_probe_B1=1,s_probe_B1
           call export(this%probe_B1(i_probe_B1),un)
         enddo
         s_probe_Btot = size(this%probe_Btot)
         write(un,*) s_probe_Btot
         do i_probe_Btot=1,s_probe_Btot
           call export(this%probe_Btot(i_probe_Btot),un)
         enddo
         call export(this%amax_JxB_x,un)
         call export(this%amax_JxB_y,un)
         call export(this%amax_JxB_z,un)
         call export(this%amax_JxB,un)
         call export(this%amax_stress_xx,un)
         call export(this%amax_stress_xy,un)
         call export(this%amax_stress_xz,un)
         call export(this%amax_stress_yx,un)
         call export(this%amax_stress_yy,un)
         call export(this%amax_stress_yz,un)
         call export(this%amax_stress_zx,un)
         call export(this%amax_stress_zy,un)
         call export(this%amax_stress_zz,un)
         call export(this%amax_stress_walls_xx,un)
         call export(this%amax_stress_walls_xy,un)
         call export(this%amax_stress_walls_xz,un)
         call export(this%amax_stress_walls_yx,un)
         call export(this%amax_stress_walls_yy,un)
         call export(this%amax_stress_walls_yz,un)
         call export(this%amax_stress_walls_zx,un)
         call export(this%amax_stress_walls_zy,un)
         call export(this%amax_stress_walls_zz,un)
       end subroutine

       subroutine import_probe_set_ind(this,un)
         implicit none
         type(probe_set_ind),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         call delete(this)
         call import(this%probe_divB,un)
         call import(this%probe_divJ,un)
         call import(this%JE,un)
         call import(this%JE_fluid,un)
         read(un,*) s_ME
         if (s_ME.gt.0) then
           do i_ME=1,s_ME
             call import(this%ME(i_ME),un)
           enddo
         endif
         read(un,*) s_ME_fluid
         if (s_ME_fluid.gt.0) then
           do i_ME_fluid=1,s_ME_fluid
             call import(this%ME_fluid(i_ME_fluid),un)
           enddo
         endif
         read(un,*) s_ME_conductor
         if (s_ME_conductor.gt.0) then
           do i_ME_conductor=1,s_ME_conductor
             call import(this%ME_conductor(i_ME_conductor),un)
           enddo
         endif
         read(un,*) s_probe_dB0dt
         if (s_probe_dB0dt.gt.0) then
           do i_probe_dB0dt=1,s_probe_dB0dt
             call import(this%probe_dB0dt(i_probe_dB0dt),un)
           enddo
         endif
         read(un,*) s_Bx
         if (s_Bx.gt.0) then
           do i_Bx=1,s_Bx
             call import(this%Bx(i_Bx),un)
           enddo
         endif
         read(un,*) s_By
         if (s_By.gt.0) then
           do i_By=1,s_By
             call import(this%By(i_By),un)
           enddo
         endif
         read(un,*) s_Bz
         if (s_Bz.gt.0) then
           do i_Bz=1,s_Bz
             call import(this%Bz(i_Bz),un)
           enddo
         endif
         read(un,*) s_probe_B0
         if (s_probe_B0.gt.0) then
           do i_probe_B0=1,s_probe_B0
             call import(this%probe_B0(i_probe_B0),un)
           enddo
         endif
         read(un,*) s_probe_B1
         if (s_probe_B1.gt.0) then
           do i_probe_B1=1,s_probe_B1
             call import(this%probe_B1(i_probe_B1),un)
           enddo
         endif
         read(un,*) s_probe_Btot
         if (s_probe_Btot.gt.0) then
           do i_probe_Btot=1,s_probe_Btot
             call import(this%probe_Btot(i_probe_Btot),un)
           enddo
         endif
         call import(this%amax_JxB_x,un)
         call import(this%amax_JxB_y,un)
         call import(this%amax_JxB_z,un)
         call import(this%amax_JxB,un)
         call import(this%amax_stress_xx,un)
         call import(this%amax_stress_xy,un)
         call import(this%amax_stress_xz,un)
         call import(this%amax_stress_yx,un)
         call import(this%amax_stress_yy,un)
         call import(this%amax_stress_yz,un)
         call import(this%amax_stress_zx,un)
         call import(this%amax_stress_zy,un)
         call import(this%amax_stress_zz,un)
         call import(this%amax_stress_walls_xx,un)
         call import(this%amax_stress_walls_xy,un)
         call import(this%amax_stress_walls_xz,un)
         call import(this%amax_stress_walls_yx,un)
         call import(this%amax_stress_walls_yy,un)
         call import(this%amax_stress_walls_yz,un)
         call import(this%amax_stress_walls_zx,un)
         call import(this%amax_stress_walls_zy,un)
         call import(this%amax_stress_walls_zz,un)
       end subroutine

       subroutine export_primitives_probe_set_ind(this,un)
         implicit none
         type(probe_set_ind),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_probe_set_ind(this,un)
         implicit none
         type(probe_set_ind),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_probe_set_ind(this,dir,name)
         implicit none
         type(probe_set_ind),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_probe_set_ind(this,dir,name)
         implicit none
         type(probe_set_ind),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_probe_set_ind(this,dir)
         implicit none
         type(probe_set_ind),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         call suppress_warnings(this)
         call set_IO_dir(this%probe_divB,dir//'probe_divB'//fortran_PS)
         call set_IO_dir(this%probe_divJ,dir//'probe_divJ'//fortran_PS)
         call set_IO_dir(this%JE,dir//'JE'//fortran_PS)
         call set_IO_dir(this%JE_fluid,dir//'JE_fluid'//fortran_PS)
         s_ME = size(this%ME)
         do i_ME=1,s_ME
           call set_IO_dir(this%ME(i_ME),&
           dir//'ME_'//int2str(i_ME)//fortran_PS)
         enddo
         s_ME_fluid = size(this%ME_fluid)
         do i_ME_fluid=1,s_ME_fluid
           call set_IO_dir(this%ME_fluid(i_ME_fluid),&
           dir//'ME_fluid_'//int2str(i_ME_fluid)//fortran_PS)
         enddo
         s_ME_conductor = size(this%ME_conductor)
         do i_ME_conductor=1,s_ME_conductor
           call set_IO_dir(this%ME_conductor(i_ME_conductor),&
           dir//'ME_conductor_'//int2str(i_ME_conductor)//fortran_PS)
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         do i_probe_dB0dt=1,s_probe_dB0dt
           call set_IO_dir(this%probe_dB0dt(i_probe_dB0dt),&
           dir//'probe_dB0dt_'//int2str(i_probe_dB0dt)//fortran_PS)
         enddo
         s_Bx = size(this%Bx)
         do i_Bx=1,s_Bx
           call set_IO_dir(this%Bx(i_Bx),&
           dir//'Bx_'//int2str(i_Bx)//fortran_PS)
         enddo
         s_By = size(this%By)
         do i_By=1,s_By
           call set_IO_dir(this%By(i_By),&
           dir//'By_'//int2str(i_By)//fortran_PS)
         enddo
         s_Bz = size(this%Bz)
         do i_Bz=1,s_Bz
           call set_IO_dir(this%Bz(i_Bz),&
           dir//'Bz_'//int2str(i_Bz)//fortran_PS)
         enddo
         s_probe_B0 = size(this%probe_B0)
         do i_probe_B0=1,s_probe_B0
           call set_IO_dir(this%probe_B0(i_probe_B0),&
           dir//'probe_B0_'//int2str(i_probe_B0)//fortran_PS)
         enddo
         s_probe_B1 = size(this%probe_B1)
         do i_probe_B1=1,s_probe_B1
           call set_IO_dir(this%probe_B1(i_probe_B1),&
           dir//'probe_B1_'//int2str(i_probe_B1)//fortran_PS)
         enddo
         s_probe_Btot = size(this%probe_Btot)
         do i_probe_Btot=1,s_probe_Btot
           call set_IO_dir(this%probe_Btot(i_probe_Btot),&
           dir//'probe_Btot_'//int2str(i_probe_Btot)//fortran_PS)
         enddo
         call set_IO_dir(this%amax_JxB_x,dir//'amax_JxB_x'//fortran_PS)
         call set_IO_dir(this%amax_JxB_y,dir//'amax_JxB_y'//fortran_PS)
         call set_IO_dir(this%amax_JxB_z,dir//'amax_JxB_z'//fortran_PS)
         call set_IO_dir(this%amax_JxB,dir//'amax_JxB'//fortran_PS)
         call set_IO_dir(this%amax_stress_xx,&
         dir//'amax_stress_xx'//fortran_PS)
         call set_IO_dir(this%amax_stress_xy,&
         dir//'amax_stress_xy'//fortran_PS)
         call set_IO_dir(this%amax_stress_xz,&
         dir//'amax_stress_xz'//fortran_PS)
         call set_IO_dir(this%amax_stress_yx,&
         dir//'amax_stress_yx'//fortran_PS)
         call set_IO_dir(this%amax_stress_yy,&
         dir//'amax_stress_yy'//fortran_PS)
         call set_IO_dir(this%amax_stress_yz,&
         dir//'amax_stress_yz'//fortran_PS)
         call set_IO_dir(this%amax_stress_zx,&
         dir//'amax_stress_zx'//fortran_PS)
         call set_IO_dir(this%amax_stress_zy,&
         dir//'amax_stress_zy'//fortran_PS)
         call set_IO_dir(this%amax_stress_zz,&
         dir//'amax_stress_zz'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_xx,&
         dir//'amax_stress_walls_xx'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_xy,&
         dir//'amax_stress_walls_xy'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_xz,&
         dir//'amax_stress_walls_xz'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_yx,&
         dir//'amax_stress_walls_yx'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_yy,&
         dir//'amax_stress_walls_yy'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_yz,&
         dir//'amax_stress_walls_yz'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_zx,&
         dir//'amax_stress_walls_zx'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_zy,&
         dir//'amax_stress_walls_zy'//fortran_PS)
         call set_IO_dir(this%amax_stress_walls_zz,&
         dir//'amax_stress_walls_zz'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_probe_set_ind(this,dir)
         implicit none
         type(probe_set_ind),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%probe_divB,dir//'probe_divB'//fortran_PS)
         call make_IO_dir(this%probe_divJ,dir//'probe_divJ'//fortran_PS)
         call make_IO_dir(this%JE,dir//'JE'//fortran_PS)
         call make_IO_dir(this%JE_fluid,dir//'JE_fluid'//fortran_PS)
         s_ME = size(this%ME)
         do i_ME=1,s_ME
           call make_IO_dir(this%ME(i_ME),&
           dir//'ME_'//int2str(i_ME)//fortran_PS)
         enddo
         s_ME_fluid = size(this%ME_fluid)
         do i_ME_fluid=1,s_ME_fluid
           call make_IO_dir(this%ME_fluid(i_ME_fluid),&
           dir//'ME_fluid_'//int2str(i_ME_fluid)//fortran_PS)
         enddo
         s_ME_conductor = size(this%ME_conductor)
         do i_ME_conductor=1,s_ME_conductor
           call make_IO_dir(this%ME_conductor(i_ME_conductor),&
           dir//'ME_conductor_'//int2str(i_ME_conductor)//fortran_PS)
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         do i_probe_dB0dt=1,s_probe_dB0dt
           call make_IO_dir(this%probe_dB0dt(i_probe_dB0dt),&
           dir//'probe_dB0dt_'//int2str(i_probe_dB0dt)//fortran_PS)
         enddo
         s_Bx = size(this%Bx)
         do i_Bx=1,s_Bx
           call make_IO_dir(this%Bx(i_Bx),&
           dir//'Bx_'//int2str(i_Bx)//fortran_PS)
         enddo
         s_By = size(this%By)
         do i_By=1,s_By
           call make_IO_dir(this%By(i_By),&
           dir//'By_'//int2str(i_By)//fortran_PS)
         enddo
         s_Bz = size(this%Bz)
         do i_Bz=1,s_Bz
           call make_IO_dir(this%Bz(i_Bz),&
           dir//'Bz_'//int2str(i_Bz)//fortran_PS)
         enddo
         s_probe_B0 = size(this%probe_B0)
         do i_probe_B0=1,s_probe_B0
           call make_IO_dir(this%probe_B0(i_probe_B0),&
           dir//'probe_B0_'//int2str(i_probe_B0)//fortran_PS)
         enddo
         s_probe_B1 = size(this%probe_B1)
         do i_probe_B1=1,s_probe_B1
           call make_IO_dir(this%probe_B1(i_probe_B1),&
           dir//'probe_B1_'//int2str(i_probe_B1)//fortran_PS)
         enddo
         s_probe_Btot = size(this%probe_Btot)
         do i_probe_Btot=1,s_probe_Btot
           call make_IO_dir(this%probe_Btot(i_probe_Btot),&
           dir//'probe_Btot_'//int2str(i_probe_Btot)//fortran_PS)
         enddo
         call make_IO_dir(this%amax_JxB_x,dir//'amax_JxB_x'//fortran_PS)
         call make_IO_dir(this%amax_JxB_y,dir//'amax_JxB_y'//fortran_PS)
         call make_IO_dir(this%amax_JxB_z,dir//'amax_JxB_z'//fortran_PS)
         call make_IO_dir(this%amax_JxB,dir//'amax_JxB'//fortran_PS)
         call make_IO_dir(this%amax_stress_xx,&
         dir//'amax_stress_xx'//fortran_PS)
         call make_IO_dir(this%amax_stress_xy,&
         dir//'amax_stress_xy'//fortran_PS)
         call make_IO_dir(this%amax_stress_xz,&
         dir//'amax_stress_xz'//fortran_PS)
         call make_IO_dir(this%amax_stress_yx,&
         dir//'amax_stress_yx'//fortran_PS)
         call make_IO_dir(this%amax_stress_yy,&
         dir//'amax_stress_yy'//fortran_PS)
         call make_IO_dir(this%amax_stress_yz,&
         dir//'amax_stress_yz'//fortran_PS)
         call make_IO_dir(this%amax_stress_zx,&
         dir//'amax_stress_zx'//fortran_PS)
         call make_IO_dir(this%amax_stress_zy,&
         dir//'amax_stress_zy'//fortran_PS)
         call make_IO_dir(this%amax_stress_zz,&
         dir//'amax_stress_zz'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_xx,&
         dir//'amax_stress_walls_xx'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_xy,&
         dir//'amax_stress_walls_xy'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_xz,&
         dir//'amax_stress_walls_xz'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_yx,&
         dir//'amax_stress_walls_yx'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_yy,&
         dir//'amax_stress_walls_yy'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_yz,&
         dir//'amax_stress_walls_yz'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_zx,&
         dir//'amax_stress_walls_zx'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_zy,&
         dir//'amax_stress_walls_zy'//fortran_PS)
         call make_IO_dir(this%amax_stress_walls_zz,&
         dir//'amax_stress_walls_zz'//fortran_PS)
       end subroutine

       subroutine export_folder_structure_probe_set_ind(this,dir)
         implicit none
         type(probe_set_ind),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         integer :: un
         call export_structured(this%probe_divB,&
         dir//'probe_divB'//fortran_PS)
         call export_structured(this%probe_divJ,&
         dir//'probe_divJ'//fortran_PS)
         call export_structured(this%JE,dir//'JE'//fortran_PS)
         call export_structured(this%JE_fluid,dir//'JE_fluid'//fortran_PS)
         s_ME = size(this%ME)
         write(un,*) s_ME
         do i_ME=1,s_ME
           call export_structured(this%ME(i_ME),&
           dir//'ME_'//int2str(i_ME)//fortran_PS)
         enddo
         s_ME_fluid = size(this%ME_fluid)
         write(un,*) s_ME_fluid
         do i_ME_fluid=1,s_ME_fluid
           call export_structured(this%ME_fluid(i_ME_fluid),&
           dir//'ME_fluid_'//int2str(i_ME_fluid)//fortran_PS)
         enddo
         s_ME_conductor = size(this%ME_conductor)
         write(un,*) s_ME_conductor
         do i_ME_conductor=1,s_ME_conductor
           call export_structured(this%ME_conductor(i_ME_conductor),&
           dir//'ME_conductor_'//int2str(i_ME_conductor)//fortran_PS)
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         write(un,*) s_probe_dB0dt
         do i_probe_dB0dt=1,s_probe_dB0dt
           call export_structured(this%probe_dB0dt(i_probe_dB0dt),&
           dir//'probe_dB0dt_'//int2str(i_probe_dB0dt)//fortran_PS)
         enddo
         s_Bx = size(this%Bx)
         write(un,*) s_Bx
         do i_Bx=1,s_Bx
           call export_structured(this%Bx(i_Bx),&
           dir//'Bx_'//int2str(i_Bx)//fortran_PS)
         enddo
         s_By = size(this%By)
         write(un,*) s_By
         do i_By=1,s_By
           call export_structured(this%By(i_By),&
           dir//'By_'//int2str(i_By)//fortran_PS)
         enddo
         s_Bz = size(this%Bz)
         write(un,*) s_Bz
         do i_Bz=1,s_Bz
           call export_structured(this%Bz(i_Bz),&
           dir//'Bz_'//int2str(i_Bz)//fortran_PS)
         enddo
         s_probe_B0 = size(this%probe_B0)
         write(un,*) s_probe_B0
         do i_probe_B0=1,s_probe_B0
           call export_structured(this%probe_B0(i_probe_B0),&
           dir//'probe_B0_'//int2str(i_probe_B0)//fortran_PS)
         enddo
         s_probe_B1 = size(this%probe_B1)
         write(un,*) s_probe_B1
         do i_probe_B1=1,s_probe_B1
           call export_structured(this%probe_B1(i_probe_B1),&
           dir//'probe_B1_'//int2str(i_probe_B1)//fortran_PS)
         enddo
         s_probe_Btot = size(this%probe_Btot)
         write(un,*) s_probe_Btot
         do i_probe_Btot=1,s_probe_Btot
           call export_structured(this%probe_Btot(i_probe_Btot),&
           dir//'probe_Btot_'//int2str(i_probe_Btot)//fortran_PS)
         enddo
         call export_structured(this%amax_JxB_x,&
         dir//'amax_JxB_x'//fortran_PS)
         call export_structured(this%amax_JxB_y,&
         dir//'amax_JxB_y'//fortran_PS)
         call export_structured(this%amax_JxB_z,&
         dir//'amax_JxB_z'//fortran_PS)
         call export_structured(this%amax_JxB,dir//'amax_JxB'//fortran_PS)
         call export_structured(this%amax_stress_xx,&
         dir//'amax_stress_xx'//fortran_PS)
         call export_structured(this%amax_stress_xy,&
         dir//'amax_stress_xy'//fortran_PS)
         call export_structured(this%amax_stress_xz,&
         dir//'amax_stress_xz'//fortran_PS)
         call export_structured(this%amax_stress_yx,&
         dir//'amax_stress_yx'//fortran_PS)
         call export_structured(this%amax_stress_yy,&
         dir//'amax_stress_yy'//fortran_PS)
         call export_structured(this%amax_stress_yz,&
         dir//'amax_stress_yz'//fortran_PS)
         call export_structured(this%amax_stress_zx,&
         dir//'amax_stress_zx'//fortran_PS)
         call export_structured(this%amax_stress_zy,&
         dir//'amax_stress_zy'//fortran_PS)
         call export_structured(this%amax_stress_zz,&
         dir//'amax_stress_zz'//fortran_PS)
         call export_structured(this%amax_stress_walls_xx,&
         dir//'amax_stress_walls_xx'//fortran_PS)
         call export_structured(this%amax_stress_walls_xy,&
         dir//'amax_stress_walls_xy'//fortran_PS)
         call export_structured(this%amax_stress_walls_xz,&
         dir//'amax_stress_walls_xz'//fortran_PS)
         call export_structured(this%amax_stress_walls_yx,&
         dir//'amax_stress_walls_yx'//fortran_PS)
         call export_structured(this%amax_stress_walls_yy,&
         dir//'amax_stress_walls_yy'//fortran_PS)
         call export_structured(this%amax_stress_walls_yz,&
         dir//'amax_stress_walls_yz'//fortran_PS)
         call export_structured(this%amax_stress_walls_zx,&
         dir//'amax_stress_walls_zx'//fortran_PS)
         call export_structured(this%amax_stress_walls_zy,&
         dir//'amax_stress_walls_zy'//fortran_PS)
         call export_structured(this%amax_stress_walls_zz,&
         dir//'amax_stress_walls_zz'//fortran_PS)
       end subroutine

       subroutine export_structured_D_probe_set_ind(this,dir)
         implicit none
         type(probe_set_ind),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%probe_divB,&
         dir//'probe_divB'//fortran_PS)
         call export_structured(this%probe_divJ,&
         dir//'probe_divJ'//fortran_PS)
         call export_structured(this%JE,dir//'JE'//fortran_PS)
         call export_structured(this%JE_fluid,dir//'JE_fluid'//fortran_PS)
         s_ME = size(this%ME)
         write(un,*) s_ME
         do i_ME=1,s_ME
           call export_structured(this%ME(i_ME),&
           dir//'ME_'//int2str(i_ME)//fortran_PS)
         enddo
         s_ME_fluid = size(this%ME_fluid)
         write(un,*) s_ME_fluid
         do i_ME_fluid=1,s_ME_fluid
           call export_structured(this%ME_fluid(i_ME_fluid),&
           dir//'ME_fluid_'//int2str(i_ME_fluid)//fortran_PS)
         enddo
         s_ME_conductor = size(this%ME_conductor)
         write(un,*) s_ME_conductor
         do i_ME_conductor=1,s_ME_conductor
           call export_structured(this%ME_conductor(i_ME_conductor),&
           dir//'ME_conductor_'//int2str(i_ME_conductor)//fortran_PS)
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         write(un,*) s_probe_dB0dt
         do i_probe_dB0dt=1,s_probe_dB0dt
           call export_structured(this%probe_dB0dt(i_probe_dB0dt),&
           dir//'probe_dB0dt_'//int2str(i_probe_dB0dt)//fortran_PS)
         enddo
         s_Bx = size(this%Bx)
         write(un,*) s_Bx
         do i_Bx=1,s_Bx
           call export_structured(this%Bx(i_Bx),&
           dir//'Bx_'//int2str(i_Bx)//fortran_PS)
         enddo
         s_By = size(this%By)
         write(un,*) s_By
         do i_By=1,s_By
           call export_structured(this%By(i_By),&
           dir//'By_'//int2str(i_By)//fortran_PS)
         enddo
         s_Bz = size(this%Bz)
         write(un,*) s_Bz
         do i_Bz=1,s_Bz
           call export_structured(this%Bz(i_Bz),&
           dir//'Bz_'//int2str(i_Bz)//fortran_PS)
         enddo
         s_probe_B0 = size(this%probe_B0)
         write(un,*) s_probe_B0
         do i_probe_B0=1,s_probe_B0
           call export_structured(this%probe_B0(i_probe_B0),&
           dir//'probe_B0_'//int2str(i_probe_B0)//fortran_PS)
         enddo
         s_probe_B1 = size(this%probe_B1)
         write(un,*) s_probe_B1
         do i_probe_B1=1,s_probe_B1
           call export_structured(this%probe_B1(i_probe_B1),&
           dir//'probe_B1_'//int2str(i_probe_B1)//fortran_PS)
         enddo
         s_probe_Btot = size(this%probe_Btot)
         write(un,*) s_probe_Btot
         do i_probe_Btot=1,s_probe_Btot
           call export_structured(this%probe_Btot(i_probe_Btot),&
           dir//'probe_Btot_'//int2str(i_probe_Btot)//fortran_PS)
         enddo
         call export_structured(this%amax_JxB_x,&
         dir//'amax_JxB_x'//fortran_PS)
         call export_structured(this%amax_JxB_y,&
         dir//'amax_JxB_y'//fortran_PS)
         call export_structured(this%amax_JxB_z,&
         dir//'amax_JxB_z'//fortran_PS)
         call export_structured(this%amax_JxB,dir//'amax_JxB'//fortran_PS)
         call export_structured(this%amax_stress_xx,&
         dir//'amax_stress_xx'//fortran_PS)
         call export_structured(this%amax_stress_xy,&
         dir//'amax_stress_xy'//fortran_PS)
         call export_structured(this%amax_stress_xz,&
         dir//'amax_stress_xz'//fortran_PS)
         call export_structured(this%amax_stress_yx,&
         dir//'amax_stress_yx'//fortran_PS)
         call export_structured(this%amax_stress_yy,&
         dir//'amax_stress_yy'//fortran_PS)
         call export_structured(this%amax_stress_yz,&
         dir//'amax_stress_yz'//fortran_PS)
         call export_structured(this%amax_stress_zx,&
         dir//'amax_stress_zx'//fortran_PS)
         call export_structured(this%amax_stress_zy,&
         dir//'amax_stress_zy'//fortran_PS)
         call export_structured(this%amax_stress_zz,&
         dir//'amax_stress_zz'//fortran_PS)
         call export_structured(this%amax_stress_walls_xx,&
         dir//'amax_stress_walls_xx'//fortran_PS)
         call export_structured(this%amax_stress_walls_xy,&
         dir//'amax_stress_walls_xy'//fortran_PS)
         call export_structured(this%amax_stress_walls_xz,&
         dir//'amax_stress_walls_xz'//fortran_PS)
         call export_structured(this%amax_stress_walls_yx,&
         dir//'amax_stress_walls_yx'//fortran_PS)
         call export_structured(this%amax_stress_walls_yy,&
         dir//'amax_stress_walls_yy'//fortran_PS)
         call export_structured(this%amax_stress_walls_yz,&
         dir//'amax_stress_walls_yz'//fortran_PS)
         call export_structured(this%amax_stress_walls_zx,&
         dir//'amax_stress_walls_zx'//fortran_PS)
         call export_structured(this%amax_stress_walls_zy,&
         dir//'amax_stress_walls_zy'//fortran_PS)
         call export_structured(this%amax_stress_walls_zz,&
         dir//'amax_stress_walls_zz'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_probe_set_ind(this,dir)
         implicit none
         type(probe_set_ind),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_ME
         integer :: i_ME_fluid
         integer :: i_ME_conductor
         integer :: i_probe_dB0dt
         integer :: i_Bx
         integer :: i_By
         integer :: i_Bz
         integer :: i_probe_B0
         integer :: i_probe_B1
         integer :: i_probe_Btot
         integer :: s_ME
         integer :: s_ME_fluid
         integer :: s_ME_conductor
         integer :: s_probe_dB0dt
         integer :: s_Bx
         integer :: s_By
         integer :: s_Bz
         integer :: s_probe_B0
         integer :: s_probe_B1
         integer :: s_probe_Btot
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%probe_divB,&
         dir//'probe_divB'//fortran_PS)
         call import_structured(this%probe_divJ,&
         dir//'probe_divJ'//fortran_PS)
         call import_structured(this%JE,dir//'JE'//fortran_PS)
         call import_structured(this%JE_fluid,dir//'JE_fluid'//fortran_PS)
         s_ME = size(this%ME)
         do i_ME=1,s_ME
           call import_structured(this%ME(i_ME),&
           dir//'ME_'//int2str(i_ME)//fortran_PS)
         enddo
         s_ME_fluid = size(this%ME_fluid)
         do i_ME_fluid=1,s_ME_fluid
           call import_structured(this%ME_fluid(i_ME_fluid),&
           dir//'ME_fluid_'//int2str(i_ME_fluid)//fortran_PS)
         enddo
         s_ME_conductor = size(this%ME_conductor)
         do i_ME_conductor=1,s_ME_conductor
           call import_structured(this%ME_conductor(i_ME_conductor),&
           dir//'ME_conductor_'//int2str(i_ME_conductor)//fortran_PS)
         enddo
         s_probe_dB0dt = size(this%probe_dB0dt)
         do i_probe_dB0dt=1,s_probe_dB0dt
           call import_structured(this%probe_dB0dt(i_probe_dB0dt),&
           dir//'probe_dB0dt_'//int2str(i_probe_dB0dt)//fortran_PS)
         enddo
         s_Bx = size(this%Bx)
         do i_Bx=1,s_Bx
           call import_structured(this%Bx(i_Bx),&
           dir//'Bx_'//int2str(i_Bx)//fortran_PS)
         enddo
         s_By = size(this%By)
         do i_By=1,s_By
           call import_structured(this%By(i_By),&
           dir//'By_'//int2str(i_By)//fortran_PS)
         enddo
         s_Bz = size(this%Bz)
         do i_Bz=1,s_Bz
           call import_structured(this%Bz(i_Bz),&
           dir//'Bz_'//int2str(i_Bz)//fortran_PS)
         enddo
         s_probe_B0 = size(this%probe_B0)
         do i_probe_B0=1,s_probe_B0
           call import_structured(this%probe_B0(i_probe_B0),&
           dir//'probe_B0_'//int2str(i_probe_B0)//fortran_PS)
         enddo
         s_probe_B1 = size(this%probe_B1)
         do i_probe_B1=1,s_probe_B1
           call import_structured(this%probe_B1(i_probe_B1),&
           dir//'probe_B1_'//int2str(i_probe_B1)//fortran_PS)
         enddo
         s_probe_Btot = size(this%probe_Btot)
         do i_probe_Btot=1,s_probe_Btot
           call import_structured(this%probe_Btot(i_probe_Btot),&
           dir//'probe_Btot_'//int2str(i_probe_Btot)//fortran_PS)
         enddo
         call import_structured(this%amax_JxB_x,&
         dir//'amax_JxB_x'//fortran_PS)
         call import_structured(this%amax_JxB_y,&
         dir//'amax_JxB_y'//fortran_PS)
         call import_structured(this%amax_JxB_z,&
         dir//'amax_JxB_z'//fortran_PS)
         call import_structured(this%amax_JxB,dir//'amax_JxB'//fortran_PS)
         call import_structured(this%amax_stress_xx,&
         dir//'amax_stress_xx'//fortran_PS)
         call import_structured(this%amax_stress_xy,&
         dir//'amax_stress_xy'//fortran_PS)
         call import_structured(this%amax_stress_xz,&
         dir//'amax_stress_xz'//fortran_PS)
         call import_structured(this%amax_stress_yx,&
         dir//'amax_stress_yx'//fortran_PS)
         call import_structured(this%amax_stress_yy,&
         dir//'amax_stress_yy'//fortran_PS)
         call import_structured(this%amax_stress_yz,&
         dir//'amax_stress_yz'//fortran_PS)
         call import_structured(this%amax_stress_zx,&
         dir//'amax_stress_zx'//fortran_PS)
         call import_structured(this%amax_stress_zy,&
         dir//'amax_stress_zy'//fortran_PS)
         call import_structured(this%amax_stress_zz,&
         dir//'amax_stress_zz'//fortran_PS)
         call import_structured(this%amax_stress_walls_xx,&
         dir//'amax_stress_walls_xx'//fortran_PS)
         call import_structured(this%amax_stress_walls_xy,&
         dir//'amax_stress_walls_xy'//fortran_PS)
         call import_structured(this%amax_stress_walls_xz,&
         dir//'amax_stress_walls_xz'//fortran_PS)
         call import_structured(this%amax_stress_walls_yx,&
         dir//'amax_stress_walls_yx'//fortran_PS)
         call import_structured(this%amax_stress_walls_yy,&
         dir//'amax_stress_walls_yy'//fortran_PS)
         call import_structured(this%amax_stress_walls_yz,&
         dir//'amax_stress_walls_yz'//fortran_PS)
         call import_structured(this%amax_stress_walls_zx,&
         dir//'amax_stress_walls_zx'//fortran_PS)
         call import_structured(this%amax_stress_walls_zy,&
         dir//'amax_stress_walls_zy'//fortran_PS)
         call import_structured(this%amax_stress_walls_zz,&
         dir//'amax_stress_walls_zz'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_probe_set_ind(this)
         implicit none
         type(probe_set_ind),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module