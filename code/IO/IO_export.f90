      module IO_export_mod
      use current_precision_mod
      use mesh_mod
      use string_mod
      use data_location_mod
      use SF_mod
      use VF_mod
      use base_export_mod
      use IO_tools_mod
      use time_marching_params_mod
      use face_edge_corner_indexing_mod
      use datatype_conversion_mod
      implicit none

      private

      interface export_3D_3C;   module procedure export_3D_3C_steady;     end interface
      interface export_3D_3C;   module procedure export_3D_3C_unsteady;   end interface
      public :: export_3D_3C ! call export_3D_3C(m,U,dir,name,pad,TMP)
                             ! call export_3D_3C(m,U,dir,name,pad)

      interface export_2D_3C;   module procedure export_2D_3C_steady;     end interface
      interface export_2D_3C;   module procedure export_2D_3C_unsteady;   end interface
      public :: export_2D_3C ! call export_2D_3C(m,U,dir,name,pad,direction,plane,TMP)
                             ! call export_2D_3C(m,U,dir,name,pad,direction,plane)

      interface export_1D_3C;   module procedure export_1D_3C_steady;     end interface
      interface export_1D_3C;   module procedure export_1D_3C_unsteady;   end interface
      public :: export_1D_3C ! call export_1D_3C(m,U,dir,name,pad,direction,line,TMP)
                             ! call export_1D_3C(m,U,dir,name,pad,direction,line)


      interface export_3D_2C;   module procedure export_3D_2C_steady;     end interface
      interface export_3D_2C;   module procedure export_3D_2C_unsteady;   end interface
      public :: export_3D_2C ! call export_3D_2C(m,U,dir,name,pad,TMP,direction)
                             ! call export_3D_2C(m,U,dir,name,pad,direction)

      interface export_2D_2C;   module procedure export_2D_2C_steady;     end interface
      interface export_2D_2C;   module procedure export_2D_2C_unsteady;   end interface
      public :: export_2D_2C ! call export_2D_2C(m,U,dir,name,pad,TMP,direction,plane)
                             ! call export_2D_2C(m,U,dir,name,pad,direction,plane)

      interface export_1D_2C;   module procedure export_1D_2C_steady;     end interface
      interface export_1D_2C;   module procedure export_1D_2C_unsteady;   end interface
      public :: export_1D_2C ! call export_1D_2C(m,U,dir,name,pad,TMP,direction,line)
                             ! call export_1D_2C(m,U,dir,name,pad,direction,line)


      interface export_3D_1C;   module procedure export_3D_1C_steady;     end interface
      interface export_3D_1C;   module procedure export_3D_1C_unsteady;   end interface
      public :: export_3D_1C ! call export_3D_1C(m,U,dir,name,pad,TMP)
                             ! call export_3D_1C(m,U,dir,name,pad)

      interface export_2D_1C;   module procedure export_2D_1C_steady;     end interface
      interface export_2D_1C;   module procedure export_2D_1C_unsteady;   end interface
      public :: export_2D_1C ! call export_2D_1C(m,U,dir,name,pad,TMP,direction,plane)
                             ! call export_2D_1C(m,U,dir,name,pad,direction,plane)

      interface export_1D_1C;   module procedure export_1D_1C_steady;     end interface
      interface export_1D_1C;   module procedure export_1D_1C_unsteady;   end interface
      public :: export_1D_1C ! call export_1D_1C(m,U,dir,name,pad,TMP,direction,line)
                             ! call export_1D_1C(m,U,dir,name,pad,direction,line)

      public :: export_mesh

      interface construct_name; module procedure construct_name_SF;       end interface
      interface construct_name; module procedure construct_name_VF;       end interface

      contains

      ! **********************************************************************
      ! **************************** 3 COMPONENTS ****************************
      ! **********************************************************************

      subroutine export_3D_3C_steady(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s))
        call exp_3D_3C(m,pad,un,str(s),U)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_3D_3C_unsteady(m,U,dir,name,pad,TMP)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(VF),intent(in) :: U
        type(time_marching_params),intent(in) :: TMP
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_3D_3C(m,pad,un,str(s),U)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      subroutine export_2D_3C_steady(m,U,dir,name,pad,direction,plane)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,plane
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s))
        call exp_2D_3C(m,pad,un,str(s),U,direction,plane)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_2D_3C_unsteady(m,U,dir,name,pad,TMP,direction,plane)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,plane
        type(time_marching_params),intent(in) :: TMP
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_2D_3C(m,pad,un,str(s),U,direction,plane)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      subroutine export_1D_3C_steady(m,U,dir,name,pad,direction,line)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(VF),intent(in) :: U
        integer,dimension(2),intent(in) :: line
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s))
        call exp_1D_3C(m,pad,un,str(s),U,direction,line)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_1D_3C_unsteady(m,U,dir,name,pad,TMP,direction,line)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(time_marching_params),intent(in) :: TMP
        type(VF),intent(in) :: U
        integer,dimension(2),intent(in) :: line
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_1D_3C(m,pad,un,str(s),U,direction,line)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      ! **********************************************************************
      ! **************************** 2 COMPONENTS ****************************
      ! **********************************************************************

      subroutine export_3D_2C_steady(m,U,dir,name,pad,direction)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s))
        call exp_3D_2C(m,pad,un,str(s),U,direction)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_3D_2C_unsteady(m,U,dir,name,pad,TMP,direction)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(time_marching_params),intent(in) :: TMP
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_3D_2C(m,pad,un,str(s),U,direction)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      subroutine export_2D_2C_steady(m,U,dir,name,pad,direction,plane)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,plane
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s))
        call exp_2D_2C(m,pad,un,str(s),U,direction,plane)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_2D_2C_unsteady(m,U,dir,name,pad,TMP,direction,plane)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,plane
        type(time_marching_params),intent(in) :: TMP
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_2D_2C(m,pad,un,str(s),U,direction,plane)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      subroutine export_1D_2C_steady(m,U,dir,name,pad,direction,line)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,dimension(2),intent(in) :: line
        integer,intent(in) :: pad,direction
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s))
        call exp_1D_2C(m,pad,un,str(s),U,direction,line)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_1D_2C_unsteady(m,U,dir,name,pad,TMP,direction,line)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        integer,dimension(2),intent(in) :: line
        type(time_marching_params),intent(in) :: TMP
        type(VF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,get_DL(U))
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_1D_2C(m,pad,un,str(s),U,direction,line)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      ! **********************************************************************
      ! **************************** 1 COMPONENTS ****************************
      ! **********************************************************************

      subroutine export_3D_1C_steady(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(SF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,U%DL)
        un = new_and_open(dir,str(s))
        call exp_3D_1C(m,pad,un,str(s),U)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_3D_1C_unsteady(m,U,dir,name,pad,TMP)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(SF),intent(in) :: U
        type(time_marching_params),intent(in) :: TMP
        type(string) :: s
        integer :: un
        call construct_name(s,name,U%DL)
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_3D_1C(m,pad,un,str(s),U)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      subroutine export_2D_1C_steady(m,U,dir,name,pad,direction,plane)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,plane
        type(SF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,U%DL)
        un = new_and_open(dir,str(s))
        call exp_2D_1C(m,pad,un,str(s),U,direction,plane)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_2D_1C_unsteady(m,U,dir,name,pad,TMP,direction,plane)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,plane
        type(time_marching_params),intent(in) :: TMP
        type(SF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,U%DL)
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_2D_1C(m,pad,un,str(s),U,direction,plane)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      subroutine export_1D_1C_steady(m,U,dir,name,pad,direction,line)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        integer,dimension(2),intent(in) :: line
        type(SF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,U%DL)
        un = new_and_open(dir,str(s))
        call exp_1D_1C(m,pad,un,str(s),U,direction,line)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine
      subroutine export_1D_1C_unsteady(m,U,dir,name,pad,TMP,direction,line)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        integer,dimension(2),intent(in) :: line
        type(time_marching_params),intent(in) :: TMP
        type(SF),intent(in) :: U
        type(string) :: s
        integer :: un
        call construct_name(s,name,U%DL)
        un = new_and_open(dir,str(s)//'_t='//int2str(TMP%n_step))
        call exp_1D_1C(m,pad,un,str(s),U,direction,line)
        call close_and_message(un,dir,str(s)//'_t='//int2str(TMP%n_step))
        call delete(s)
      end subroutine

      ! **********************************************************************
      ! **************************** 0 COMPONENTS ****************************
      ! **********************************************************************

      subroutine export_mesh(m,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        integer :: un
        un = new_and_open(dir,name)
        call exp_mesh_SF(m,pad,un,name)
        call close_and_message(un,dir,name)
      end subroutine

      ! **********************************************************************
      ! ******************************* NAMING *******************************
      ! **********************************************************************

      subroutine construct_name_SF(s,name,DL)
        implicit none
        type(string),intent(inout) :: s
        type(data_location),intent(in) :: DL
        character(len=*),intent(in) :: name
        call init(s,name//get_char(DL))
            if (is_Face(DL)) then; call append(s,'_'//xyz_given_dir(DL%face))
        elseif (is_Edge(DL)) then; call append(s,'_'//xyz_given_dir(DL%edge))
        endif
      end subroutine

      subroutine construct_name_VF(s,name,DL)
        implicit none
        type(string),intent(inout) :: s
        type(data_location),intent(in),dimension(3) :: DL
        character(len=*),intent(in) :: name
#ifdef _DEBUG_IO_EXPORT_
        call insist_collocated(DL,'construct_name_VF')
#endif

        call init(s,name//get_char(DL(1)))
        if (is_Face(DL(1))) call append(s,'_'//xyz_given_dir(DL(1)%face))
        if (is_Edge(DL(1))) call append(s,'_'//xyz_given_dir(DL(1)%edge))
      end subroutine

      end module