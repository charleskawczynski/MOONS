       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module data_location_mod
       use string_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use dir_manip_mod
       implicit none

       private
       public :: data_location
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_data_location;              end interface
       interface delete;                 module procedure delete_data_location;                 end interface
       interface display;                module procedure display_data_location;                end interface
       interface display_short;          module procedure display_short_data_location;          end interface
       interface display;                module procedure display_wrap_data_location;           end interface
       interface print;                  module procedure print_data_location;                  end interface
       interface print_short;            module procedure print_short_data_location;            end interface
       interface export;                 module procedure export_data_location;                 end interface
       interface export_primitives;      module procedure export_primitives_data_location;      end interface
       interface import;                 module procedure import_data_location;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_data_location;end interface
       interface export_structured;      module procedure export_structured_D_data_location;    end interface
       interface import_structured;      module procedure import_structured_D_data_location;    end interface
       interface import_primitives;      module procedure import_primitives_data_location;      end interface
       interface export;                 module procedure export_wrap_data_location;            end interface
       interface import;                 module procedure import_wrap_data_location;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_data_location;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_data_location;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_data_location;      end interface

       type data_location
         logical :: C = .false.
         logical :: N = .false.
         logical :: E = .false.
         logical :: F = .false.
         logical :: defined = .false.
         integer :: face = 0
         integer :: edge = 0
         integer :: volume_ID = 0
         logical,dimension(3) :: CC_along = .false.
         logical,dimension(3) :: N_along = .false.
         integer,dimension(3) :: CC_eye = 0
         integer,dimension(3) :: N_eye = 0
       end type

       contains

       subroutine init_copy_data_location(this,that)
         implicit none
         type(data_location),intent(inout) :: this
         type(data_location),intent(in) :: that
         call delete(this)
         this%C = that%C
         this%N = that%N
         this%E = that%E
         this%F = that%F
         this%defined = that%defined
         this%face = that%face
         this%edge = that%edge
         this%volume_ID = that%volume_ID
         this%CC_along = that%CC_along
         this%N_along = that%N_along
         this%CC_eye = that%CC_eye
         this%N_eye = that%N_eye
       end subroutine

       subroutine delete_data_location(this)
         implicit none
         type(data_location),intent(inout) :: this
         this%C = .false.
         this%N = .false.
         this%E = .false.
         this%F = .false.
         this%defined = .false.
         this%face = 0
         this%edge = 0
         this%volume_ID = 0
         this%CC_along = .false.
         this%N_along = .false.
         this%CC_eye = 0
         this%N_eye = 0
       end subroutine

       subroutine display_data_location(this,un)
         implicit none
         type(data_location),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'C         = ',this%C
         write(un,*) 'N         = ',this%N
         write(un,*) 'E         = ',this%E
         write(un,*) 'F         = ',this%F
         write(un,*) 'defined   = ',this%defined
         write(un,*) 'face      = ',this%face
         write(un,*) 'edge      = ',this%edge
         write(un,*) 'volume_ID = ',this%volume_ID
         write(un,*) 'CC_along  = ',this%CC_along
         write(un,*) 'N_along   = ',this%N_along
         write(un,*) 'CC_eye    = ',this%CC_eye
         write(un,*) 'N_eye     = ',this%N_eye
       end subroutine

       subroutine display_short_data_location(this,un)
         implicit none
         type(data_location),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'C         = ',this%C
         write(un,*) 'N         = ',this%N
         write(un,*) 'E         = ',this%E
         write(un,*) 'F         = ',this%F
         write(un,*) 'defined   = ',this%defined
         write(un,*) 'face      = ',this%face
         write(un,*) 'edge      = ',this%edge
         write(un,*) 'volume_ID = ',this%volume_ID
         write(un,*) 'CC_along  = ',this%CC_along
         write(un,*) 'N_along   = ',this%N_along
         write(un,*) 'CC_eye    = ',this%CC_eye
         write(un,*) 'N_eye     = ',this%N_eye
       end subroutine

       subroutine display_wrap_data_location(this,dir,name)
         implicit none
         type(data_location),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_data_location(this)
         implicit none
         type(data_location),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_data_location(this)
         implicit none
         type(data_location),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_data_location(this,un)
         implicit none
         type(data_location),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_data_location(this,un)
         implicit none
         type(data_location),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_data_location(this,un)
         implicit none
         type(data_location),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'C          = ';write(un,*) this%C
         write(un,*) 'N          = ';write(un,*) this%N
         write(un,*) 'E          = ';write(un,*) this%E
         write(un,*) 'F          = ';write(un,*) this%F
         write(un,*) 'defined    = ';write(un,*) this%defined
         write(un,*) 'face       = ';write(un,*) this%face
         write(un,*) 'edge       = ';write(un,*) this%edge
         write(un,*) 'volume_ID  = ';write(un,*) this%volume_ID
         write(un,*) 'CC_along   = ';write(un,*) this%CC_along
         write(un,*) 'N_along    = ';write(un,*) this%N_along
         write(un,*) 'CC_eye     = ';write(un,*) this%CC_eye
         write(un,*) 'N_eye      = ';write(un,*) this%N_eye
       end subroutine

       subroutine import_primitives_data_location(this,un)
         implicit none
         type(data_location),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%C
         read(un,*); read(un,*) this%N
         read(un,*); read(un,*) this%E
         read(un,*); read(un,*) this%F
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%face
         read(un,*); read(un,*) this%edge
         read(un,*); read(un,*) this%volume_ID
         read(un,*); read(un,*) this%CC_along
         read(un,*); read(un,*) this%N_along
         read(un,*); read(un,*) this%CC_eye
         read(un,*); read(un,*) this%N_eye
       end subroutine

       subroutine export_wrap_data_location(this,dir,name)
         implicit none
         type(data_location),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_data_location(this,dir,name)
         implicit none
         type(data_location),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_data_location(this,dir)
         implicit none
         type(data_location),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_data_location(this,dir)
         implicit none
         type(data_location),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_folder_structure_data_location(this,dir)
         implicit none
         type(data_location),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
       end subroutine

       subroutine export_structured_D_data_location(this,dir)
         implicit none
         type(data_location),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_data_location(this,dir)
         implicit none
         type(data_location),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_data_location(this)
         implicit none
         type(data_location),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module