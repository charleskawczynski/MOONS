       module face_domain_mod
       use current_precision_mod
       use face_edge_corner_indexing_mod
       use IO_tools_mod
       use grid_mod
       use data_location_mod
       use block_mod
       use domain_mod
       use mesh_mod
       implicit none

       private
       public :: face_domain
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_props

       interface init;        module procedure init_face_domain;           end interface
       interface init;        module procedure init_face_domain_copy;      end interface
       interface delete;      module procedure delete_face_domain;         end interface
       interface display;     module procedure display_face_domain;        end interface
       interface print;       module procedure print_face_domain;          end interface
       interface export;      module procedure export_face_domain;         end interface
       interface import;      module procedure import_face_domain;         end interface
       interface export;      module procedure export_face_domain_wrapper; end interface
       interface import;      module procedure import_face_domain_wrapper; end interface

       interface init_props;  module procedure init_props_FD;              end interface

       type face_domain
         type(domain) :: g,b,i,i_opp ! ghost, boundary,interior, opposite interior
         real(cp),dimension(6) :: dh,nhat
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_face_domain(FD,B,face)
         implicit none
         type(face_domain),intent(inout) :: FD
         type(block),intent(in) :: B
         integer,intent(in) :: face
         call add(FD%g,B%fg(face),B%g,face,1)
         call add(FD%b,B%fb(face),B%g,face,1)
         call add(FD%i,B%fi(face),B%g,face,1)
         call add(FD%i_opp,B%fi(opp_face_given_face(face)),B%g,face,1)
         FD%nhat = nhat_given_face(face)
         FD%dh(face) = B%fg(face)%c(dir_given_face(face))%dhn(1)
       end subroutine

       subroutine init_face_domain_copy(FD_out,FD_in)
         implicit none
         type(face_domain),intent(inout) :: FD_out
         type(face_domain),intent(in) :: FD_in
         call delete(FD_out)
         call init(FD_out%g,FD_in%g)
         call init(FD_out%b,FD_in%b)
         call init(FD_out%i,FD_in%i)
         call init(FD_out%i_opp,FD_in%i_opp)
         FD_out%dh = FD_in%dh
         FD_out%nhat = FD_in%nhat
       end subroutine

       subroutine delete_face_domain(FD)
         implicit none
         type(face_domain),intent(inout) :: FD
         call delete(FD%g)
         call delete(FD%b)
         call delete(FD%i)
         call delete(FD%i_opp)
         FD%dh = 0.0_cp
         FD%nhat = 0.0_cp
       end subroutine

       subroutine print_face_domain(FD,name)
         implicit none
         type(face_domain),intent(in) :: FD
         character(len=*),intent(in) :: name
         call print(FD%g,name)
         call print(FD%b,name)
         call print(FD%i,name)
         call print(FD%i_opp,name)
       end subroutine

       subroutine display_face_domain(FD,un)
         implicit none
         type(face_domain),intent(inout) :: FD
         integer,intent(in) :: un
         call display(FD%g,un)
         call display(FD%b,un)
         call display(FD%i,un)
         call display(FD%i_opp,un)
         write(un,*) 'dh = ',FD%dh
         write(un,*) 'nhat = ',FD%nhat
       end subroutine

       subroutine export_face_domain(FD,un)
         implicit none
         type(face_domain),intent(in) :: FD
         integer,intent(in) :: un
         call export(FD%g,un)
         call export(FD%b,un)
         call export(FD%i,un)
         call export(FD%i_opp,un)
         write(un,*) 'FD%dh = ';    write(un,*) FD%dh
         write(un,*) 'FD%nhat = ';  write(un,*) FD%nhat
       end subroutine

       subroutine export_face_domain_wrapper(FD,dir,name)
         implicit none
         type(face_domain),intent(in) :: FD
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(FD,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_face_domain(FD,un)
         implicit none
         type(face_domain),intent(inout) :: FD
         integer,intent(in) :: un
         call import(FD%g,un)
         call import(FD%b,un)
         call import(FD%i,un)
         call import(FD%i_opp,un)
         read(un,*); read(un,*) FD%dh
         read(un,*); read(un,*) FD%nhat
       end subroutine

       subroutine import_face_domain_wrapper(FD,dir,name)
         implicit none
         type(face_domain),intent(inout) :: FD
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(FD,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! *****************************************************************

       subroutine init_props_FD(FD,DL)
         implicit none
         type(face_domain),intent(inout) :: FD
         type(data_location),intent(in) :: DL
         call init_props(FD%g,DL)
         call init_props(FD%b,DL)
         call init_props(FD%i,DL)
         call init_props(FD%i_opp,DL)
       end subroutine

       end module