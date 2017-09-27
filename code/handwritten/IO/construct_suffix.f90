      module construct_suffix_mod
      use string_mod
      use data_location_extend_mod
      use face_edge_corner_indexing_mod
      use datatype_conversion_mod
      implicit none

      private
      public :: construct_suffix
      interface construct_suffix; module procedure construct_suffix_SF; end interface
      interface construct_suffix; module procedure construct_suffix_VF; end interface

      contains

      subroutine construct_suffix_SF(s,name,DL)
        implicit none
        type(string),intent(inout) :: s
        type(data_location),intent(in) :: DL
        character(len=*),intent(in) :: name
        call init(s,name//get_char(DL))
            if (is_Face(DL)) then; call append(s,'_'//xyz_given_dir(get_Face(DL)))
        elseif (is_Edge(DL)) then; call append(s,'_'//xyz_given_dir(get_Edge(DL)))
        endif
      end subroutine

      subroutine construct_suffix_VF(s,name,DL)
        implicit none
        type(string),intent(inout) :: s
        type(data_location),intent(in),dimension(3) :: DL
        character(len=*),intent(in) :: name
#ifdef _DEBUG_IO_EXPORT_
        call insist_collocated(DL,'construct_name_VF')
#endif

        call init(s,name//get_char(DL(1)))
        if (is_Face(DL(1))) call append(s,'_'//xyz_given_dir(get_Face(DL(1))))
        if (is_Edge(DL(1))) call append(s,'_'//xyz_given_dir(get_Edge(DL(1))))
      end subroutine

      end module