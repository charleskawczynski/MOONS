      module VF_mod
      use mesh_mod
      use SF_mod
      use VF_obj_mod
      use VF_assign_mod
      use VF_assign_negative_mod
      use VF_add_mod
      use VF_subtract_mod
      use VF_multiply_mod
      use VF_divide_mod
      use VF_aux_mod
      implicit none

      ! Initialization / Deletion (allocate/deallocate)
      public :: VF
      public :: init,delete

      ! Grid initialization
      public :: init_CC
      public :: init_Face
      public :: init_Edge
      public :: init_Node

      ! Monitoring
      public :: print
      public :: print_BCs
      public :: export_BCs

      public :: assign
      public :: assign_negative
      public :: add
      public :: subtract
      public :: multiply
      public :: divide
      public :: square
      public :: sum
      public :: dot_product
      public :: invert


      end module