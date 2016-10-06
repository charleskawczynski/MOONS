      module GF_mod
        use current_precision_mod
        use grid_mod
        use GF_base_mod
        use GF_assign_mod
        use GF_aux_mod
        use GF_add_mod
        use GF_subtract_mod
        use GF_multiply_mod
        use GF_divide_mod

        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: grid_field
        public :: init,delete,display,print,export,import ! Essentials

        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        ! BC initialization
        ! public :: init_BCs

        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: add_product,swap
        ! Auxiliary
        public :: square,min,max,minabs,maxabs
        public :: maxabsdiff,mean,sum,size

        public :: zero_ghost_xmin_xmax
        public :: zero_ghost_ymin_ymax
        public :: zero_ghost_zmin_zmax

      end module