      module GF_mod
        use current_precision_mod
        use grid_mod
        use GF_base_mod
        use GF_assign_mod
        ! use GF_assign_surface_mod
        use GF_add_mod
        use GF_subtract_mod
        use GF_multiply_mod
        use GF_divide_mod
        use GF_aux_mod
        use GF_distributions_mod
        use GF_assign_plane_mod
        use GF_plane_sum_mod

        implicit none
        private

        ! GF_base_mod
        public :: grid_field
        public :: init,delete,display,print,export,import ! Essentials
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        ! GF_distributions_mod
        public :: volume
        public :: sine_waves
        public :: cosine_waves
        public :: random_noise

        ! GF_surface_flux_mod
        public :: plane_sum_x
        public :: plane_sum_y
        public :: plane_sum_z

        ! GF_assign_plane
        public :: assign_plane_x
        public :: assign_plane_y
        public :: assign_plane_z

        ! GF_assign_mod
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: add_product,swap

        ! GF_aux_mod
        public :: square,min,max,minabs,maxabs
        public :: maxabsdiff,mean,sum,size

        public :: zero_ghost_xmin_xmax
        public :: zero_ghost_ymin_ymax
        public :: zero_ghost_zmin_zmax

      end module