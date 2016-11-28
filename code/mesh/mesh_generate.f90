       module mesh_generate_mod
       use current_precision_mod
       use mesh_mod
       use mesh_simple_geometries_mod
       use mesh_BC_geometries_mod
       use mesh_BC_geometries_symmetric_mod
       use mesh_complex_geometries_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use mesh_domain_mod
       implicit none

       private
       public :: mesh_generate

       contains

       subroutine mesh_generate(m_mom,m_ind,MD_sigma,DT,Ha,tw,include_vacuum)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_domain),intent(inout) :: MD_sigma
         real(cp),intent(in) :: Ha,tw
         logical,intent(in) :: include_vacuum
         type(dir_tree),intent(in) :: DT
         ! 3D cube uniform no walls
         ! call cube_uniform(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! 3D cube uniform with walls
         call cube_uniform(m_mom)
         call extend_cube_uniform(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)

         ! For analyzing mesh and stencils
         ! call matrix_export_mesh(m_mom)
         ! call extend_cube_uniform(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! 3D cube non-uniform no walls
         ! call cube(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! 3D cube non-uniform, uniform walls
         ! call cube(m_mom)
         ! call extend_cube_uniform(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! 2D square non-uniform no walls
         ! call square(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! 3D cube non-uniform with walls
         ! call cube(m_mom)
         ! call extend_cube(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! 2D straight duct
         ! call straight_duct_fluid(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! call flow_past_square(m_mom)
         ! call flow_past_square(m_ind)
         ! call init(MD_sigma,m_mom,m_ind)

         ! call BC_sim_mom_symmetric(m_mom,Ha)
         ! call BC_sim_ind_symmetric(m_ind,m_mom,MD_sigma,Ha,tw,include_vacuum)

         ! call BC_sim_mom_symmetric(m_mom,Ha)
         ! call BC_sim_ind_symmetric(m_ind,m_mom,MD_sigma,Ha,tw,include_vacuum)

         ! call BC_sim_mom_proper_insulate(m_mom,Ha,DT)
         ! call BC_sim_ind_proper_insulate(m_ind,m_mom,MD_sigma,DT,Ha,tw,include_vacuum)

         ! call straight_duct_fluid(m_mom)
         ! call duct_with_vacuum(m_ind,m_mom,MD_sigma)


         ! call ins_sep_channel_Tyler(m_mom)
         ! call ins_sep_channel_Tyler(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! call LDC_2D_2domains_vertical_z(m_mom)
         ! call LDC_2D_2domains_horizontal_z(m_mom)
         ! call LDC_2D_4domains(m_mom)
         ! call LDC_2D_9domains_uniform(m_mom)
         ! call flow_past_square(m_mom)
         ! call duct_2D_2domains(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(MD_sigma,m_mom,m_ind)

         ! call straight_duct_fluid(m_mom)
         ! call Hunt_duct_magnetic(m_ind,m_mom,MD_sigma)
         ! call Shercliff_duct_magnetic(m_ind,m_mom,MD_sigma)
       end subroutine


       end module