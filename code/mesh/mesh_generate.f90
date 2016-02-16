       module mesh_generate_mod
       use mesh_mod
       use mesh_simple_geometries_mod
       use mesh_complex_geometries_mod
       use domain_mod
       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: mesh_generate

       contains

       subroutine mesh_generate(m_mom,m_ind,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(domain),intent(inout) :: D_sigma
         ! call cube_uniform(m_mom)
         ! call extend_cube_uniform(m_ind,m_mom)
         ! call init(D_sigma,m_mom,m_ind)

         ! call cube_uniform(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(D_sigma,m_mom,m_ind)

         ! call cube(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(D_sigma,m_mom,m_ind)

         ! call cube(m_mom)
         ! call extend_cube(m_ind,m_mom)
         ! call init(D_sigma,m_mom,m_ind)

         ! call flow_past_square(m_mom)
         ! call flow_past_square(m_ind)
         ! call init(D_sigma,m_mom,m_ind)

         ! call BC_sim_mom(m_mom)
         ! call BC_sim_ind(m_ind,m_mom,D_sigma)

         ! call ins_sep_channel_Tyler(m_mom)
         ! call init(m_ind,m_mom)
         ! call init(D_sigma,m_mom,m_ind)

         ! call cube(m_mom)
         ! call LDC_2D_2domains_vertical_z(m_mom)
         ! call LDC_2D_2domains_horizontal_z(m_mom)
         ! call LDC_2D_4domains(m_mom)
         ! call LDC_2D_9domains_uniform(m_mom)
         call flow_past_square(m_mom)
         call init(m_ind,m_mom)
         call init(D_sigma,m_mom,m_ind)

         ! call straight_duct_fluid(m_mom)
         ! call Hunt_duct_magnetic(m_ind,m_mom,D_sigma)
         ! call Shercliff_duct_magnetic(m_ind,m_mom,D_sigma)
       end subroutine


       end module