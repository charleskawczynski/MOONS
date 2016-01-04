       module mesh_generate_mod
       use mesh_mod
       use geometries_mod
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

         call cube(m_mom)
         call extend_cube(m_ind,m_mom)
         call init(D_sigma,m_mom,m_ind)

         ! call flow_past_square(m_mom)
         ! call flow_past_square(m_ind)
         ! call init(D_sigma,m_mom,m_ind)

         ! call BC_sim_mom(m_mom)
         ! call BC_sim_ind(m_ind,m_mom,D_sigma)
       end subroutine


       end module