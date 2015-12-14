      module matrix_free_params_mod
      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: matrix_free_params
      public :: init,delete

      interface init;     module procedure init_MFP;      end interface
      interface delete;   module procedure delete_MFP;    end interface

      type matrix_free_params
        logical :: suppress_warning = .true.
        real(cp) :: c_eng
        real(cp) :: c_ind
        real(cp) :: c_mom
      end type

      contains

      subroutine init_MFP(m_out,m_in)
        implicit none
        type(matrix_free_params),intent(inout) :: m_out
        type(matrix_free_params),intent(in) :: m_in
        m_out%suppress_warning = m_in%suppress_warning
        m_out%c_eng = m_in%c_eng
        m_out%c_ind = m_in%c_ind
        m_out%c_mom = m_in%c_mom
      end subroutine

      subroutine delete_MFP(m)
        implicit none
        type(matrix_free_params),intent(inout) :: m
        m%suppress_warning = .true.
        m%c_eng = 0.0_cp
        m%c_ind = 0.0_cp
        m%c_mom = 0.0_cp
      end subroutine

      end module