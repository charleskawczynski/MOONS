     subroutine define_mesh_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       ! call init(MP,MQP)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform',45,-1.0_cp,1.0_cp))
       call add_base(SP%MP_mom,seg_1d(3,'grid_uniform',45,-1.0_cp,1.0_cp))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform',45,-1.0_cp,1.0_cp))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
       call add_ext(SP%MP_ind,seg_1d(1,'ext_uniform_IO',11))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_uniform_IO',11))
       call add_ext(SP%MP_ind,seg_1d(3,'ext_uniform_IO',11))
     end subroutine
