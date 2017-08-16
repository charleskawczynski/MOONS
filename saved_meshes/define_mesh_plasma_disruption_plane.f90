     subroutine define_mesh_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       ! call init(MP,MQP)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B' ,50 ,-1.0_cp,1.0_cp))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_B' ,50 ,-1.0_cp,1.0_cp))
       call add_base(SP%MP_mom,seg_1d(3,'grid_uniform'   ,1,-0.5_cp,0.5_cp))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
     end subroutine