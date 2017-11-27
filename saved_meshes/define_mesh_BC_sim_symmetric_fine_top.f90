     subroutine define_mesh_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       ! call init(MP,N_base,N_ext,MQP)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)

       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'        ,80,-1.0_cp,1.0_cp,2.0_cp))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'        ,40,-1.0_cp,0.0_cp,2.0_cp))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_R'        ,45, 0.0_cp,1.0_cp,2.0_cp))
       call add_ext(SP%MP_mom,seg_1d(2,'ext_prep_Roberts_C2F_IO',40, 0.0_cp +1.0_cp,2.0_cp))

       call init(SP%MP_sigma,SP%MP_mom)

       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO'      ,9,0.5_cp,,2.0_cp))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_Roberts_B_IO'      ,9,0.5_cp,,2.0_cp))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_prep_Roberts_B_IO' ,9,0.5_cp,,2.0_cp))

       call init(SP%MP_ind,SP%MP_sigma)

       call add_ext(SP%MP_ind,seg_1d(1,'ext_Roberts_near_IO'      ,9 ,7.0_cp - 1.0_cp - 0.5_cp,2.0_cp))
       call add_ext(SP%MP_ind,seg_1d(3,'ext_Roberts_near_IO'      ,9 ,7.0_cp - 1.0_cp - 0.5_cp,2.0_cp))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_prep_Roberts_R_IO'    ,9 ,7.0_cp - 1.0_cp - 0.5_cp,2.0_cp))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_app_Roberts_L_IO'     ,18,7.0_cp - 1.0_cp         ,2.0_cp))

     end subroutine
