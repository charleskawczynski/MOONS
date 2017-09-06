       module export_mesh_aux_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_import_mod
       use IO_export_mod

       use mesh_extend_mod
       use mesh_domain_mod
       use mesh_block_extend_mod
       use SF_extend_mod
       use VF_mod
       use string_mod
       use path_extend_mod
       use dir_tree_mod
       use mirror_props_mod

       use sim_params_mod
       use export_raw_processed_symmetry_mod
       use export_raw_processed_mod
       use ops_mirror_field_mod

       implicit none

       private
       public :: export_mesh_aux

       contains

       subroutine export_mesh_aux(SP,DT,m_mom,m_ind)
         implicit none
         type(mesh),intent(in) :: m_mom,m_ind
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         type(mesh_block) :: MB
         type(mesh) :: m_temp
         type(SF) :: vol_CC

         if (SP%EL%export_cell_volume) then
           if (SP%VS%U%SS%initialize) then
             call init(m_temp,m_mom)
             call init_CC(vol_CC,m_temp); call volume(vol_CC,m_temp)
             call export_raw(m_temp,vol_CC,str(DT%meshes),'cell_volume_mom',0)
             call delete(vol_CC); call delete(m_temp)
           endif
           if (SP%VS%B%SS%initialize) then
             call init(m_temp,m_ind)
             call init_CC(vol_CC,m_temp); call volume(vol_CC,m_temp)
             call export_raw(m_temp,vol_CC,str(DT%meshes),'cell_volume_ind',0)
             call delete(vol_CC); call delete(m_temp)
           endif
         endif
         if (SP%EL%export_mesh_block) then
           if (SP%VS%U%SS%initialize) then
             call init(MB,m_mom%B(1))
             call export_mesh(MB%m,str(DT%meshes),'mesh_block_mom',0)
             call delete(MB)
           endif
           if (SP%VS%B%SS%initialize) then
             call init(MB,m_ind%B(1))
             call export_mesh(MB%m,str(DT%meshes),'mesh_block_ind',0)
             call delete(MB)
           endif
         endif
       end subroutine

       end module
