     module momentum_terms_mod
     use IO_tools_mod
     use equation_term_mod
     implicit none

     private
     public :: momentum_terms
     public :: init,delete,display,print,export,import

     interface delete;  module procedure delete_MT;         end interface
     interface init;    module procedure init_MT_copy;      end interface
     interface display; module procedure display_MT;        end interface
     interface print;   module procedure print_MT;          end interface
     interface export;  module procedure export_MT;         end interface
     interface import;  module procedure import_MT;         end interface
     interface export;  module procedure export_MT_wrapper; end interface

     type momentum_terms
       type(equation_term) :: pressure_grad
       type(equation_term) :: advection_divergence
       type(equation_term) :: advection_convection
       type(equation_term) :: advection_base_flow
       type(equation_term) :: diffusion
       type(equation_term) :: diffusion_linear
       type(equation_term) :: mean_pressure_grad
       type(equation_term) :: JCrossB
       type(equation_term) :: Q2D_JCrossB
       type(equation_term) :: Buoyancy
       type(equation_term) :: Gravity
     end type

     contains

     subroutine init_MT_copy(MT,MT_in)
       implicit none
       type(momentum_terms),intent(inout) :: MT
       type(momentum_terms),intent(in) :: MT_in
       call init(MT%pressure_grad,MT_in%pressure_grad)
       call init(MT%advection_divergence,MT_in%advection_divergence)
       call init(MT%advection_convection,MT_in%advection_convection)
       call init(MT%advection_base_flow,MT_in%advection_base_flow)
       call init(MT%diffusion,MT_in%diffusion)
       call init(MT%diffusion_linear,MT_in%diffusion_linear)
       call init(MT%mean_pressure_grad,MT_in%mean_pressure_grad)
       call init(MT%JCrossB,MT_in%JCrossB)
       call init(MT%Q2D_JCrossB,MT_in%Q2D_JCrossB)
       call init(MT%Buoyancy,MT_in%Buoyancy)
       call init(MT%Gravity,MT_in%Gravity)
      end subroutine

     subroutine delete_MT(MT)
       implicit none
       type(momentum_terms),intent(inout) :: MT
       call delete(MT%pressure_grad)
       call delete(MT%advection_divergence)
       call delete(MT%advection_convection)
       call delete(MT%advection_base_flow)
       call delete(MT%diffusion)
       call delete(MT%diffusion_linear)
       call delete(MT%mean_pressure_grad)
       call delete(MT%JCrossB)
       call delete(MT%Q2D_JCrossB)
       call delete(MT%Buoyancy)
       call delete(MT%Gravity)
      end subroutine

     subroutine display_MT(MT,un)
       implicit none
       type(momentum_terms),intent(in) :: MT
       integer,intent(in) :: un
       call display(MT%pressure_grad,un,'pressure_grad')
       call display(MT%advection_divergence,un,'advection_divergence')
       call display(MT%advection_convection,un,'advection_convection')
       call display(MT%advection_base_flow ,un,'advection_base_flow')
       call display(MT%diffusion           ,un,'diffusion')
       call display(MT%diffusion_linear    ,un,'diffusion_linear')
       call display(MT%mean_pressure_grad  ,un,'mean_pressure_grad')
       call display(MT%JCrossB             ,un,'JCrossB')
       call display(MT%Q2D_JCrossB         ,un,'Q2D_JCrossB')
       call display(MT%Buoyancy            ,un,'Buoyancy')
       call display(MT%Gravity             ,un,'Gravity')
      end subroutine

     subroutine print_MT(MT)
       implicit none
       type(momentum_terms),intent(in) :: MT
       call display(MT,6)
      end subroutine

     subroutine export_MT(MT,un)
       implicit none
       type(momentum_terms),intent(in) :: MT
       integer,intent(in) :: un
       write(un,*) ' --------- momentum_terms --------- '
       call export(MT%pressure_grad       ,un,'pressure_grad')
       call export(MT%advection_divergence,un,'advection_divergence')
       call export(MT%advection_convection,un,'advection_convection')
       call export(MT%advection_base_flow ,un,'advection_base_flow')
       call export(MT%diffusion           ,un,'diffusion')
       call export(MT%diffusion_linear    ,un,'diffusion_linear')
       call export(MT%mean_pressure_grad  ,un,'mean_pressure_grad')
       call export(MT%JCrossB             ,un,'JCrossB')
       call export(MT%Q2D_JCrossB         ,un,'Q2D_JCrossB')
       call export(MT%Buoyancy            ,un,'Buoyancy')
       call export(MT%Gravity             ,un,'Gravity')
       write(un,*) ' ---------------------------------- '
      end subroutine

     subroutine import_MT(MT,un)
       implicit none
       type(momentum_terms),intent(inout) :: MT
       integer,intent(in) :: un
       read(un,*);
       call import(MT%pressure_grad       ,un)
       call import(MT%advection_divergence,un)
       call import(MT%advection_convection,un)
       call import(MT%advection_base_flow ,un)
       call import(MT%diffusion           ,un)
       call import(MT%diffusion_linear    ,un)
       call import(MT%mean_pressure_grad  ,un)
       call import(MT%JCrossB             ,un)
       call import(MT%Q2D_JCrossB         ,un)
       call import(MT%Buoyancy            ,un)
       call import(MT%Gravity             ,un)
       read(un,*);
      end subroutine

     subroutine export_MT_wrapper(MT,dir,name)
       implicit none
       type(momentum_terms),intent(in) :: MT
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(MT,un)
       call close_and_message(un,dir,name)
      end subroutine

     end module