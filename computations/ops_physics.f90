       module ops_physics_mod
       ! 
       ! Directions are frequently used in this code. 
       ! For clarity, some diagrams here show how the 
       ! directions are defined.
       ! 
       ! faceDir = 1 (x)
       ! 
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !      faceDir --->  \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       ! 
       ! 
       ! 
       ! edgeDir = 1 (x)
       ! 
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !                    \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       !                        -------> edgeDir
       ! 
       ! 
       use del_mod
       use grid_mod
       use BCs_mod
       use VF_mod
       use SF_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_aux_mod

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

       public :: orthogonalDirection

       public :: faceAdvectNew
       public :: faceAdvectDonor
       ! public :: fluidDiffusion

       ! public :: CCBfieldDiffuse
       ! interface CCBfieldDiffuse;    module procedure CCBfieldDiffuseSF;   end interface
       ! interface CCBfieldDiffuse;    module procedure CCBfieldDiffuseVF;   end interface

       contains

       ! ******************************* AUXILIARY ROUTINES *********************************

       function orthogonalDirection(dir1,dir2) result(orthDir)
         implicit none
         integer,intent(in) :: dir1,dir2
         integer :: orthDir
         select case (dir1)
           case (1);
             select case (dir2)
             case (2); orthDir = 3
             case (3); orthDir = 2
             case default
               stop 'Error: bad input to orthogonalDirection'
             end select
           case (2);
             select case (dir2)
             case (1); orthDir = 3
             case (3); orthDir = 1
             case default
               stop 'Error: bad input to orthogonalDirection'
             end select
           case (3);
             select case (dir2)
             case (1); orthDir = 2
             case (2); orthDir = 1
             case default
               stop 'Error: bad input to orthogonalDirection'
             end select
           case default
             stop 'Error: bad input to orthogonalDirection'
         end select
       end function

       ! ******************************* FACE BASED DERIVATIVES *********************************

       subroutine faceAdvectNew(div,U,Ui,g)
         ! Computes
         ! 
         !               d
         !  div_i = u_j --- (u_i)
         !              dx_j
         ! 
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         ! 
         implicit none
         type(VF),intent(inout) :: div
         type(VF),intent(in) :: U,ui
         type(grid),intent(in) :: g
         type(VF) :: U_ave,V_ave,W_ave,temp
         type(del) ::d
         integer :: pad
         pad = 1 ! Currently running for pad = 1, try pad = 0 next
         call init(U_ave,U)
         call init(V_ave,U)
         call init(W_ave,U)
         call init(temp,div)

         call assign(div,0.0_cp)
         call face2Face(U_ave,V_ave,W_ave,U,g,temp%x)

         call d%assign(temp%x,Ui%x,g,1,1,pad) ! u_ave * d/dx (u_i)
         call d%assign(temp%y,Ui%y,g,1,2,pad)
         call d%assign(temp%z,Ui%z,g,1,3,pad)
         call multiply(temp,U_ave)
         call add(div,temp)

         call d%assign(temp%x,Ui%x,g,1,1,pad) ! v_ave * d/dy (u_i)
         call d%assign(temp%y,Ui%y,g,1,2,pad)
         call d%assign(temp%z,Ui%z,g,1,3,pad)
         call multiply(temp,V_ave)
         call add(div,temp)

         call d%assign(temp%x,Ui%x,g,1,1,pad) ! w_ave * d/dz (u_i)
         call d%assign(temp%y,Ui%y,g,1,2,pad)
         call d%assign(temp%z,Ui%z,g,1,3,pad)
         call multiply(temp,W_ave)
         call add(div,temp)

         call delete(temp)
         call delete(U_ave)
         call delete(V_ave)
         call delete(W_ave)
       end subroutine

       subroutine faceAdvectDonor(div,U,Ui,temp_E1,temp_E2,temp_CC,g)
         ! Computes
         ! 
         !           d
         !  div_i = --- (u_j u_i)
         !          dx_j
         ! 
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE1 and temp_E2    --> cell edge.
         !           temp_CC               --> cell center.
         ! 
         implicit none
         type(VF),intent(inout) :: div
         type(VF),intent(in) :: U,ui
         type(VF),intent(inout) :: temp_E1,temp_E2
         type(VF),intent(inout) :: temp_CC
         type(grid),intent(in) :: g
         type(del) ::d
         integer :: pad
         pad = 1 ! Currently running for pad = 1, try pad = 0 next

         call zeroGhostPoints(div)
         
         ! d/dxj (uj ui) for i=j
         call face2CellCenter(temp_CC,U,g)
         call square(temp_CC)

         call d%assign(div%x,temp_CC%x,g,1,1,pad)
         call d%assign(div%y,temp_CC%y,g,1,2,pad)
         call d%assign(div%z,temp_CC%z,g,1,3,pad)

         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         ! x (y,z edges)
         call face2Edge(temp_E1%y,Ui%x,g,1,2)
         call face2Edge(temp_E2%y, U%z,g,3,2)
         call face2Edge(temp_E1%z,Ui%x,g,1,3)
         call face2Edge(temp_E2%z, U%y,g,2,3)
         call multiply(temp_E1%y,temp_E2%y)
         call multiply(temp_E1%z,temp_E2%z)
         call d%add(div%x,temp_E1%y,g,1,3,pad)
         call d%add(div%x,temp_E1%z,g,1,2,pad)

         ! y (x,z edges)
         call face2Edge(temp_E1%z,Ui%y,g,2,3)
         call face2Edge(temp_E2%z, U%x,g,1,3)
         call face2Edge(temp_E1%x,Ui%y,g,2,1)
         call face2Edge(temp_E2%x, U%z,g,3,1)
         call multiply(temp_E1%x,temp_E2%x)
         call multiply(temp_E1%z,temp_E2%z)
         call d%add(div%y,temp_E1%x,g,1,3,pad)
         call d%add(div%y,temp_E1%z,g,1,1,pad)

         ! z (x,y edges)
         call face2Edge(temp_E1%y,Ui%z,g,3,2)
         call face2Edge(temp_E2%y, U%x,g,1,2)
         call face2Edge(temp_E1%x,Ui%z,g,3,1)
         call face2Edge(temp_E2%x, U%y,g,2,1)
         call multiply(temp_E1%x,temp_E2%x)
         call multiply(temp_E1%y,temp_E2%y)
         call d%add(div%z,temp_E1%x,g,1,2,pad)
         call d%add(div%z,temp_E1%y,g,1,1,pad)
       end subroutine


       end module