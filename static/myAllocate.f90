       module myAllocate_mod
       use simParams_mod
       use constants_mod
       use griddata_mod
       implicit none

       interface myAllocate
         module procedure myAllocate1
         module procedure myAllocate2
         module procedure myAllocate3
       end interface

       contains

       subroutine myAllocate1(Nx,Ny,Nz,gd,gridType,dir)
         ! This routine returns the number of cells 
         ! (or nodes/faces) in the x,y and z directions 
         ! on grid gridType.
         implicit none
         integer,intent(inout) :: Nx,Ny,Nz
         type(griddata),intent(in) :: gd
         integer,intent(in) :: gridType
         integer,intent(in),optional :: dir
         integer,dimension(3) :: N,Ni
         integer :: x,y,z

         call getN(gd,N); call getNi(gd,Ni)
         if (present(dir)) then
           select case (dir)
           case (1); x=1;y=0;z=0
           case (2); x=0;y=1;z=0
           case (3); x=0;y=0;z=1
           case default
             write(*,*) 'Error: dir must =1,2,3 in myAllocate'; stop
           end select
         else; x=0;y=0;z=0 ! To suppress unused
         endif
         select case (gridType)
         ! **************** INTERIOR FACE DATA ********************
         case (1) ! Includes fictitious cells
         Nx = Ni(1)+2-x; Ny = Ni(2)+2-y; Nz = Ni(3)+2-z
         ! ************ INTERIOR CELL CENTERED DATA ***************
         case (2) ! Includes fictitious cells
         Nx = Ni(1)+2; Ny = Ni(2)+2; Nz = Ni(3)+2
         ! ***************** INTERIOR NODE DATA *******************
         case (3)
         Nx = Ni(1)+1; Ny = Ni(2)+1; Nz = Ni(3)+1
         ! **************** TOTAL DOMAIN FACE DATA ****************
         case (4) ! Includes fictitious cells
         Nx = N(1)+2-x; Ny = N(2)+2-y; Nz = N(3)+2-z
         ! ********* TOTAL DOMAIN CELL CENTERED DATA **************
         case (5) ! Includes fictitious cells
         Nx = N(1)+2; Ny = N(2)+2; Nz = N(3)+2
         ! ************** TOTAL DOMAIN NODE DATA ******************
         case (6)
         Nx = N(1)+1; Ny = N(2)+1; Nz = N(3)+1
         ! ************** TOTAL DOMAIN EDGE DATA ******************
         case (7)
         Nx = N(1)+1+x; Ny = N(2)+1+y; Nz = N(3)+1+z
         ! ************** INTERIOR DOMAIN EDGE DATA ******************
         case (8)
         Nx = Ni(1)+1+x; Ny = Ni(2)+1+y; Nz = Ni(3)+1+z
         end select
       end subroutine

       subroutine myAllocate2(Nx,Ny,Nz,gd,f)
         implicit none
         integer,intent(inout) :: Nx,Ny,Nz
         real(dpn),dimension(:,:,:),intent(in) :: f
         type(griddata),intent(in) :: gd
         integer,dimension(3) :: s
         integer,dimension(3) :: N,Ni
         s = shape(f)
         call getNi(gd,Ni)
         call getN(gd,N)

         if     (s(1).eq.Ni(1)+1) then ! Interior nodes
          Nx = s(1)-1
         elseif (s(1).eq.N(1) +1) then  ! Total domain nodes
          Nx = s(1)-1
         elseif (s(1).eq.Ni(1)+2) then    ! Interior cell centers
          Nx = s(1)-2
         elseif (s(1).eq.N(1) +2) then     ! Total domain cell centers
          Nx = s(1)-2
         else
          write(*,*) 'No correct sizes were selected in myAllocate2x.';stop
         endif

         if     (s(2).eq.Ni(2)+1) then ! Interior nodes
          Ny = s(2)-1
         elseif (s(2).eq.N(2) +1) then  ! Total domain nodes
          Ny = s(2)-1
         elseif (s(2).eq.Ni(2)+2) then    ! Interior cell centers
          Ny = s(2)-2
         elseif (s(2).eq.N(2) +2) then     ! Total domain cell centers
          Ny = s(2)-2
         else
          write(*,*) 'No correct sizes were selected in myAllocate2y.';stop
         endif

         if     (s(3).eq.Ni(3)+1) then ! Interior nodes
          Nz = s(3)-1
         elseif (s(3).eq.N(3) +1) then  ! Total domain nodes
          Nz = s(3)-1
         elseif (s(3).eq.Ni(3)+2) then    ! Interior cell centers
          Nz = s(3)-2
         elseif (s(3).eq.N(3) +2) then     ! Total domain cell centers
          Nz = s(3)-2
         else
          write(*,*) 'No correct sizes were selected in myAllocate2z.';stop
         endif
       end subroutine

       subroutine myAllocate3(Nx,Ny,Nz,gd,s)
         implicit none
         integer,intent(inout) :: Nx,Ny,Nz
         integer,dimension(3),intent(in) :: s
         type(griddata),intent(in) :: gd
         integer,dimension(3) :: N,Ni
         call getNi(gd,Ni)
         call getN(gd,N)

         if     (s(1).eq.Ni(1)+1) then ! Interior nodes
          Nx = s(1)-1
         elseif (s(1).eq.N(1) +1) then  ! Total domain nodes
          Nx = s(1)-1
         elseif (s(1).eq.Ni(1)+2) then    ! Interior cell centers
          Nx = s(1)-2
         elseif (s(1).eq.N(1) +2) then     ! Total domain cell centers
          Nx = s(1)-2
         else
          write(*,*) 'No correct sizes were selected in myAllocate2x.';stop
         endif

         if     (s(2).eq.Ni(2)+1) then ! Interior nodes
          Ny = s(2)-1
         elseif (s(2).eq.N(2) +1) then  ! Total domain nodes
          Ny = s(2)-1
         elseif (s(2).eq.Ni(2)+2) then    ! Interior cell centers
          Ny = s(2)-2
         elseif (s(2).eq.N(2) +2) then     ! Total domain cell centers
          Ny = s(2)-2
         else
          write(*,*) 'No correct sizes were selected in myAllocate2y.';stop
         endif

         if     (s(3).eq.Ni(3)+1) then ! Interior nodes
          Nz = s(3)-1
         elseif (s(3).eq.N(3) +1) then  ! Total domain nodes
          Nz = s(3)-1
         elseif (s(3).eq.Ni(3)+2) then    ! Interior cell centers
          Nz = s(3)-2
         elseif (s(3).eq.N(3) +2) then     ! Total domain cell centers
          Nz = s(3)-2
         else
          write(*,*) 'No correct sizes were selected in myAllocate2z.';stop
         endif
       end subroutine

       end module