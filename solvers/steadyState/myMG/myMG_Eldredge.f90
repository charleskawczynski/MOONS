      subroutine myMG3D(u,f,gd,rd)
        griddata.M = griddata.M + 1;
        griddata.N = griddata.N + 1;

        griddata.bctype = 1;
        params.griddata = griddata;
        params.rundata = rundata;
        ! Multigrid
        ! Set the MG parameters

        ! Number of multigrid levels
        mg.maxlev = floor(log2(griddata.M));

        ! Smoothing sweeps at each level
        mg.niteri = 1; ! Initial on fine grid
        mg.niterf = 1; ! Final on fine grid
        mg.niter1 = 1; ! After restriction
        mg.niter2 = 1; ! After prolongation

        ! Relaxation solution parameters at coarsest level
        mg.gsmaxiter = 10000;
        mg.gstol = 1e-14;

        ! Multigrid cycling parameters
        ! It should not take more than ~15 MG iterations to complete.
        ! It helps to play with the tolerance a bit.
        mg.maxcycle = 20;  ! Max number of cycles to perform
        mg.tol      = 1e-7; ! Residual norm tolerance

        ! Pack it up
        rundata.mg = mg;
        params.rundata = rundata;

        ! Initial guess is zero
        ! u = zeros(size(f));

        ! Solve it
        [u,errarray,res] = mg3d(u,f,params);


        skipline = sprintf('\n');
        disp(skipline)
        disp('Multigrid results');
        disp('--------------------');
        disp(['Number of grids = ' num2str(mg.maxlev)])
        disp(['Number of iterations = ' num2str(length(errarray))])
        disp(['Final residual Linf norm = ' num2str(errarray(end))])
        disp(['Contraction of residual error/iteration = ' ...
            num2str((errarray(end)/errarray(1))^(1/(length(errarray)-1)))])

        griddata.M = griddata.M -1;
        griddata.N = griddata.N -1;

      end subroutine

      subroutine mg3d(u,errarray,res, ui, f, varargin )
      !MG2D Perform multigrid solution of 2-d Poisson equation
      !   u = MG2D(ui,f,params) solves the 2d Poisson problem
      !    
      !            (d^2/dx^2 + d^2/dy^2) u = f
      !   
      !   for the scalar field u, given the initial guess ui, the
      !   force field f on a 2-d collocated grid (or, the nodes of a staggered
      !   grid). The Dirichlet data are assumed to be absorbed into f.
      !
      !   The grid information is provided in params.griddata. It
      !   is assumed that grid spacing is uniform and the same in both
      !   directions.
      !
      !   Multigrid parameters are in the structure params.rundata.mg
      !
      !   MAE 259A
      !   J. D. Eldredge
      !   2/24/2014
      ! 
      !   Converted to Fortran 90
      !   Charlie Kawczynski
      !   8/24/2014

      !! Unpack the arguments
      params = varargin{1};
      griddata = params.griddata;
      rundata = params.rundata;

      !! Get the grid parameters of finest grid
      dx = griddata.dx;


      !! Get the multigrid parameters
      ! Number of initial smoothing sweeps at finest level
      niteri = rundata.mg.niteri;
      ! Number of final smoothing sweeps at finest level
      niterf = rundata.mg.niterf;
      ! Number of coarseness levels to descend. Finest grid is level 1, and
      ! coarsest grid is level maxlev
      maxlev = rundata.mg.maxlev;
      ! Max number of cycles to perform
      maxcycle = rundata.mg.maxcycle;
      ! MG solution tolerance
      tol = rundata.mg.tol;
      ! GS tolerance
      gstol = rundata.mg.gstol;
      ! GS max iterations
      gsmaxiter = rundata.mg.gsmaxiter;

      ! Set initial guess for u
      u = ui;
      clear errarray
      do iter = 1:maxcycle
                
          !! Start the multigrid V-cycle
          if (maxlev > 1) then
              ! Perform smoothing sweeps on finest level.
              u = mgsmooth(u,f,dx,niteri);
              ! Get the residual on finest level (level 1)
              res = f - lap2d(u,griddata);
              ! Zero the boundary values
      !         res(1:griddata.M,[1 griddata.N]) = 0;
      !         res([1 griddata.M],1:griddata.N) = 0;
              res(1:end,[1 end]) = 0;
              res([1 end],1:end) = 0;
              
              ! Perform the cycle, starting at level 2
              ucc = mgcycle(2,res,params);
              ! Prolong correction to grid level 1
              u = u + mgprolong(ucc);
              ! Final smoothing sweeps
              u = mgsmooth(u,f,dx,niterf);
              
          else
              ! Only one level. Just use relaxation to convergence.
              do riter = 1:gsmaxiter
                  unp1 = mgsmooth(u,f,griddata.dx);
                  temp = norm(u-unp1,inf);
                  u = unp1;
                  
                  if (temp < gstol),
                      break
                  end
              enddo
              
          endif
          
          ! Final residual
          res = f - lap2d(u,griddata);
          ! Zero the boundary values
          res(1:griddata.M+1,[1 griddata.N+1]) = 0;
          res([1 griddata.M+1],1:griddata.N+1) = 0;
          ! Norm of residual
          rnorm = norm(res,inf);
         
          ! Store the residal error for diagnostics
          errarray(iter) = rnorm;
          
          if (rnorm < tol) then
              stop
          endif
      enddo

      end subroutine


      subroutine mgcycle(uc,lev,resf,varargin)
      !MGCYCLE Perform one descent of a multigrid cycle 
      !   uc = MGCYCLE(res,params) takes the residual resf from a finer grid at level
      !        lev-1 and returns the smoothed correction uc at level lev as well
      !        as the norm of the residual at that level. It may perform further
      !        descent into coarser grids, depending on the number of levels
      !        specified in params.rundata.mg.maxlev
      !
      !   MAE 259A
      !   J. D. Eldredge
      !   2/24/2014
      ! 
      !   Converted to Fortran 90
      !   Charlie Kawczynski
      !   8/24/2014

      !! Unpack the arguments
      params = varargin{1}
      griddata = params.griddata
      rundata = params.rundata

      !! Unpack the multigrid parameters
      ! Number of smoothing sweeps after each restriction
      niter1 = rundata.mg.niter1
      ! Number of smoothing sweeps after each prolongation
      niter2 = rundata.mg.niter2
      ! Number of coarseness levels to descend. Finest grid is level 1, and
      ! coarsest grid is level maxlev
      maxlev = rundata.mg.maxlev
      ! GS tolerance
      gstol = rundata.mg.gstol
      ! GS max iterations
      gsmaxiter = rundata.mg.gsmaxiter

      !! Get the grid parameters at level lev and re-set the griddata structure
      dx = 2*griddata.dx
      dxsq = dx*dx
      griddata.dx = dx
      griddata.dy = dx
      griddata.M = griddata.M/2
      griddata.N = griddata.N/2
      params.griddata = griddata

      !! Restrict residual onto grid at level lev and use this as RHS data
      f = mgrestrict(resf)


      !! Perform further descent, if we are not at the bottom yet
      if (lev < maxlev) then
          ! Perform smoothing at this level. Start with zero correction, since we
          ! are only computing what will be added to the next-finer grids
          ! correction after prolongation
          uc = zeros(size(f))
          uc = mgsmooth(uc,f,dx,niter1)
          
          ! Compute the residual
          res = f - lap2d(uc,griddata)
          
          ! Zero the boundary values
      !     res(1:griddata.M,[1 griddata.N]) = 0;
      !     res([1 griddata.M],1:griddata.N) = 0;
          res(1:end,[1 end]) = 0
          res([1 end],1:end) = 0
          
          ! Cycle through next coarser level
          ucc = mgcycle(lev+1,res,params)
          ! ucc contains correction at coarser level lev+1
          ! Prolong this correction to this level (lev)
          uc = uc + mgprolong(ucc)
          
          ! Further smoothing
          uc = mgsmooth(uc,f,dx,niter2)
            
      else
          ! At coarsest level. Solve exactly.
          !uc = poisson2d(f,griddata);     

          uc = zeros(size(f))
          do iter = 1,gsmaxiter
              ucnp1 = mgsmooth(uc,f,griddata.dx)
              temp = norm(uc-ucnp1,inf)
              uc = ucnp1
        
              if (temp < gstol) then
                  break
              end
          enddo
          
      endif

      end subroutine

      subroutine mgrestrict( ru, u )
      !MGRESTRICT Restriction operator for 2-d multigrid
      !   ru = MGRESTRICT(u) restrict the data u on a fine grid of uniform spacing
      !      onto a coarser grid of uniform spacing with twice the value.
      !
      !   It is assumed that the Dirichlet boundary data lie in u.
      !
      !   The routine uses the 9-point restriction stencil that is the adjoint of
      !   the 9-point prolongation defined in MGPROLONG
      !
      !   MAE 259A
      !   J. D. Eldredge
      !   2/24/2014
      ! 
      !   Converted to Fortran 90
      !   Charlie Kawczynski
      !   8/24/2014

      !! Set up the size of the restricted data
      M = size(u,1)-1;
      N = size(u,2)-1;
      ru = zeros(M/2+1,N/2+1);
      ! Note that ru is fixed at zero on the boundary nodes

      !! Interior coarse nodes
      do jj = 2,N/2
          ru(2:M/2,jj) = 0.0625*u(2:2:M-2,2*jj-2)+&
                         0.125*u(3:2:M-1,2*jj-2)+&
                         0.0625*u(4:2:M,2*jj-2) +&
                         0.125*u(2:2:M-2,2*jj-1)+&
                         0.25*u(3:2:M-1,2*jj-1)+&
                         0.125*u(4:2:M,2*jj-1) +&
                         0.0625*u(2:2:M-2,2*jj)+&
                         0.125*u(3:2:M-1,2*jj)+&
                         0.0625*u(4:2:M,2*jj)
      end do

      end subroutine

      subroutine mgprolong( pu, u )
      !MGPROLONG Prolongation operator for 2-d multigrid
      !   pu = MGPROLONG(u) prolongs the data u on a coarse grid of uniform spacing
      !      onto a finer grid of uniform spacing with half the value.
      !
      !   It is assumed that the Dirichlet boundary data lie in u.
      !
      !   The routine uses the standard 9-point prolongation operator
      !
      !   MAE 259A
      !   J. D. Eldredge
      !   2/24/2014
      ! 
      !   Converted to Fortran 90
      !   Charlie Kawczynski
      !   8/24/2014

      !! Set up the size of the prolonged data
      M = size(u,1)-1
      N = size(u,2)-1
      pu = zeros(2*M+1,2*N+1)

      !! Take care of coarse nodes that coincide with fine nodes. This also takes
      ! care of boundary data
      do jj = 1,N+1
          pu(1:2:2*M+1,2*jj-1) = u(1:M+1,jj)
      enddo

      !! Now the midpoints in x direction
      ! and x-average contributions to the interstitial points
      do jj = 1,N
          pu(2:2:2*M,2*jj-1) = 0.5* (u(1:M,jj) + u(2:M+1,jj))
          pu(2:2:2*M,2*jj  ) = 0.25*(u(1:M,jj) + u(2:M+1,jj) + u(1:M,jj+1) + u(2:M+1,jj+1))
      enddo
      pu(2:2:2*M,2*N+1) = 0.5* (u(1:M,N+1) + u(2:M+1,N+1))



      !! Now midpoints in y direction
      u = transpose(u)
      pu = transpose(pu)
      do ii = 1,M
          pu(2:2:2*N,2*ii-1) = 0.5*(u(1:N,ii) + u(2:N+1,ii))
      enddo
      pu(2:2:2*N,2*M+1) = 0.5*(u(1:N,M+1) + u(2:N+1,M+1))
      u = transpose(u)
      pu = transpose(pu)

      end subroutine

      subroutine mgsmooth(v, u, f, dx, varargin )
      !MGSMOOTH Perform smoothing of data in a multigrid algorithm
      !   v = MGSMOOTH(u,f,dx) performs one smoothing iteration of data u on a
      !     grid with uniform spacing dx and right-hand side f, using five-point
      !     red-black Gauss-Seidel relaxation.
      !          u(i,j) = ( u(i-1,j)+u(i+1,j)+u(i,j-1)+u(i,j+1) )/4
      !                       - dx^2*f(i,j)/4
      !
      !     It is assumed that the array u holds prescribed boundary values in
      !     the boundary elements. Also, f must be the same size as u.
      !
      !   v = MGSMOOTH(u,f,dx,niter) performs 'niter' iterations of the smoothing
      !     operation.
      !
      !   MAE 259A
      !   J. D. Eldredge
      !   2/24/2014

      !! Set the number of iterations
      niter = 1;
      if (nargin > 3),
          niter = varargin{1}
      end

      !! Put RHS data in temporary array. Get grid sizes
      ! Store boundary data
      v = u
      M = size(u,1)-1
      N = size(u,2)-1
      fact = -0.25*dx*dx
      !f = -0.25*dx*dx*f;

      !! This form is a bit more readable
      !! Start the smoothing sweeps
      ! for isweep = 1:niter
      !     ! Loop through even and odd checkerboard squares
      !     for color = 0:1
      !         stag = color;
      !         for jj = 2:N ! Loop through interior rows
      !             v(2+stag:2:end-1-stag,jj) = fact*f(2+stag:2:end-1-stag,jj) + 0.25*( ...
      !                 v(1+stag:2:end-2-stag,jj) + v(3+stag:2:end-stag,jj));
      !             stag = 1-stag; ! Switch between even-odd nodes from row to row   
      !         end
      !         ! Now transpose in order to add in the (i,j-1) and (i,j+1) elements
      !         v = v';
      !         stag = color;
      !         for ii = 2:M ! Loop through interior columns
      !             v(2+stag:2:end-1-stag,ii) = v(2+stag:2:end-1-stag,ii) + 0.25*( ...
      !                 v(1+stag:2:end-2-stag,ii) + v(3+stag:2:end-stag,ii));
      !             stag = 1-stag; ! Switch between even-odd nodes from column to column   
      !         end
      !         ! Transpose back
      !         v = v';
      !         
      !     end
      ! end

      !! This form is more efficient
      [I,J] = meshgrid(1:M+1,1:N+1)
      red = mod(I+J,2)
      black = ones(size(red))-red
      fred = f.*red
      do isweep = 1,niter
          ! Get only the red squares on the checkerboard
          vred = v.*red
          
          ! The Laplacian of vred only gathers information from the arms of the
          ! stencil at each black point, since the vred = 0 at the stencil
          ! center.
          vblack = fact*f + 0.25*lap2d(vred)*black
          
          ! Zero the boundary entries in vblack, so they dont contribute to vred
          vblack(1:M+1,[1 N+1]) = 0;
          vblack([1 M+1],1:N+1) = 0;
          
          ! Need to zero out the entries at the red squares in vblack.
          vblack = vblack*black
          
          ! Now perform the smoothing from the new black square data at the red
          ! squares. Also, add the black squares in.
          v = fact*fred + vblack + 0.25*lap2d(vblack)*red
          
          ! Finally, zero out the boundary elements
          v(1:M+1,[1 N+1]) = 0
          v([1 M+1],1:N+1) = 0
      enddo

      end subroutine
