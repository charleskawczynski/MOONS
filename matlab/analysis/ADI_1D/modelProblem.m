classdef modelProblem
    properties
        u_solution,u_trisolve,f,err;
    end
    methods
        function m = modelProblem()
        end

        %% Dirichlet problem (identity on boundary)
        function a = dirichlet(a,x,u_solution,f,alpha,n,Nsweeps,nlevels,dt,multiScale)
            %% GRID GENERATION
            h = x(2) - x(1);

            %% PREP FORMATTED OUTPUT
            st = [];
            for i=1:nlevels; if (i~=nlevels);st = [st '%d\t\t,'];else;st = [st '%d']; end; end

            %% MULTI-SCALE TIME STEP SELECTION
            if multiScale; disp(['Number of sweeps = ' num2str(Nsweeps)]); end
            if ~multiScale; disp(['Number of iterations = ' num2str(Nsweeps)]); end
            if multiScale; disp(['Number of multi-scale time levels = ' num2str(nlevels)]);end

            dtj = zeros(1,nlevels);
            for j=1:nlevels
                hj = 2^(j-1)*h; dtj(j) = 4*(hj^2)/(alpha*pi^2);
            end
            if multiScale
                disp(sprintf(['dt(j)            = ' st],dtj))
            end


            %% MULTI-SCALE RELAXATION WITH THOMAS ALGORITHM (ELDREDGE)
            un = zeros(size(f));
            un(1) = u_solution(1); un(end) = u_solution(end);

            if multiScale
                for k=1:Nsweeps
                    for j=1:nlevels
                        unp1 = a.relax1D(un,f,alpha,dtj(j),n,h);
                        diffNorm = max(abs(un - unp1));
                        un = unp1;
                    end
                    for j=nlevels:-1:1
                        unp1 = a.relax1D(un,f,alpha,dtj(j),n,h);
                        diffNorm = max(abs(un - unp1));
                        un = unp1;
                    end
                end
            else
                for k=1:Nsweeps
                    unp1 = a.relax1D(un,f,alpha,dt,n,h);
                    diffNorm = max(abs(un - unp1));
                    un = unp1;
                end
            end
            u_trisolve = un;

            %% ERROR
            err_trisolve = max(abs(u_trisolve - u_solution));

            disp(['Difference between iterates = ' num2str(diffNorm)])
            disp(['Error w.r.t. exact discrete solution = ' num2str(err_trisolve)])

            %% VISUALIZATION
            figure
            subplot(2,1,1)
            plot(x,u_trisolve,'b-',x,u_solution,'r-')
            title('Solution to \nabla^2u = f')
            xlabel('x')
            ylabel('u(x)')
            legend('u_{trisolve}','u_{solution}')

            subplot(2,1,2)
            plot(x,f,'k-')
            title('Forcing term in \nabla^2u = f')
            xlabel('x')
            ylabel('f(x)')
            legend('f')

            a.u_solution = u_solution;
            a.u_trisolve = u_trisolve;
        end
        function [a b c] = setUpSystem(obj,alpha,h,n)
            a = [alpha*ones(1,n-1)/h^2 0];
            b = [1 -2*alpha*ones(1,n-1)/h^2 1];
            c = [0 alpha*ones(1,n-1)/h^2];
        end
        function u = relax1D(a,un,f,alpha,dt,n,h)
            %% Solves the equation
            % (I - .5 dt alpha B) u = (I + .5 dt alpha B) un - dt*f
            % for u. Where B = tridiag(1,-2,1)

            % Construct RHS
            [loDiag, Diag, upDiag] = setUpSystem(a,.5*dt*alpha,h,n);
            Diag(2:end-1) = 1 + Diag(2:end-1);
            A = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);
            dbstop at 98 in modelProblem
            rhs = A*un - dt*f;
            [loDiag, Diag, upDiag] = setUpSystem(a,-.5*dt*alpha,h,n);
            Diag(2:end-1) = 1 + Diag(2:end-1);

            u = zeros(size(un));
            u(2:end-1) = trisolve(loDiag(2:end-1),Diag(2:end-1),upDiag(2:end-1),rhs(2:end-1),'reg');

        end


        %% Uses trisolve on entire system
        function a = neumannTotal(a,x,u_solution,f,alpha,n,Nsweeps,nlevels,dt,multiScale)
            h = x(2) - x(1);

            %% PREP FORMATTED OUTPUT
            st = [];
            for i=1:nlevels; if (i~=nlevels);st = [st '%d\t\t,'];else;st = [st '%d']; end; end

            %% MULTI-SCALE TIME STEP SELECTION
            if multiScale; disp(['Number of sweeps = ' num2str(Nsweeps)]); end
            if ~multiScale; disp(['Number of iterations = ' num2str(Nsweeps)]); end
            if multiScale; disp(['Number of multi-scale time levels = ' num2str(nlevels)]);end

            dtj = zeros(1,nlevels);
            for j=1:nlevels
                hj = 2^(j-1)*h; dtj(j) = 4*(hj^2)/(alpha*pi^2);
            end
            if multiScale
                disp(sprintf(['dt(j)            = ' st],dtj))
            end

            %% MULTI-SCALE RELAXATION WITH THOMAS ALGORITHM (ELDREDGE)
            un = zeros(size(f));

            if multiScale
                for k=1:Nsweeps
                    for j=1:nlevels
                        unp1 = relax1DNeumannTotal(a,un,f,alpha,dtj(j),n,h); %#ok<CPROP>
                        diffNorm = max(abs(un - unp1));
                        un = unp1;
                    end
                    for j=nlevels:-1:1
                        unp1 = relax1DNeumannTotal(a,un,f,alpha,dtj(j),n,h); %#ok<CPROP>
                        diffNorm = max(abs(un - unp1));
                        un = unp1;
                    end
                end
            else
                for k=1:Nsweeps
                    unp1 = relax1DNeumannTotal(a,un,f,alpha,dt,n,h); %#ok<CPROP>
                    diffNorm = max(abs(un - unp1));
                    un = unp1;
                end
            end
            un = un - mean(un);
            u_trisolve = un;

            %% ERROR
            err_trisolve = max(abs(u_trisolve - u_solution)); %#ok<CPROP>

            disp(['Difference between iterates = ' num2str(diffNorm)])
            disp(['Error w.r.t. exact discrete solution = ' num2str(err_trisolve)])

            %% VISUALIZATION
            figure
            subplot(2,1,1)
            plot(x,u_trisolve,'b-',x,u_solution,'r-') %#ok<CPROP>
            title('Solution to \nabla^2u = f')
            xlabel('x')
            ylabel('u(x)')
            legend('u_{trisolve}','u_{solution}')

            subplot(2,1,2)
            plot(x,f,'k-')
            title('Forcing term in \nabla^2u = f')
            xlabel('x')
            ylabel('f(x)')
            legend('f')

            a.u_solution = u_solution; %#ok<CPROP>
            a.u_trisolve = u_trisolve;
        end
        function u = relax1DNeumannTotal(a,un,f,alpha,dt,n,h)
            %% Solves the equation
            % (I - .5 dt alpha B) u = (I + .5 dt alpha B) un - dt*f
            % for u. Where B = tridiag(1,-2,1)

            % Construct RHS
            [loDiag, Diag, upDiag] = setUpSystemNeumannTotal(a,.5*dt*alpha,h,n);
            Diag = 1 + Diag;
            A = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);
            dbstop at 190 in modelProblem
            utemp = un; utemp(2:end-1) = 0;
            lapUBC = lap(a,utemp,alpha,h);
%             dbstop at 197 in modelProblem
            % f(1) = f(1) - alpha/(h^2)*un(2);
            % f(end) = f(end) - alpha/(h^2)*un(end-1);
            % rhs = A*un - dt*f;
            
            f(1) = f(1) - lapUBC(2);
            f(end) = f(end) - lapUBC(end-1);
            rhs = A*un - dt*f;

            [loDiag, Diag, upDiag] = setUpSystemNeumannTotal(a,-.5*dt*alpha,h,n);
            Diag = 1 + Diag;
            u = trisolve(loDiag,Diag,upDiag,rhs,'reg');

        end
        function [a b c] = setUpSystemNeumannTotal(obj,alpha,h,n)
            a = alpha*ones(1,n)/h^2;
            b = alpha*(-2*ones(1,n+1)/h^2);
            c = alpha*ones(1,n)/h^2;
        end


        %% Uses trisolve only on interior system
        function a = neumannInterior(a,x,u_solution,f,u_bcs,alpha,n,Nsweeps,nlevels,dt,multiScale)
            dh = x(2) - x(1);
            h = x;
            %% PREP FORMATTED OUTPUT
            st = [];
            for i=1:nlevels; if (i~=nlevels);st = [st '%d\t\t,'];else;st = [st '%d']; end; end

            %% MULTI-SCALE TIME STEP SELECTION
            if multiScale; disp(['Number of sweeps = ' num2str(Nsweeps)]); end
            if ~multiScale; disp(['Number of iterations = ' num2str(Nsweeps)]); end
            if multiScale; disp(['Number of multi-scale time levels = ' num2str(nlevels)]);end

            dtj = zeros(1,nlevels);
            for j=1:nlevels
                hj = 2^(j-1)*dh; dtj(j) = 4*(hj^2)/(alpha*pi^2);
            end
            if multiScale
                disp(sprintf(['dt(j)            = ' st],dtj))
            end

            %% MULTI-SCALE RELAXATION WITH THOMAS ALGORITHM (ELDREDGE)
            un = zeros(size(f));

            if multiScale
                for k=1:Nsweeps
                    for j=1:nlevels
                        unp1 = relax1DNeumannInterior(a,un,f,u_bcs,alpha,dtj(j),n,h); %#ok<CPROP>
                        diffNorm = max(abs(un - unp1));
                        un = unp1;
                    end
                    for j=nlevels:-1:1
                        unp1 = relax1DNeumannInterior(a,un,f,u_bcs,alpha,dtj(j),n,h); %#ok<CPROP>
                        diffNorm = max(abs(un - unp1));
                        un = unp1;
                    end
                end
            else
                for k=1:Nsweeps
                    unp1 = relax1DNeumannInterior(a,un,f,u_bcs,alpha,dt,n,h); %#ok<CPROP>
                    diffNorm = max(abs(un - unp1));
                    un = unp1;
                end
            end
            un = un - mean(un);
            u_trisolve = un;

            %% ERROR
            err_trisolve = max(abs(u_trisolve - u_solution)); %#ok<CPROP>

            disp(['Difference between iterates = ' num2str(diffNorm)])
            disp(['Error w.r.t. exact discrete solution = ' num2str(err_trisolve)])

            %% VISUALIZATION
            figure
            subplot(2,1,1)
            plot(x,u_trisolve,'b-',x,u_solution,'r-') %#ok<CPROP>
            title('Solution to \nabla^2u = f')
            xlabel('x')
            ylabel('u(x)')
            legend('u_{trisolve}','u_{solution}')

            subplot(2,1,2)
            plot(x,f,'k-')
            title('Forcing term in \nabla^2u = f')
            xlabel('x')
            ylabel('f(x)')
            legend('f')

            a.u_solution = u_solution; %#ok<CPROP>
            a.u_trisolve = u_trisolve;
        end
        function u = relax1DNeumannInterior(a,un,f,u_bcs,alpha,dt,n,h)
            %% Solves the equation
            % (I - .5 dt alpha B) u = (I + .5 dt alpha B) un - dt*f
            % for u. Where B = tridiag(1,-2,1)
            dh = h(2) - h(1);
            
            % Construct RHS
            [loDiag, Diag, upDiag] = setUpSystemNeumannInterior(a,.5*dt*alpha,dh,n);
            Diag(2:end-1) = 1 + Diag(2:end-1);
            A = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);

            utemp = un; utemp(2:end-1) = 0;
            lapUBC = lap(a,utemp,alpha,dh);
            dbstop at 303 in modelProblem
            % Boundary values no longer matter since interior of matrix is
            % inverted.
            % f(1) = f(1) - lapUBC(2);
            % f(end) = f(end) - lapUBC(end-1);
            rhs = A*un - dt*f;

            [loDiag, Diag, upDiag] = setUpSystemNeumannInterior(a,-.5*dt*alpha,dh,n);
            Diag(2:end-1) = 1 + Diag(2:end-1);
            
            u = zeros(size(un));
            u(2:end-1) = trisolve(loDiag(2:end-1),Diag(2:end-1),upDiag(2:end-1),rhs(2:end-1),'reg');

            u = applyBCs(a,u,u_bcs,h);

        end
        function [a b c] = setUpSystemNeumannInterior(obj,alpha,h,n)
            a = [alpha*ones(1,n-1)/h^2 0];
            b = [0 -2*alpha*ones(1,n-1)/h^2 0];
            c = [0 alpha*ones(1,n-1)/h^2];
        end


        %% Other routines
        function lapU = lap(obj,u,alpha,h)
            lapU = [0; alpha*diff(diff(u))/h^2; 0];
        end
        function err = computeNorms(f_approx,f_exact)
            err.L2 = norm(f_approx-f_exact)/norm(f_exact);
            err.L1 = norm(f_approx-f_exact,1)/norm(f_exact,1);
            err.max = max(abs(f_approx-f_exact))/max(abs(f_exact));
            disp(['L1 error = ' num2str(err.L1)]);
            disp(['L2 error = ' num2str(err.L2)]);
            disp(['Linf error = ' num2str(err.max)]);
        end
        function u = applyBCs(obj,u,u_bcs,h)
            dh = h(2)-h(1);
            if (u_bcs.type==1) % Dirichlet (wall coincident) ~O(dh)
                u(1) = u_bcs.val(1);
                u(end) = u_bcs.val(end);
            elseif (u_bcs.type==2) % Dirichlet (interpolated) ~O(dh^2)
                u(1) = u(2) + (u_bcs.val(1) - u(2));
                u(end) = u(end-1) + (u_bcs.val(1) - u(end-1));
            elseif (u_bcs.type==3) % Neumann (wall coincident) ~O(dh^2)
                b = 2*dh; a = dh;
                u(1) =   (u_bcs.val(1)*(b-a)-b/a*u(2)+a/b*u(3))/(a/b-b/a);
                u(end) = (u_bcs.val(end)*(b-a)-b/a*u(end-1)+a/b*u(end-2))/(a/b-b/a);
            elseif (u_bcs.type==4) % Neumann - wall coincident ~O(dh)
                u(1) = u(1) - dh*u_bcs.val(1);
                u(end) = u(end-1) + dh*u_bcs.val(end);
            elseif (u_bcs.type==5) % Neumann - wall incoincident ~O(dh^2)
                u(1) = u(1) + dh*u_bcs.val(1);
                u(end) = u(end-1) + dh*u_bcs.val(end);
            end
        end

    end
end