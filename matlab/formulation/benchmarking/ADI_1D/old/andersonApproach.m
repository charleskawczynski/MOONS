classdef andersonApproach
    properties
        u_solution,u_trisolve,f,err;
    end
    methods
        function a = andersonApproach(n,neumann,Nsweeps,p,nlevels,tol,dt,multiScale)
            %% GRID GENERATION
            x = linspace(0,1,n+1); h = x(2) - x(1); alpha = 2;

            %% PREP FORMATTED OUTPUT
            st = [];
            for i=1:nlevels; if (i~=nlevels);st = [st '%d\t\t,'];else;st = [st '%d']; end; end

            %% PROBLEM SETUP
            u_solution = cos(p*pi*x)';
            if (neumann); u_solution = u_solution - mean(u_solution); end

            f = lap(u_solution,alpha,h);
            f(1) = alpha*(-u_solution(1)+u_solution(2))/h^2;
            f(end) = alpha*(-u_solution(end)+u_solution(end-1))/h^2;

            f = f - mean(f);

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
            if (~neumann); un(1) = u_solution(1); un(end) = u_solution(end); end

            if multiScale
                for k=1:Nsweeps
                    for j=1:nlevels
                        if (neumann); unp1 = relax1DNeumann(un,f,alpha,dtj(j),n,h); end
                        if (~neumann); unp1 = relax1D(un,f,alpha,dtj(j),n,h); end
                        diffNorm = max(abs(un - unp1));
                        un = unp1;
                    end
                    for j=nlevels:-1:1
                        if (neumann); unp1 = relax1DNeumann(un,f,alpha,dtj(j),n,h); end
                        if (~neumann); unp1 = relax1D(un,f,alpha,dtj(j),n,h); end
                        diffNorm = max(abs(un - unp1));
                        un = unp1;
                    end
                end
            else
                for k=1:Nsweeps
                    if (neumann); unp1 = relax1DNeumann(un,f,alpha,dt,n,h); end
                    if (~neumann); unp1 = relax1D(un,f,alpha,dtj(j),n,h); end
                    diffNorm = max(abs(un - unp1));
                    un = unp1;
                end
            end
            if (neumann); un = un - mean(un); end
            % if (~neumann); un = un - mean(un); end
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
            fact = 1.8;
            % axis([-0.1 1.1 min(u_solution)*(fact) max(u_solution)*fact])

            subplot(2,1,2)
            plot(x,f,'k-')
            title('Forcing term in \nabla^2u = f')
            xlabel('x')
            ylabel('f(x)')
            legend('f')
            % axis([-0.1 1.1 min(f)*(fact) max(f)*fact])

            a.u_solution = u_solution;
            a.u_trisolve = u_trisolve;
        end
        function [a b c] = setUpSystem(alpha,h,n)
            a = [alpha*ones(1,n-1)/h^2 0];
            b = [1 -2*alpha*ones(1,n-1)/h^2 1];
            c = [0 alpha*ones(1,n-1)/h^2];
        end
        function u = relax1DNeumann(un,f,alpha,dt,n,h)
            %% Solves the equation
            % (I - .5 dt alpha B) u = (I + .5 dt alpha B) un - dt*f
            % for u. Where B = tridiag(1,-2,1)

            % Construct RHS
            [loDiag, Diag, upDiag] = setUpSystemNeumann(.5*dt*alpha,h,n);
            Diag(2:end-1) = 1 + Diag(2:end-1);
            A = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);

            % u_ghost = zeros(1,2); g = 0; % Ghost point / slope for Neumann

            % u_ghost(1) = un(2) - h*g;
            % u_ghost(2) = un(end-1) - h*g;

            % f(1) = f(1) - alpha/(h^2)*u_ghost(1);
            % f(end) = f(end) - alpha/(h^2)*u_ghost(2);

            rhs = A*un - dt*f;
            rhs(1) = 0; rhs(end) = 0;
            % rhs = rhs - mean(rhs);

            [loDiag, Diag, upDiag] = setUpSystemNeumann(-.5*dt*alpha,h,n);
            Diag(2:end-1) = 1 + Diag(2:end-1);
            % u = trisolve(loDiag,Diag,upDiag,rhs,'reg');
            u = zeros(size(un));
            u(2:end-1) = trisolve(loDiag(2:end-1),Diag(2:end-1),upDiag(2:end-1),rhs(2:end-1),'reg');
            % u(1) = u(2);
            % u(end) = u(end-1);

            g = 0;
            u(1) = 1/3*(4*u(2) - 2*h*g - u(3));
            u(end) = -1/3*(4*u(end-1) - 2*h*g - u(end-1));

        end
    end
end