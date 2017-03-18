function [ x ] = trisolve(a, b, c, f, type )
%TRISOLVE Solve a simple tridiagonal system of equations
%   Given a matrix A with a tridiagonal structure specified by the
%   elements a, b, and c, and the right-hand side column array f, solve the
%   system to find x. The size of the system is determined from the size of
%   f.
%   
%     x = trisolve(a,b,c,f,'reg') would set A as a regular tridiagonal
%       matrix. Here, a, b and c could be scalars (in which it is assumed
%       that each diagonal is constant) or vectors (in which b is the main
%       diagonal and should have the same length as f, and a and c are the
%       sub- and super-diagonal and have length one smaller than b).
%
%     x = trisolve(a,b,c,f,'circ') would set A as a circulant tridiagonal
%       matrix. Here, a, b and c are restricted to be scalars.
%
%   MAE 259A
%   J. D. Eldredge
%   1/5/2014

M = length(f);

if (strcmp(type,'circ')),
    % For a circulant matrix, get the eigenvalues
    m = 0:M-1;
    lam = b + (a+c)*cos(2*pi*m/M) - 1i*(a-c)*sin(2*pi*m/M);
    
    % Transform the rhs (fhat = V^-1*f).
    fhat = fft(f)/M;
    
    % Get xhat = Lam^-1*fhat. Note the use of .' rather than ' to compute
    % the non-conjugate transpose of the eigenvalue array.
    xhat = fhat./lam.';
    
    % Get x = V*xhat by transforming back.
    x = real(M*ifft(xhat));
elseif (strcmp(type,'reg'))
    
    x = zeros(size(f));
    if (isscalar(b)),
        % Treat the case of constant diagonals
        if (b == 0),
            disp('ERROR: Matrix is singular.');
            return
        end
        % Modify the first row coefficients.
        cvec = ones(M-1,1);
        cvec(1) = c/b;
        f(1) = f(1)/b;
        ctemp = cvec(1);
        ftemp = f(1);
        for ii = 2:M-1
            temp = b - a*ctemp;
            ctemp = c/temp;  % ctemp was cvec(ii-1), now cvec(ii)
            ftemp = (f(ii) - a*ftemp)/temp; % ftemp was f(ii-1), now f(ii)
            cvec(ii) = ctemp;
            f(ii) = ftemp;
        end
        f(M) = (f(M)-a*f(M-1))/(b-a*cvec(M-1));
        
        temp = f(M);
        x(M) = temp;
        for ii = M-1:-1:1
            temp = f(ii) - cvec(ii)*temp;
            x(ii) = temp;
        end
    else
        % Treat the case of vector diagonals. Note that the indexing for 
        % a has been adjusted so that it goes from 1 to M-1 (rather than 2
        % to M as in the posted algorithm).
        
        if (b(1) == 0),
            disp('ERROR: First pivot is zero.');
            return
        end
        % Modify the first row coefficients.
        c(1) = c(1)/b(1);
        f(1) = f(1)/b(1);
        ctemp = c(1);
        ftemp = f(1);
        for ii = 2:M-1
            temp = b(ii) - a(ii-1)*ctemp;
            ctemp = c(ii)/temp;
            ftemp = (f(ii) - a(ii-1)*ftemp)/temp;
            c(ii) = ctemp;
            f(ii) = ftemp;
        end
        f(M) = (f(M)-a(M-1)*f(M-1))/(b(M)-a(M-1)*c(M-1));
        
        % Back substitute
        temp = f(M);
        x(M) = temp;
        for ii = M-1:-1:1
            temp = f(ii) - c(ii)*temp;
            x(ii) = temp;
        end
        
    end
        

   
    
    
end


end

