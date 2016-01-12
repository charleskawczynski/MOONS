function analyzeMatrix(A,name,symmetryTest,conditionNumber,preconditioner,getEig,debug)

%% Check if SPD
s = size(A);
if symmetryTest
    u = rand(s(1),1); v = rand(s(1),1);
    e1 = (v'*A*u - u'*A*v)/length(u);
    e2 = max(max(abs(A - A')));
    disp([' ---------------------------- Analysis of matrix operator ' name ' ---------------------------- '])
    disp(['Symmetry error (v^T*A*u - u^T*A*v)/size(u) = ' num2str(e1)])
    disp(['Symmetry error max(max(abs(A - A^T))) = ' num2str(e2)])
end
if conditionNumber
    disp(['Condition number = ' num2str(cond(A))])
end
if preconditioner
    M = diag(1./diag(A));
    A_prec = M*A;
end
if preconditioner && conditionNumber
    disp(['Condition number after preconditioning = ' num2str(cond(A_prec))])
end
if getEig
    disp(['Eigenvalues = ']); Eig = eig(A)'
end

%% Plot original matrix
% figure; spy(A)
% title(['spy(' name ')'])

% figure; surf(A)
% title(['Matrix surface of ' name ' operator'])

if preconditioner
%     figure; surf(A_prec)
%     title(['Matrix surface of preconditioned ' name ' operator'])
    figure; plot(diag(A))
    title(['diag(' name ')'])
%     figure; plot(1./diag(A))
%     title(['1/diag(' name ')'])
end


if debug
    idx = eye(size(A)); Y = (1-idx).*A;
    figure
    surf(Y)
    title(['Matrix (minus diagonal): ' name ' operator'])


    %% Plot SPD error of matrix (minus diagonal, since this likely does not contribute)

    if (max(max(abs(A-A')))>0)
        figure; spy(A-A')
        title(['SPD erorr of matrix : ' name ' operator'])

        idx = eye(size(A)); Y = (1-idx).*A;
        figure
        contourf(Y-Y')
        title(['SPD error of matrix (minus diagonal): ' name ' operator'])
    end
end

end