function D = symmetry_prefactor(A,TF)
s = size(A);
m = s(1);
c = ones(m,1);
c(1) = 1;
for i=1:m
    for k=1:m-1
        if (i+k<=m)
            if (abs(A(i,i+k))>0) && (abs(A(i+k,i))>0)
                if abs((A(i,i+k) - A(i+k,i)))>0
                    c(i+k) = c(i)*A(i,i+k)/A(i+k,i);
%                     if TF
%                         disp(['c_' num2str(i+k-1) ' = ' num2str(c(i+k-1)) ])
%                         disp(['A(' num2str(i) ',' num2str(i+k) ') = ' num2str(A(i,i+k))])
%                         disp(['A(' num2str(i+k) ',' num2str(i) ') = ' num2str(A(i+k,i))])
%                         disp(['c_' num2str(i+k) ' = ' num2str(c(i+k)) ])
%                         disp('---------------------------------------------------')
%                     end
                end
            end
        end
    end
end
D = diag(c);
end