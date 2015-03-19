function lapU = lap(u,alpha,h)
lapU = [0; alpha*diff(diff(u))/h^2; 0];
end