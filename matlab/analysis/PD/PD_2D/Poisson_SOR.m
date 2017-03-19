function u = Poisson_SOR(u,f,m)
dx2 = m.x.dh^2;
dy2 = m.y.dh^2;
Nx = m.x.N_cells;
Ny = m.y.N_cells;
r = dx2*dy2/(dx2+dy2);
s = [Nx Ny];
omega = 2.0/(1.0 + sqrt(1.0 - ((cos(pi/(Nx)) + cos(pi/(Ny)))/2.0)^2.0));
for ijk=1:50
    %% SOR loop
    for i=2:s(1)-1
        for j=2:s(2)-1
            u.vals(i,j) = u.vals(i,j)*(1.0-omega) + ...
                omega*0.5*( (u.vals(i+1,j)+u.vals(i-1,j))/(1.0+dx2/dy2)+ ...
                (u.vals(i,j+1)+u.vals(i,j-1))/(dy2/dx2+1.0) - r*f.vals(i,j) );
        end
    end
    %% Apply BCs
    u.vals(1,:) = -u.vals(2,:);
    u.vals(end,:) = -u.vals(end-1,:);
    u.vals(:,1) = -u.vals(:,2);
    u.vals(:,end) = -u.vals(:,end-1);
end
end