function [x,y,z,Nx,Ny,Nz,var] = getField(dir,name,component)

addpath(dir.working)

[temp nheaderLines] = myImport(name);
var = temp(:,component);

[Nx Ny Nz] = getN(temp,nheaderLines);
var = reshape(var,Nx,Ny,Nz);

temp2 = reshape(temp(:,1),Nx,Ny,Nz); x = temp2(:,1,1); x = reshape(x,1,Nx);
temp2 = reshape(temp(:,2),Nx,Ny,Nz); y = temp2(1,:,1); y = reshape(y,1,Ny);
temp2 = reshape(temp(:,3),Nx,Ny,Nz); z = temp2(1,1,:); z = reshape(z,1,Nz);

% rmpath(dir.working)

end