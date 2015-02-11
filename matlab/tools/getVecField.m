function [x,y,z,Nx,Ny,Nz,varx,vary,varz] = getVecField(dir,name,components)

temp = myImport(name);
varx = temp(:,components(1));
vary = temp(:,components(2));
varz = temp(:,components(3));

[Nx Ny Nz] = getN(temp);

varx = reshape(varx,Nx,Ny,Nz);
vary = reshape(vary,Nx,Ny,Nz);
varz = reshape(varz,Nx,Ny,Nz);

temp2 = reshape(temp(:,1),Nx,Ny,Nz); x = temp2(:,1,1); x = reshape(x,1,Nx);
temp2 = reshape(temp(:,2),Nx,Ny,Nz); y = temp2(1,:,1); y = reshape(y,1,Ny);
temp2 = reshape(temp(:,3),Nx,Ny,Nz); z = temp2(1,1,:); z = reshape(z,1,Nz);

end