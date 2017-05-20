function print_N_cells(x,y,z,r,r_power)
F = r^r_power;
if x.N_FWV ~= z.N_FWV
	error('Error: use same number of points for x and z');
end

% Add 2 times wall plus two times vacuum. Then define total along each direction:
X = [x.N_FWV(1) 2*x.N_FWV(2) 2*x.N_FWV(3)]; x.N_FWV_tot = sum(X);
Y = [y.N_FWV(1) 2*y.N_FWV(2) 2*y.N_FWV(3)]; y.N_FWV_tot = sum(Y);
Z = [z.N_FWV(1) 2*z.N_FWV(2) 2*z.N_FWV(3)]; z.N_FWV_tot = sum(Z);

% Scale by factor, then take ceiling:
X = ceil(X*F); XT = ceil(x.N_FWV_tot*F);
Y = ceil(Y*F); YT = ceil(y.N_FWV_tot*F);
Z = ceil(Z*F); ZT = ceil(z.N_FWV_tot*F);

% Total number of cells
T = sum(XT*YT*ZT);

disp(['Ratio compared to r^0 mesh = ' num2str(F)])
disp(['N_cells_xz(F,W,V) = (' num2str(X(1)) ',' num2str(X(2)) ',' num2str(X(3)) ')'])
% disp(['N_cells_x(F,W,V) = (' num2str(X(1)) ',' num2str(X(2)) ',' num2str(X(3)) ')'])
disp(['N_cells_y (F,W,V) = (' num2str(Y(1)) ',' num2str(Y(2)) ',' num2str(Y(3)) ')'])
% disp(['N_cells_z(F,W,V) = (' num2str(Z(1)) ',' num2str(Z(2)) ',' num2str(Z(3)) ')'])
disp(['N_cells_total(x,y,z) = (' num2str(XT) ',' num2str(YT) ',' num2str(ZT) ')'])
disp(['N_cells total (millions) = ' num2str(T/10^6)])
end