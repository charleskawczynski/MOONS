function print_N_cells(x,y,z,r,r_power)
F = r.^r_power;
if x.N_FWV ~= z.N_FWV
	error('Error: use same number of points for x and z');
end

% Compute number of cells in each region, then multiply by ratio and round:
X = ceil([F(1)*x.N_FWV(1) F(2)*x.N_FWV(2) F(3)*x.N_FWV(3)]);
Y = ceil([F(1)*y.N_FWV(1) F(2)*y.N_FWV(2) F(3)*y.N_FWV(3)]);
Z = ceil([F(1)*z.N_FWV(1) F(2)*z.N_FWV(2) F(3)*z.N_FWV(3)]);

% Compute total along each direction (2 * wall + 2 * vacuum), then sum:
X_tot = [X(1) 2*X(2) 2*X(3)]; XT = sum(X_tot);
Y_tot = [Y(1) 2*Y(2) 2*Y(3)]; YT = sum(Y_tot);
Z_tot = [Z(1) 2*Z(2) 2*Z(3)]; ZT = sum(Z_tot);

% Total number of cells
T = sum(XT*YT*ZT);

% disp(['N_cells_x[ Fluid , Wall (each) , Vacuum (each) ] = (' num2str(X(1)) ',' num2str(X(2)) ',' num2str(X(3)) ')'])
% disp(['N_cells_y[ Fluid , Wall (each) , Vacuum (each) ] = (' num2str(Y(1)) ',' num2str(Y(2)) ',' num2str(Y(3)) ')'])
% disp(['N_cells_z[ Fluid , Wall (each) , Vacuum (each) ] = (' num2str(Z(1)) ',' num2str(Z(2)) ',' num2str(Z(3)) ')'])
% disp(['N_cells_total(x,y,z) = (' num2str(XT) ',' num2str(YT) ',' num2str(ZT) ')'])
% disp(['N_cells total (millions) = ' num2str(T/10^6)])

disp(['N_cells_x[ Fluid , Wall (each) , Vacuum (each) ] = (' num2str(X(1)) ',' num2str(X(2)) ',' num2str(X(3)) ')' ', total (millions) = ' num2str(T/10^6)])
end