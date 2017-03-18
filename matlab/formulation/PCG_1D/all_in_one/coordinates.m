function c = coordinates(N_cells)
c.a = 0;
c.b = 1;
c.L = c.b - c.a;
c.N_cells = N_cells;
c.dh = c.L/(c.N_cells);
c.hn = c.a:c.dh:c.b;

c.hn = [c.hn(1)-c.dh c.hn c.hn(end)+c.dh];
c.hc = c.hn+c.dh/2; c.hc = c.hc(1:end-1);

c.h.n = c.hn;
c.h.c = c.hc;

c.dhn = diff(c.hn);
c.dhc = diff(c.hc);

c.sc = length(c.hc);
c.sn = length(c.hn);

c.vol.c = c.dhn';
c.vol.c(1) = 0;
c.vol.c(end) = 0;
c.vol.n = c.dhc';
c.vol.n = [0; c.vol.n; 0];

c.T.C2N.D = -1./c.dhc';
c.T.C2N.U = 1./c.dhc';

c.T.N2C.D = -1./c.dhn';
c.T.N2C.U = 1./c.dhn';
end