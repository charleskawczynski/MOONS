function Dx = del(x,c)
Dx = x;
Dx.vals = zeros(c.sc,1);
Dx.vals(:) = diff(x.vals)./c.dhn';
end