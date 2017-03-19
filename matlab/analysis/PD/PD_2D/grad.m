function grad = grad(f,m,dir)
if dir==1
    grad = ( f.vals(3:end,:)-f.vals(1:end-2,:) )/(2*m.x.dh);
elseif dir==2
    grad = ( f.vals(:,3:end)-f.vals(:,1:end-2) )/(2*m.y.dh);
else
    error('dir must = 1,2 in grad.m')
end
end