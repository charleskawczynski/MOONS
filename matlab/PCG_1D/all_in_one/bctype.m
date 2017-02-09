function BCT = bctype(num)
switch num
    case 1
        BCT.Dirichlet = true;
        BCT.Neumann = false;
        BCT.Periodic = false;
    case 2
        BCT.Dirichlet = false;
        BCT.Neumann = true;
        BCT.Periodic = false;
    case 3
        BCT.Dirichlet = false;
        BCT.Neumann = false;
        BCT.Periodic = true;
    otherwise
        error('Bad bctype input')
end
end