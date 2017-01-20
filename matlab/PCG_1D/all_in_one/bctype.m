function BCT = bctype(num)
switch num
    case 1
        BCT.Dirichlet = true;
        BCT.Neumann = false;
    case 2
        BCT.Dirichlet = false;
        BCT.Neumann = true;
    otherwise
        error('Bad bctype input')
end
end