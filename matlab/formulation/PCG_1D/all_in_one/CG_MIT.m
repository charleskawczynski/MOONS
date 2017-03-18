function CG = CG_MIT(x,A,b,iter)
[x r norms] = CG_MIT_raw(x,A,b)
CG.r = r;
CG.x = x;
CG.norms(iter) = norms;
end