function n = Ln_norms(x)
n.L1 = sum(abs(x));
n.L2 = x'*x;
n.Linf = max(abs(x));
n.L2 = n.L2^0.5;
end