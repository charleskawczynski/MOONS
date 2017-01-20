function n = Ln_norms(u)
n.L1 = sum(abs(u));
n.L2 = u'*u;
n.Linf = max(abs(u));
n.L2 = n.L2^0.5;
end