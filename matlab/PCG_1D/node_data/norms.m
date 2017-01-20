function [L1 L2 Linf] = norms(u)
L1 = sum(abs(u));
L2 = u'*u;
Linf = max(abs(u));
L2 = L2^0.5;
end