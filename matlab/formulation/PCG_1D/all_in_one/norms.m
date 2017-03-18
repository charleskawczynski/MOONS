function [L1 L2 Linf] = norms(x)
L1 = sum(abs(x));
L2 = x'*x;
Linf = max(abs(x));
L2 = L2^0.5;
end