function Ax = operator(x,c,centered)
Ax = x;
x = apply_BCs(x,c,false,centered);
temp = del(x,c);
temp = switchToDual(temp);
Ax = del(temp,c);
Ax = switchToDual(Ax);
Ax.vals = zeroghostpoints(Ax.vals);
end