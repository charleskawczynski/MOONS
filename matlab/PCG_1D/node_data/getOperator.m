function A = getOperator(operator,x,c,interiorOnly,centered)
s = length(x.vals);
A = zeros(s,s);
xhat = x;
xhat.vals(:) = 0;
for i=1:s
    if interiorOnly
        if i==1 || i==s
            continue
        end
    end
    xhat.vals(i) = 1;
    temp = operator(xhat,c,centered);
    A(:,i) = temp.vals;
    xhat.vals(i) = 0;
end
end