function A = getOperator(operator,x,c)
s = length(x.vals);
A = zeros(s,s);
xhat = x;
xhat.vals(:) = 0;
for i=1:s
    xhat.vals(i) = 1;
    temp = operator(xhat,c);
    A(:,i) = temp.vals;
    xhat.vals(i) = 0;
end
end