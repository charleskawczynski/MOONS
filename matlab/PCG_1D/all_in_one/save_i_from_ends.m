function x = save_i_from_ends(x,i)
temp=zeros(size(x));
temp(1+i)=x(1+i);
temp(end-i) = x(end-i);
x=temp;
end