function u = save_i_from_ends(u,i)
temp=zeros(size(u));
temp(1+i)=u(1+i);
temp(end-i) = u(end-i);
u=temp;
end