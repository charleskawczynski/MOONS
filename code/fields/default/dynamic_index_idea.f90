do k=1+pad,f%s(3)-pad; do j=1+pad,f%s(2)-pad
  call operator(dfdh%f(:,j,k),f%f(:,j,k),T,f%s(dir),dfdh%s(dir),gt)
enddo; enddo

i0=(2-dir)*(3-dir)/2 ! = 1 if dir = 1
j0=(dir-1)*(3-dir)/2 ! = 1 if dir = 2
k0=(dir-1)*(dir-2)/2 ! = 1 if dir = 3

i_line,j_line,k_line
suppose dir = 1
i1
i1 = i*i0 + 1*(1-i0); i2 = i*i0 + i_stop*(1-i0)
j1 = j*j0 + 1*(1-j0); j2 = j*j0 + j_stop*(1-j0)
k1 = k*k0 + 1*(1-k0); k2 = k*k0 + k_stop*(1-k0)
i_line = (/(1+pad,i=1,f%s(dir)-pad)/)






do k=1,i_line%s; do j=1,j_line%s; do i=1,i_line%s
  call operator(dfdh%f(di(dir)%i1(i):di(dir)%i2(i),di(dir)%j1(j):di(dir)%j2(j),di(dir)%k1(k):di(dir)%k2(k)),&
  	               f%f(di(dir)%i1(i):di(dir)%i2(i),di(dir)%j1(j):di(dir)%j2(j),di(dir)%k1(k):di(dir)%k2(k)),&
  	               T,f%s(dir),dfdh%s(dir),gt)
enddo; enddo; enddo
