!  ***********************************************
!        Shercliff's Analytical Solution
!  ***********************************************
      subroutine sher(nx1,nx2,nx3,nx4,ny1,ny2,ny3)
      ! dummy arguments
      integer*4 :: nx1,nx2,nx3,nx4,ny1,ny2,ny3
      real*8, pointer :: yy(:)

      ! local variables
      integer*4 :: i,j,n
      real*8 :: Nm,b,factor,dterm,t1,term,cterm
      real*8 :: total,vsum1,vsum2,bb,cc,h1,h2,yc

      allocate( yy(ny1:ny2) )

      i = float(3)*float((nx3-nx2))/float(4)

      b = 0.5*(y(ny2) - y(ny1))

      yc = 0.5*(y(ny2) + y(ny1))

      do j = ny1,ny2
      yy(j) = y(j) - yc
      enddo

! Evaluation of Mean Shercliff Velocity
      total = 0.0d0

      do n = 0,100

         dterm = 1./((2.*n+1.)**4.)

         t1 =  ((2.*n+1.)**2.)*pi*pi
         nm = sqrt((Hart**2.) + (t1/(b*b)))

         term = 2.*nm*b*b*(cosh(nm) - cosh(Hart))/(t1*sinh(nm))
         term = (1. - term)*dterm

         total = total + term

      enddo

      vsum1 = total

! Evaluation of Shape of the Shercliff Velocity Profile

do j = ny1,ny2

      total = 0.0d0

     do n = 0,100

         dterm = ((-1.)**n)/((2.*n+1.)**3.)

         cterm = cos((2.*n+1.)*pi*yy(j)/(2.*b))

            bb = Hart
            cc = -((2.*n+1.)**2.)*pi*pi/(4.*b*b)
            h1 = (-bb + sqrt(bb*bb - 4.*cc))/2.
            h2 = (-bb - sqrt(bb*bb - 4.*cc))/2.

            t1 = sinh((h1-h2))
            term = ((sinh(h2)) - (sinh(h1)))/t1
            term = (1. + term)*cterm
            term = term*dterm

         total = total + term

      enddo

      vsum2 = total

      ua(i,j) = (vsum2/vsum1)*pi/2.0
enddo

deallocate(yy)

      return
      end subroutine sher