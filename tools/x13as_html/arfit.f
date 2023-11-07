CC
C  Routine qrdcmp(a,m,n,R) performs the QR decomposition A=QR
C  Returns R: upper triangular matrix, n*n. Q is such that: Q'Q=I.
CC     
      subroutine  qrdcmp(A,m,n,R2)
C ..
C
      implicit none
C
C.. Formal Arguments ..
	integer m,n
      real*8 A(m,n),R2(n,n)
C
C.. Local Scalars ..
      integer i,j,k,l,n1
      real*8 f,g,h,s
C
C.. Local Arrays ..
      real*8 R(m,n)
      
      do i = 1,m
	 do j = 1,n
	  R(i,j) = A(i,j)
	 end do
	end do
      do i = 1,n
	 l = i+1
	 s = 0.0d0
	 do j = i, m
	  s = s + R(j,i)*R(j,i)
	 end do
	 f = R(i,i)
	 g = sqrt(s)
	 if ( f .ge. 0.0d0) then
	  g = -g
	 end if
	 h = f*g - s
	 R(i,i) = f - g
	 do j = l,n
	  s = 0.0
	  do k = i,m
	   s = s + R(k,i)*R(k,j)
	  end do
	  f = s/h
	  do k = i,m
         R(k,j) = R(k,j) + f*R(k,i)
        end do
	 end do
	 R(i,i) = g
	enddo
      if (m .lt. n) then
	 n1 = m
      else 
	 n1 = n
	end if
      do i = 2,n1
	 do j = 1,i-1
	  R(i,j) = 0.0d0
	 end do
	enddo

      do i = 1,n
	 do j = 1,n
	  R2(i,j) = R(i,j)
	 end do
	end do
	return
	end
CC
C     Routine mat_sqr(A,la,ca,C) returns C=A'A, C[1...ca][1...ca].
CC
      subroutine mat_sqr(A,la,ca,C)
C ..
C
      implicit none
C
C.. Formal Arguments ..
	integer la,ca
      real*8 A(la,ca),C(ca,ca)
C
C.. Local Arrays ..
      real*8 AA(la,ca)
      call mat_trans(A,la,ca,AA)
      call mat_mult(AA,ca,la,A,ca,C)
      return
	end
CC
C     mat_trans() transpose a matrix. 
CC
      subroutine mat_trans(A,la,ca,C)
C ..
C
      implicit none
C
C.. Formal Arguments ..
	integer la,ca
      real*8 A(la,ca),C(ca,la)
C
C.. Local Scalars ..
      integer i,j
      
      do i=1,la
	 do j = 1,ca
	  C(j,i) = A(i,j)
	 enddo
	enddo
      return
	end
CC
C      Routine mat_mult multiply two matrix A[1...la][1...ca] and B[1...ca][1...cb]
CC
      subroutine mat_mult(A,la,ca,B,cb,C) 
C ..
C
      implicit none
C
C.. Formal Arguments ..
	integer la,ca,cb
      real*8 A(la,ca),B(ca,cb),C(la,cb)
C
C.. Local Scalars ..
      integer i,j,k
	real*8 sum
      sum = 0.0d0
      do i = 1,la
	 do k = 1,cb
	  do j = 1,ca
	   sum = sum + A(i,j) * B(j,k)
	  end do
	  C(i,k) = sum
	  sum = 0.0
	 end do
	end do
	return
	end
CC
C     Routine mat_inv invert a square matrix of dim n.
CC
      subroutine mat_inv(A,n,c,IA,ifail)
C ..
C
      implicit none
C
C.. Formal Arguments ..
	integer n,ifail
      real*8 A(n,n),IA(n,n)
	real*8 c
C
C.. Local Scalars ..
      integer i, j
      integer indx(n+1)
      real*8 col(n+1)
      call ludcmp(a,n,indx,c,ifail)
      if (ifail .eq. 1) then
	 return
	end if
      do j = 1,n
	 do i = 1,n
	  col(i) = 0.0d0
	 end do
	 col(j) = 1.0d0
	 call lubksb(a,n,indx,col)
	 do i = 1,n
	  ia(i,j) = col(i)
	 end do
	end do
      return
      end
CC
C     FUNCTION LUBKSB : LU BACK-SUBSTITUTION
C     Given a returned from ludcmp() lubsk solves aX=b. The solution is sent
C     back into b. n is the size of the squared matrix a, and indx is provided
C     by ludcmp().
CC
      subroutine lubksb(a,n,indx,b)
C ..
C
      implicit none
C
C.. Formal Arguments ..
	integer n
      integer indx(n+1)
      real*8 A(n,n)
      real*8 b(n+1)
C
C.. Local Scalars ..
      integer i, j,ip,ii
      real*8 sum
      ii = 0
      do i=1,n
	 ip=indx(i)
	 sum=b(ip)
	 b(ip)=b(i)
	 if (ii .ne. 0) then
	  do j=ii,i-1
	   sum = sum - a(i,j)*b(j)
	  end do
	 else if (sum .ne. 0.0d0) then
	  ii=i
	 end if
	 b(i)=sum
	end do
      do i = n,1,-1 
	 sum=b(i)
	 do j=i+1,n
	  sum = sum - a(i,j)*b(j)
	 end do
	 b(i)=sum/a(i,i)
	end do
	return
	end
CC
C     FUNCTION LUDCMP : LU DECOMPOSITION
C     Given a nxn matrix a[1...n][1...n] this routine replace it by its LU
C     decomposition, outputted as a. The vector indx[1...n] records the row
C     permutation, and d = +-1 depending on the whether the number of row
C     interchanges is odd of even.
CC
      subroutine ludcmp(a,n,indx,d,ifail)
C ..
C
      implicit none
      real*8 TINY
	parameter (TINY = 1.0d-20)
C
C.. Formal Arguments ..
	integer n,ifail
      integer indx(n+1)
      real*8 A(n,n)
      real*8 d
C
C.. Local Scalars ..
      integer i, imax, j, k
      real*8 sum, dum, big, temp
	real*8 vv(n+1)
      ifail = 0
	d=1.0d0
	big = 0.0d0
      do i = 1,n
	 big = 0.0d0
	 do j=1,n
	  temp = abs(a(i,j))
	  if (temp .gt. big) then
	   big=temp
	  end if
	 end do
	 if (big .eq. 0.0d0) then
        ifail = 1
	  return
c	    "Singular Matrix in Routine LUDCMP"
	 end if
	 vv(i) = 1.0/big
	end do
      
      do j=1,n
	 do i=1,j-1
	  sum=a(i,j)
	  do k=1,i-1
	   sum = sum - a(i,k)*a(k,j)
        end do
	  a(i,j) = sum
	 end do
	 big = 0.0d0
	 do i = j,n
	  sum=a(i,j)
	  do k = 1,j-1
	   sum = sum - a(i,k)*a(k,j)
	  end do
	  a(i,j) = sum
	  dum = vv(i)*abs(sum)
	  if (dum .ge. big)  then
	   big=dum
	   imax=i
	  end if 
	 end do
	 if (j .ne. imax) then
	  do k=1,n
	   dum = a(imax,k)
	   a(imax,k) = a(j,k)
	   a(j,k) = dum
	  end do
	  d = -d
	  vv(imax) = vv(j)
	 end if
	 indx(j) = imax
	 if (a(j,j) .eq. 0.0d0) then
	  a(j,j) = TINY
	 end if
	 if (j .ne. n)  then
	  dum=1.0d0 / a(j,j)
	  do i=j+1,n
	   a(i,j) = a(i,j) * dum
	  end do
	 end if
	end do
      return
	end
CC
C
CC
      subroutine arfit(x,nz,p,phi,aic,ifail)
C ..
C
      implicit none
C
C.. Formal Arguments ..
	integer nz,p,ifail
      real*8 x(nz),phi(p)
      real*8 aic
C
C.. Local Scalars ..
      integer i, j, ind
	real*8 c,ssr,sum,PI
      real*8 ylag(nz-p,p),tylag(p,nz-p), y(nz-p,1),Rylag(p,p),
     $       Sylag(p,p),Maux(p),Maux2(p,1),arparam(p+1)
cc 
	PI = dacos(-1.0d0)
	if (p .gt. 0) then
       do i = 1,p
        do j=1,nz-p
	   ylag(j,i) = x(p-i+j)
	  end do
	  end do                 
        
        do j=1,nz-p
         y(j,1) = x(p+j)         
        end do
cc	  call qrdcmp(ylag,nz-p,p,Rylag)
cc      call mat_sqr(Rylag,p,p,Sylag)
        call mat_sqr(ylag,nz-p,p,Sylag)
        call mat_inv(Sylag,p,c,Rylag,ifail)
	  if (ifail .eq. 1) then
	   return
	  end if
        call mat_trans(ylag,nz-p,p,tylag)
        call mat_mult(tylag,p,nz-p,y,1,Maux)
        call mat_mult(Rylag,p,p,Maux,1,Maux2)
   

        arparam(1)=1.0d0
        do i=1,p
         arparam(i+1) = -Maux2(i,1)  
         phi(i) = Maux2(i,1)  
	  end do
	  ssr = 0.0d0
        do i=1,nz-p
         sum =0 !Calculo de Residuos de arfit Domingo (11-11-04)
         do j=1,p+1
	    sum = sum + arparam(j) * x(p+i+1-j)
	   end do
         ssr = ssr + sum*sum
	  end do
        aic = ssr
c        aic = -2.0d0*(-.5d0*log(2.0d0*PI) - .5d0*log(ssr/(nz-p))
c     $       - .5d0*(nz-p)) + log(dble(nz-p))*p

	else if (p .eq. 0) then
	 ssr = .0
       do i=1,nz
        ssr = ssr + x(i)*x(i)
	 end do
       aic = ssr
c       aic = -2.0*(-.5*log(2.0*PI) - .5*log(ssr/nz) - .5*nz)
      end if
	return
	end 
