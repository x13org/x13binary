      subroutine mult1(A,la,na,B,lb,nb,C,nc)

c This subroutine multiplies together two polynomials a(x) and b(x):
c (for multiplying two polynomials in B)
c Note that la and lb, the dimensions of A and B from the main program
c may be greater than na and na, the dimensions used in the subroutine
c
c    a(x) = a0 + a1*x^(1) + a2*x^(2) + ... + a(na)*x^(na)
c
c    b(x) = b0 + b1*x^(1) + b2*x^(2) + ... + b(nb)*x^(nb)
c
c
c Note that the general form used here has positive signs for the 
c coefficients.  The coefficients are stored in elements 0 to na of 
c array A and -nb to nc of array B in the order of increasing powers of x, i.e.
c
c    A(0:na) = [a0, a1, ... , a(na)]'.
c
c    B(0:nb) = [b0, b1, ... , b(nb)]'.
c
c The product c(x) = a(x)b(x),  its coefficients are 
c stored in the array C = [c0, ... , c(nc)]'.
c Note, nc is the dimensions of the vector c, but all of the vector may not be used

      double precision A(0:la),B(0:lb),C(0:nc)

      do 1 j = 0,nc
        C(j) = 0.0D0
 1    continue
      do 3 j = 0,na
        do 4 k = 0,nb
          C(k+j) = C(k+j) + A(j)*B(k)
 4      continue
 3    continue
c      do 2 i = 0,na+nb
c         do 3 j = 0,na
c            do 4 k = 0,nb
c               if (k+j .eq. i) then
c                  C(i) = C(i) + A(j)*B(k)
c               end if
c4           continue
c3        continue
c2     continue
      return
      end
      