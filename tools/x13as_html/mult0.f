      subroutine mult0(A,la,na,B,llb,lub,nb,nnb,C,llc,luc)

c This subroutine multiplies together two polynomials a(x) and b(x):
c (for multiplying a polynomial in F times a polynomial of F and B)
c (if 2nd polynomial is a polynomial in B, set first dimension equal to 0)
c note that dimensions of matrices A,B from original program (la,llb)
c may be different than the dimension used in subroutine
c
c    a(x) = a0 + a1*x^(-1) + a2*x^(-2) + ... + a(na)*x^(-na)
c
c    b(x) = b(-nb)*x^(-nb) + b(-nb+1)*x^(-nb+1) + b0... + b(nnb)*x^(nnb).
c
c Note that the general form used here has positive signs for the 
c coefficients.  The coefficients are stored in elements 0 to na of 
c array A and -nb to nc of array B in the order of increasing (in absolute value) powers of x, i.e.
c
c    A(0:na) = [a0, a1, ... , a(na)]'.
c
c    B(-nb:nnb) = [b(-nb), b(-nb+1), ..b(0). , b(nnb)]'.
c
c The product c(x) = a(x)b(x),  its coefficients are 
c stored in the array C = [c(-na-nb), ... , c(nc)]'.
c Note, llc and luc are the dimensions of the vector c, but all of the vector may not be used

      double precision A(0:la),B(-llb:lub),C(-llc:luc)

      do 1 j = -llc,luc
        C(j) = 0.0D0
 1    continue
         do 3 j = 0,na
            do 4 k = -nb,nnb
                  C(k-j) = C(k-j) + A(j)*B(k)
 4          continue
 3       continue
c      do 2 i = -(na+nb),nnb
c         do 3 j = 0,na
c            do 4 k = -nb,nnb
c               if (k-j .eq. i) then
c                  C(i) = C(i) + A(j)*B(k)
c               end if
c4           continue
c3        continue
c2     continue
      return
      end
      