      subroutine mult2(A,lla,lua, na, nna, B,llb,lub,nb,nnb,C,nc,nnc)

c This subroutine multiplies together two polynomials a(x) and b(x):
c (for multiplying two polynomials, both in F an B)
c Note that la and lb, the dimensions of A and B from the main program
c may be different from that used in the subroutine
c
c    a(x) = a(-na)x^(-na) + ...+ a0 + a1*x^(1) + a2*x^(2) + ... + a(nna)*x^(nna)
c
c    b(x) = b(-nb)x^(-nb) + ...+ b0 + b1*x^(1) + b2*x^(2) + ... + b(nnb)*x^(nnb)
c
c Note that the general form used here has positive signs for the 
c coefficients.  The coefficients are stored in elements -na to nna of 
c array A and -nb to nnb of array B in the order of increasing powers of x, i.e.
c
c    A(-na:na) = [a(-na), ...a0, a1, ... , a(nna)]'.
c
c    B(-nb:nb) = [b(-nb), ...b0, b1, ... , b(nnb)]'.
c
c The product c(x) = a(x)b(x),  its coefficients are 
c stored in the array C = [c0, ... , c(nc)]'.
c Note, nc is the dimensions of the vector c, but all of the vector may not be used

      double precision A(-lla:lua),B(-llb:lub),C(-nc:nnc)

      do 1 j = -nc,nnc
        C(j) = 0.0D0
 1    continue
         do 3 j = -na,nna
            do 4 k = -nb,nnb
                  C(k+j) = C(k+j) + A(j)*B(k)
 4          continue
 3       continue
      return
      end
c      do 2 i = -(na+nb),nna+nnb
c         do 3 j = -na,nna
c            do 4 k = -nb,nnb
c               if (k+j .eq. i) then
c                  C(i) = C(i) + A(j)*B(k)
c               end if
c4           continue
c3        continue
c2     continue
