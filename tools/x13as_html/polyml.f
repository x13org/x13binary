      SUBROUTINE polyml(Polya,Alag,Na,Polyb,Blag,Nb,Pc,Polyc,Clag,Nc)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c Changed:
c  To find the degree of a polynomial by searching the associated lags 
c    of the polynomial for the highest lag, by REG on 04 Feb 2004.
c-----------------------------------------------------------------------
c     Calculates the coefficients of the polynomial multiplication of
c a*b=c where a,b, and c have the form
c
c       1-a(1)*B**alag(1)-a(2)*B**alag(2)- ... -a(na)*B**alag(na).
c
c The method takes the outer product of the coefficients, ab' equal to
c         1      -a(1)     -a(2)   ...       -a(na)         c(1)...c(na)
c      -b(1)  a(1)b(1)  a(2)b(1)   ...    a(na)b(1)   c( na+1)  ...c(2na)
c      -b(2)  a(1)b(2)  a(2)b(2)   ...    a(na)b(2)   c(2na+1)  ...c(3na)
c        .       .         .        .         .     =        .
c        .       .         .        .         .              .
c        .       .         .        .         .              .
c     -b(nb) a(1)b(nb) a(2)b(nb)   ...   a(na)b(nb)   c(na*nb+na+1)...
c c(na*nb+na+nb), and an outer sum where clag(i,j)=a(i)+b(j),i=1,na,
c j=1,nb, giving a power or lag matrix.  But clag(i,j) is never
c calculated because the lags are sorted and the coefficients of like
c powers are summed.
c-----------------------------------------------------------------------
c var     type   description
c-----------------------------------------------------------------------
c polya  r  Input vector of polynomial coefficients
c alag   i  Input vector of the lags of the coefficients of a
c polyb  r  Input vector of polynomial coefficients
c blag   i  Input vector of the lags of the coefficients of b
c polyc  r  Output vector of polynomial coefficients is copied from
c            t at the end so that one of the input polynomials also
c            be the output polynomial
c clag   i  Output vector of the lags of the coefficients of c.  Also
c            assigned at the end
c i      i  Local do loop index
c j      i  Local do loop index
c lagb   i  Local current lag power of b
c lagt   i  Local current lag of the temporary polynomial which will
c            be c
c na     i  Input number of coefficients in the polynomial a
c nb     i  Input number of coefficients in the polynomial b
c nc     i  Output number of coefficients in the polynomial c
c nt     i  Local length of temporary vector of coefficients of c
c pc     i  'parameter' for the number of elements the c polynomial
c            can have
c pcoef  i  Local parameter for the length of the temporary polynomial
c            Must be larger than than nc which is greater than the
c             max((nd+1)(nsd+1)(nphi+1)-1,(nth+1)(nsth+1)-1)
c polyt     r  Local temporary vector for the coefficients of polynomial c
c tlag   i  Local temporary vector for the lags of the polynomial c
c tmp    r  Local temporary scalar for the current coefficient value of
c            c
c tmpb   r  Local temporary scalar for the current coefficient value of
c            b
c zero   d  Local double precision 0
c-----------------------------------------------------------------------
      INTEGER PCOEF,Pc
      PARAMETER(PCOEF=200)
      INTEGER Alag(*),Blag(*),Clag(Pc),i,j,tlag(PCOEF),lagb,lagt,Na,Nb,
     &        Nc,nt
      DOUBLE PRECISION Polya(*),Polyb(*),Polyc(Pc),polyt(PCOEF),tmpb,tmp
c-----------------------------------------------------------------------
c     Copy the a polynomial into the first row or elements of the c
c polynomial.
c-----------------------------------------------------------------------
      nt=0
      DO i=1,Na
       tmp=Polya(i)
       lagt=Alag(i)
       CALL insort(tmp,lagt,nt,polyt,tlag)
      END DO
c-----------------------------------------------------------------------
c     Then for the remaining rows, first, set the first element in the
c row to the coefficient value and lag of b then place it in c some the
c lags are still in order.
c-----------------------------------------------------------------------
      DO i=1,Nb
       tmpb=Polyb(i)
       lagb=Blag(i)
c     ------------------------------------------------------------------
       tmp=tmpb
       lagt=lagb
       CALL insort(tmp,lagt,nt,polyt,tlag)
c-----------------------------------------------------------------------
c     Set the rest of the row of c to the outer product or sum and add
c them to c in order of their lag powers. Note, to maintain the sign
c convention of the polynomial coefficients the sign of the outer
c product must be reversed.
c-----------------------------------------------------------------------
       DO j=1,Na
        tmp=-tmpb*Polya(j)
        lagt=lagb+Alag(j)
        CALL insort(tmp,lagt,nt,polyt,tlag)
       END DO
      END DO
c-----------------------------------------------------------------------
c     After the polynomial c and its lags have been calculated copy the
c nonzero lags of the temporary vector to c and the number of
c coefficients to nc.
c-----------------------------------------------------------------------
c      j=1
      DO i=1,nt
c       if(c(i).ne.zero)then
c        j=j+1
c        c(j)=Polyt(i)
c        clag(j)=tlag(i)
       Polyc(i)=polyt(i)
       Clag(i)=tlag(i)
c       endif
      END DO
c      nc=j
      Nc=nt
c     ------------------------------------------------------------------
      RETURN
      END
