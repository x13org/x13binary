C     Last change:  BCM  26 Jan 98    1:10 pm
      SUBROUTINE olsreg(Xy,Nrxy,Ncxy,Pcxy,B,Chlxpx,Pxpx,Info)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Does ordinary least squares regression by forming the  normal
c equations and solving them using a Cholesky decomposition.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c b       d  Output nc long output vector of regression estimates
c i       i  Local do loop index
c lerr    l  Local error for X'X not positive definite (.true.).  Used in the
c             cholesky decomposition return
c nb      i  Local number of b elements and the number of columns in the
c             X matrix
c ncxy    i  Input number of columns in the X:y matrix and rows in the b
c             vector
c nrxy    i  Input number of rows in the X:y matrix
c pxpx    i  Input PARAMETER for the maximum number of elements in
c             [X:y]'[X:y]
c pcxy    i  Input PARAMETER for the polumn dimension of  [X:y].  Note
c             the data is always in the pcxyth column
c xelt    i  Local index for the current element in [X:y]'[X:y].
c xy      d  Input nr by ncxy matrix of regression variables and data
c             in last, pcxyth, column of [X:y]
c chlxpx  d  Ouput pxpx array long for the Cholesky decomposition of
c             the X'X matrix will be in the first (ncxy-1)ncxy/2 elements,
c             the z=chol(X'X)b in the (ncxy-1)ncxy/2+1 to ncxy(ncxy+1)/2-1,
c             and the square root of the residual sum of squares in the
c             ncxy(ncxy+1)/2th element.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      CHARACTER cstr1*(6),cstr2*(6)
      INTEGER i,Info,nb,Nrxy,Ncxy,xelt,Pcxy,Pxpx,nstr1,nstr2
      DOUBLE PRECISION B,Chlxpx,Xy
      DIMENSION B(Ncxy-1),Chlxpx(Pxpx),Xy(Pcxy,Nrxy)
c-----------------------------------------------------------------------
c     Check that the packed chlxpx is large enough to handle [X:y]'[X:y]
c-----------------------------------------------------------------------
      IF(Ncxy*(Ncxy+1)/2.gt.Pxpx)THEN
       CALL itoc(Ncxy,cstr1,nstr1)
       IF(.not.Lfatal)CALL itoc(Pxpx,cstr2,nstr2)
       IF(Lfatal)RETURN
       CALL errhdr
       CALL writln('Elements needed for [X:y]''[X:y] = '//
     &             cstr1(1:(nstr1-1))//' *('//cstr1(1:(nstr1-1))//
     &             '+1)/2 >'//cstr2(1:(nstr2-1))//'.',STDERR,Mt2,T,T)
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Form X'X and X'y (b) of the normal equations by forming [X:y]'[X:y]
c-----------------------------------------------------------------------
      CALL xprmx(Xy,Nrxy,Ncxy,Pcxy,Chlxpx)
c-----------------------------------------------------------------------
c     Find the Cholesky decomposition of [X:y]'[X:y] and solve the normal
c equations.  DPOFA does the factorization so
c                 [L' z        ]
c                 [0  sqrt(RRS)]
c-----------------------------------------------------------------------
      CALL dppfa(Chlxpx,Ncxy,Info)
      IF(Info.le.0.or.Info.eq.Ncxy)THEN
c-----------------------------------------------------------------------
c     Betas are L'b=z so solve the upper triangular system for them
c-----------------------------------------------------------------------
       nb=Ncxy-1
       xelt=nb*Ncxy/2
       CALL copy(Chlxpx(xelt+1),nb,1,B)
c     ------------------------------------------------------------------
       DO i=nb,1,-1
        B(i)=B(i)/Chlxpx(xelt)
        xelt=xelt-i
        CALL daxpy(i-1,-B(i),Chlxpx(xelt+1),1,B,1)
       END DO
c-----------------------------------------------------------------------
c     Info is reset in the case of a y is linearly dependant on X
c-----------------------------------------------------------------------
       Info=0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
