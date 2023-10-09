C     Last change:  BCM  15 Jan 2008    12:40 pm
      SUBROUTINE ar30rg(YY,Nrxy,Maxar,Bar,Var)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c    Performs AR 30 using OLS regression on series YY for AR-spectrum
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      LOGICAL F
      INTEGER PMXAR
      DOUBLE PRECISION TWO,ZERO,MONE
      PARAMETER(PMXAR=30,TWO=2D0,ZERO=0D0,MONE=-1D0,F=.false.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION YY,ymat,txy,tchxpx,Bar,Var,apa,ta,amat,ainv,atmp,
     &                 bmat,btmp,zmat
      INTEGER Maxar,Nrxy,info,i,i1,j,j1,j2,mx1,na,nb,ny,nz,ntrxy
      LOGICAL Good
c-----------------------------------------------------------------------
      DIMENSION YY(*),Bar(*),ta(PLEN),amat(Maxar,Maxar),
     &          ainv(Maxar,Maxar),atmp(Maxar,Maxar),na(2),bmat(Maxar,1),
     &          btmp(Maxar,1),nb(2),ymat(Maxar,1),ny(2),zmat(1,1),nz(2)
c-----------------------------------------------------------------------
      DOUBLE PRECISION dpmpar
      EXTERNAL dpmpar
c-----------------------------------------------------------------------
c     Initialize matrices      
c-----------------------------------------------------------------------
      do i=1,Maxar
       do j=1,Maxar
        amat(i,j)=ZERO
       end do
       bmat(i,1)=ZERO
      end do
c-----------------------------------------------------------------------
c    Set dimensions for y,z matrix
c-----------------------------------------------------------------------
      ny(1)=Maxar
      ny(2)=1
      nz(1)=1
      nz(2)=1
c-----------------------------------------------------------------------
c    Form A matrix from data
c-----------------------------------------------------------------------
      mx1=Maxar+1
      DO i=mx1,Nrxy
       i1=1
       j1=i-1
       j2=i-Maxar
       DO j=j1,j2,-1
        ymat(i1,1)=YY(j)
        i1=i1+1
       END DO
       CALL mulMatTr(ymat,ny,ymat,ny,atmp,na)
       CALL addMat(amat,na,atmp,na,amat,na)
      END DO
c-----------------------------------------------------------------------
c     invert A matrix
c-----------------------------------------------------------------------
      CALL invMat(amat,na,ainv,na)
c-----------------------------------------------------------------------
c    Form B matrix from data
c-----------------------------------------------------------------------
      DO i=mx1,Nrxy
       i1=1
       j1=i-1
       j2=i-Maxar
       DO j=j1,j2,-1
        ymat(i1,1)=YY(j)
        i1=i1+1
       END DO
       zmat(1,1)=YY(i)
       CALL mulMat(ymat,ny,zmat,nz,btmp,nb)
       CALL addMat(bmat,nb,btmp,nb,bmat,nb)
      END DO
c-----------------------------------------------------------------------
c     generate betas, copy into Bar.
c-----------------------------------------------------------------------
      CALL mulMat(ainv,na,bmat,nb,btmp,nb)
      do j=1,Maxar
       Bar(j)=btmp(j,1)
      end do
c-----------------------------------------------------------------------
c     generate variance by first forming residuals
c     then summing their squares
c-----------------------------------------------------------------------
      DO i=mx1,Nrxy
       i1=i-mx1+1
       ta(i1)=YY(i)
       do j=1,Maxar
        ta(i1)=ta(i1)-(YY(i-j)*Bar(j))
       END DO
      END DO
      ntrxy=Nrxy-Maxar
      CALL yprmy(ta,ntrxy,apa)
c-----------------------------------------------------------------------
c     Calculate the variance
c-----------------------------------------------------------------------
      Var=apa/dble(ntrxy-1)
      IF(Var.lt.TWO*dpmpar(1))Var=ZERO
c-----------------------------------------------------------------------
      RETURN
      END