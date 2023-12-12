C     Last change:  BCM  26 Jan 98    1:20 pm
**==resid.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE resid(Xy,Nr,Nc,Pc,Begcol,Endcol,Fac,B,Rsd)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Returns the residuals from a regression model,  rsd=y+sign(fac)*Xb
c where X and y are the extended matrix [X:y] and the data vector y is
c in the pcth column so if pc=nc then there can be only nc-1 regression
c effects.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c addsub   i  Local factor to add or subtract the regression residuals
c              thus calculating y-Xb or a+Xb
c b        d  Input min(nc,pc-1) long vector of regression parameter
c              estimates
c begcol   i  Input begining column of X used to calculate the residuals
c endcol   i  Input end column of X used to calculate the residuals
c fac      d  Input scalar whose sign determines weather the regression
c              effects are added or subtracted from the data.
c i        i  Local do loop index
c j        i  Local do loop index
c nc       i  Input number for columns used in [X:y]
c nr       i  Input number of rows in both  X and y
c one      d  Local PARAMETER of a double precision 1
c pc       i  Input PARAMETER for the leading array (column) index of [X:y]
c rsd      d  Output nr long vector of residuals
c sum      d  Local inner product of x(i,.)*b(.)
c xy       d  Input nr by nc [X:y] matrix with the data vector y in the
c              pcth column which may not be nc+1
c zo       d  Local PARAMETER for a double precision 0
c-----------------------------------------------------------------------
c     Data typing and initialization
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE
      LOGICAL T,F
      PARAMETER(ONE=1D0,T=.true.,F=.false.)
      INTEGER Pc
c-----------------------------------------------------------------------
      INTEGER Begcol,Endcol,icol,Nc,Nr
      DOUBLE PRECISION addsub,B,Fac,Rsd,Xy
      DIMENSION B(*),Rsd(Nr),Xy(Pc*Nr)
c-----------------------------------------------------------------------
c     First check that the begining and ending columns are between 1
c and nc.
c-----------------------------------------------------------------------
      IF(Nc.eq.0.or.Endcol+1.eq.Begcol)THEN
       CALL dcopy(Nr,Xy(Pc),Pc,Rsd,1)
      ELSE IF(Begcol.lt.1.or.Endcol.gt.Nc.or.Endcol.lt.Begcol)THEN
       CALL eWritln('Column, 1<=begcol<=endcol<=    nb',
     &              STDERR,Mt2,T,F)
       CALL writTag(Mt2,Cbr)
       WRITE(STDERR,1010)Begcol,Endcol,Nc-1
       WRITE(Mt2,1010)Begcol,Endcol,Nc-1
       CALL writTag(Mt2,'</p>')
 1010  FORMAT(26x,3I8)
       CALL abend
       RETURN
c-----------------------------------------------------------------------
c     Make addsub from fac
c-----------------------------------------------------------------------
      ELSE
       addsub=sign(ONE,Fac)
       CALL dcopy(Nr,Xy(Pc),Pc,Rsd,1)
c-----------------------------------------------------------------------
c     Calculate the residuals
c-----------------------------------------------------------------------
       DO icol=Begcol,Endcol
        CALL daxpy(Nr,addsub*B(icol),Xy(icol),Pc,Rsd,1)
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
