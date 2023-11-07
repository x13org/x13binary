C     Last change:  BCM  29 Sep 97    9:44 am
**==forcst.f    processed by SPAG 4.03F  at 17:25 on  7 Jun 1994
      SUBROUTINE forcst(Sts,Ib,Ie,Ke,Nyr,Iorder,Wt,R)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER i,Ib,Ie,Iorder,j,k,Ke,l,Nyr
      DOUBLE PRECISION R,Sts,w,Wt,ONE
C*** End of declarations inserted by SPAG
      PARAMETER (ONE=1D0)
C --- THIS SUBROUTINE FORECASTS SEASONALS FROM IE+1 TO KE
C --- IORDER REPRESENTS THE ORDER OF DIFFERENCES USED.
C --- WT IS THE WEIGHT GIVEN TO THE FORECASTED DIFFERENCE.
C --- THERE IS A RATIO OF R BETEEN SUCCESSIVE DIFFERENCES.
      LOGICAL dpeq
      EXTERNAL dpeq
      DIMENSION Sts(*)
      IF(.not.dpeq(R,ONE))THEN
       w=Wt*(R-ONE)/(R**Iorder-ONE)
      ELSE
       w=Wt
      END IF
      l=Ke-Ie
      DO i=1,l
       j=Ie+i
       Sts(j)=Sts(j-Nyr)
       DO k=1,Iorder
        Sts(j)=Sts(j)+w*(R**(Iorder-k))*(Sts(j-k*Nyr)-Sts(j-(k+1)*Nyr))
       END DO
      END DO
C --- THIS PART IS A BACKCAST FROM IB-1 TO IB-L.
      IF(Ib.gt.1)THEN
       DO i=1,l
        j=Ib-i
        Sts(j)=Sts(j+Nyr)
        DO k=1,Iorder
         Sts(j)=Sts(j)+w*(R**(Iorder-k))*(Sts(j+k*Nyr)-Sts(j+(k+1)*Nyr))
        END DO
       END DO
      END IF
      RETURN
      END
