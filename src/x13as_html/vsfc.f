      SUBROUTINE vsfc(Sts,Lfda,Llda,Nyr,Lter)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     Center seasonal factors by applying a 2 x Nyr moving average to
c     them - made a subroutine to enable it to be applied to combined
c     seasonal factor from X-11 seasonal and user seasonal effects
c     BCM June 2003
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
C-----------------------------------------------------------------------
      DOUBLE PRECISION Sts,Temp
      INTEGER i,k,k1,kfda,klda,Lfda,Llda,Nyr,Lter
      DIMENSION Lter(PSP),Sts(PLEN),Temp(PLEN)
C-----------------------------------------------------------------------
      COMMON /work  / Temp
C-----------------------------------------------------------------------
C --- APPLY A 2 X NYR MOVING AVERAGE TO THE SEASONALS.
C-----------------------------------------------------------------------
      CALL averag(Sts,Temp,Lfda,Llda,2,Nyr)
      k=Nyr/2
      kfda=Lfda+k
      klda=Llda-k
C-----------------------------------------------------------------------
C --- FILL IN THE MISSING END TERMS BY REPEATING FIRST AND LAST
C --- AVAILABLE MOVING AVERAGE VALUE.
C-----------------------------------------------------------------------
*      IF(allstb)THEN
      k1=mod(kfda,Nyr)
      DO i=1,k
       k1=k1-1
       IF(k1.le.0)k1=Nyr+k1
       IF(Lter(k1).eq.5)THEN
        Temp(kfda-i)=Temp(kfda-i+Nyr)
       ELSE
        Temp(kfda-i)=Temp(kfda)
       END IF
      END DO
      k1=mod(klda,Nyr)
      DO i=1,k
       k1=k1+1
       IF(k1.gt.Nyr)k1=1
       IF(Lter(k1).eq.5)THEN
        Temp(klda+i)=Temp(klda+i-Nyr)
       ELSE
        Temp(klda+i)=Temp(klda)
       END IF
      END DO
C-----------------------------------------------------------------------
C --- DIVIDE SEASONALS BY THE 2 X NYR MOVING AVERAGE
C-----------------------------------------------------------------------
      CALL divsub(Sts,Sts,Temp,Lfda,Llda)
C-----------------------------------------------------------------------
      RETURN
      END
