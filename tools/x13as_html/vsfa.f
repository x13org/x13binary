C     Last change:  BCM   4 Sep 1998    1:42 pm
      SUBROUTINE vsfa(Stsi,Lfda,Llda,Nyr)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C --- SEASONAL FACTOR CURVE ROUTINE.
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11opt.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION cs,fis,fk,r1,r2,savg,simon,stimon,Stsi,tmp1,tmp2
      INTEGER i,j,k,kfda,ki,Lfda,Llda,m,n,Nyr
      DIMENSION Stsi(PLEN),savg(PYRS+6),simon(PYRS+6),stimon(PYRS+6)
C-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
      kfda=Lfda+Nyr-1
      Ratis=999.99D0
      r1=ZERO
      r2=ZERO
C-----------------------------------------------------------------------
C --- PLACE MONTHLY SI IN SIMON.
C-----------------------------------------------------------------------
      DO j=Lfda,kfda
       m=j-(j-1)/Nyr*Nyr
       k=3
       DO i=j,Llda,Nyr
        k=k+1
        simon(k)=Stsi(i)
       END DO
C-----------------------------------------------------------------------
C --- COMPUTE A 7-TERM MOVING AVERAGE FOR AN ESTIMATE OF S.
C-----------------------------------------------------------------------
       tmp1=(simon(4)+simon(5)+simon(6))/3.0D0
       tmp2=(simon(k)+simon(k-1)+simon(k-2))/3.0D0
       DO i=1,3
        ki=k+i
        simon(i)=tmp1
        simon(ki)=tmp2
       END DO
       CALL averag(simon,savg,1,ki,1,7)
       Rati(m)=ZERO
       Rati(m+Nyr)=ZERO
       Rati(m+2*Nyr)=999.99D0
C-----------------------------------------------------------------------
C --- DIVIDE SI/S FOR AN ESTIMATE OF I.
C-----------------------------------------------------------------------
       IF(Psuadd)THEN
        DO i=4,k
         stimon(i)=simon(i)-savg(i)+1D0
        END DO
       ELSE
        CALL divsub(stimon,simon,savg,4,k)
       END IF
C-----------------------------------------------------------------------
C --- ADJUST FOR THE LENGTH OF THE SERIES.
C-----------------------------------------------------------------------
       n=k-4
C-----------------------------------------------------------------------
C --- COMPUTE IBAR,SBAR, AN RATIOS.
C-----------------------------------------------------------------------
       IF(Muladd.lt.1)THEN
        DO i=5,k
         Rati(m)=Rati(m)+abs(stimon(i)-stimon(i-1))/stimon(i-1)
         Rati(m+Nyr)=Rati(m+Nyr)+abs(savg(i)-savg(i-1))/savg(i-1)
        END DO
        Rati(m)=Rati(m)*100D0*fis(cs,n)
        Rati(m+Nyr)=Rati(m+Nyr)*100D0*cs
       ELSE
        DO i=5,k
         Rati(m)=Rati(m)+abs(stimon(i)-stimon(i-1))*fis(cs,n)
         Rati(m+Nyr)=Rati(m+Nyr)+abs(savg(i)-savg(i-1))*cs
        END DO
       END IF
       r1=r1+Rati(m)
       r2=r2+Rati(m+Nyr)
c-----------------------------------------------------------------------
c     Change to handle series that are "STEP functions" BCM 10-97
c-----------------------------------------------------------------------
       IF(.NOT.dpeq(Rati(m+Nyr),ZERO))THEN
        IF(Rati(m).le.999D0*Rati(m+Nyr))
     &     Rati(m+Nyr*2)=Rati(m)/Rati(m+Nyr)
       END IF
c-----------------------------------------------------------------------
       fk=n
       Rati(m)=Rati(m)/fk
       Rati(m+Nyr)=Rati(m+Nyr)/fk
      END DO
      IF(r1.le.999D0*r2.AND.(.not.dpeq(r2,ZERO)))Ratis=r1/r2
      IF(Muladd.eq.2)THEN
       DO i=1,Nyr
        Rati(i)=100D0*Rati(i)
        Rati(i+Nyr)=100D0*Rati(i+Nyr)
       END DO
      END IF
      RETURN
      END
