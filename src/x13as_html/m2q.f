      SUBROUTINE m2q(Y,Yq,N1,N2,N1q,N2q,Start,Startq,Isrflw)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c   subroutine that converts a monthly series to a quarterly series
c-----------------------------------------------------------------------
      INTEGER YR,MO
      PARAMETER(YR=1,MO=2)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Y,Yq
      INTEGER N1,N2,N1q,N2q,Start,Startq,Isrflw,i,iq,iq2,im,nq
      DIMENSION Y(PLEN),Yq(PLEN),Start(2),Startq(2)
c-----------------------------------------------------------------------
      Startq(YR)=Start(YR)
      IF(Start(MO).eq.1)THEN
c       iq=1
       im=N1
       Startq(MO)=1
      ELSE IF(Start(MO).le.4)THEN
c       iq=2
       im=N1+(4-Start(MO))
       Startq(MO)=2
      ELSE IF(Start(MO).le.7)THEN
c       iq=3
       im=N1+(7-Start(MO))
       Startq(MO)=3
      ELSE IF(Start(MO).le.10)THEN
c       iq=4
       im=N1+(10-Start(MO))
       Startq(MO)=4
      ELSE
c       iq=5
       im=N1+(13-Start(MO))
       Startq(MO)=1
       Startq(YR)=Startq(YR)+1
      END IF
c-----------------------------------------------------------------------
      nq=0
      iq = 1
      DO i=im,N2,3
c-----------------------------------------------------------------------
c-----update on 6/27, truncate the months at the end of span which is
c-----full quarter.
c-----------------------------------------------------------------------
       IF (i+2.le.N2) THEN
         nq=nq+1
         iq2=iq+nq-1
         Yq(iq2)=Y(i+2)
         IF(Isrflw.le.1)THEN
           Yq(iq2)=Yq(iq2)+Y(i)
           Yq(iq2)=Yq(iq2)+Y(i+1)
         END IF
        END IF
      END DO
c-----------------------------------------------------------------------
      N1q=iq
      N2q=iq2
c-----------------------------------------------------------------------
      RETURN
      END
      
