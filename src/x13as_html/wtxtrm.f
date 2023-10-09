C     Last change:  BCM  25 Nov 97   10:46 am
**==wtxtrm.f    processed by SPAG 4.03F  at 17:00 on 16 May 1994
      DOUBLE PRECISION FUNCTION wtxtrm(X,Xbar,Stddev,Sigmu,Sigml,Istep,
     &                                 Lstwt)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      DOUBLE PRECISION MONE,ZERO
      PARAMETER(MONE=-1D0,ZERO=0D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION X,Xbar,Stddev,Sigmu,Sigml,temp,Lstwt
      INTEGER Istep
c-----------------------------------------------------------------------
c     Initialize extreme weight
c-----------------------------------------------------------------------
      wtxtrm=Lstwt
c-----------------------------------------------------------------------
C --- COMPUTE DEVIATION OF EACH IRREGULAR VALUE.
c-----------------------------------------------------------------------
      temp=abs(X-Xbar)/Stddev
      IF(temp.le.Sigmu)THEN
c-----------------------------------------------------------------------
C --- ASSIGN GRADUATED WEIGHT BETWEEN THE LIMITS.
c-----------------------------------------------------------------------
       IF(temp.gt.Sigml.and.Istep.ne.1)wtxtrm=(Sigmu-temp)/(Sigmu-Sigml)
      ELSE IF(Istep.eq.1)THEN
c-----------------------------------------------------------------------
C --- IN THE FIRST ITERATION ASSIGN A WEIGHT OF 0.0 TO ALL EXTREMES.
c-----------------------------------------------------------------------
       wtxtrm=ZERO
      ELSE IF(Lstwt.gt.ZERO)THEN
c-----------------------------------------------------------------------
C --- IN THE SECOND ITERATION ASSIGN A TEMPORARY WEIGHT OF -1.0 TO
C --- ALL EXTREMES.
c-----------------------------------------------------------------------
       wtxtrm=MONE
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
 
