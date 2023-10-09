C     Last change:  BCM  19 May 2003    2:28 pm
      INTEGER FUNCTION mdssln(Sp)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Function that returns length of sliding span based on the value of
c     the first order seasonal moving term in ARIMA model -	from 
c     Findley (2003)
c     ------------------------------------------------------------------
      DOUBLE PRECISION getsma
      EXTERNAL getsma
c     ------------------------------------------------------------------
      DOUBLE PRECISION sma,smalim
      INTEGER i,Sp
      DIMENSION smalim(15)
c     ------------------------------------------------------------------
      DATA smalim /0.16D0,0.325D0,0.49D0,0.535D0,0.62D0,0.64D0,0.695D0,
     &             0.71D0,0.75D0,0.76D0,0.795D0,0.805D0,0.84D0,0.85D0,
     &             0.91D0/
c     ------------------------------------------------------------------
      sma=getsma()
      DO i=1,15
       IF(sma.lt.smalim(i))THEN
        mdssln=(i+3)*Sp
        RETURN
       END IF
      END DO
      mdssln=19*Sp
c     ------------------------------------------------------------------
      RETURN
      END
	  