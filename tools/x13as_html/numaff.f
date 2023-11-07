      INTEGER FUNCTION numaff(Betals,Muladd,Nterm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c   Determime how many SI ratios are effected by a level shift of a 
c   given magnitude (Betals is the coefficient in the log scale).
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ONEHND
      PARAMETER(ONE=1D0,ONEHND=100D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Betals,pctchg
      INTEGER i,itrend,numsi,Muladd,Nterm
      DOUBLE PRECISION limsi
      DIMENSION numsi(12,5),limsi(11)
c-----------------------------------------------------------------------
      DATA (numsi(i,1), i = 1,12) / 0,1,1,1,2,2,2,3,3,4,4,5 /
      DATA (numsi(i,2), i = 1,12) / 0,1,1,1,1,2,2,2,2,2,3,3 /
      DATA (numsi(i,3), i = 1,12) / 0,0,1,1,1,1,1,1,2,2,2,2 /
      DATA (numsi(i,4), i = 1,12) / 0,0,0,1,1,1,1,1,1,1,1,1 /
      DATA (numsi(i,5), i = 1,12) / 0,0,0,0,0,0,1,1,1,1,1,1 /
      DATA (limsi(i), i = 1,11) / 1.1D0,1.2D0,1.3D0,1.5D0,1.8D0,1.9D0, 
     &                            2.0D0,2.6D0,2.9D0,3.6D0,5.5D0 /
c-----------------------------------------------------------------------
      IF(Muladd.eq.1)THEN
       numaff=1
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Determine the precentage change in the level due to the level 
c     shift outlier.
c-----------------------------------------------------------------------
      pctchg = dabs((ONE/dexp(Betals)-ONE)*ONEHND)
c-----------------------------------------------------------------------
c     Set trend index
c-----------------------------------------------------------------------
      IF (Nterm.ge.23) THEN
       itrend=1
      ELSE IF (Nterm.ge.13) THEN
       itrend=2
      ELSE IF (Nterm.ge.9) THEN
       itrend=3
      ELSE IF (Nterm.ge.7) THEN
       itrend=4
      ELSE
       itrend=5
      END IF
c-----------------------------------------------------------------------
c     set number of observations effected based on percent change in 
c     level due to LS
c-----------------------------------------------------------------------
      DO i=1,11
       IF(pctchg.le.limsi(i))THEN
        numaff=numsi(i,itrend)
        RETURN
       END IF
      END DO
c-----------------------------------------------------------------------
      numaff=numsi(12,itrend)
      RETURN
      END
