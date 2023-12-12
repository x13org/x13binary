      SUBROUTINE svchsd(Chvec,Ib,Ie,Iagr,Muladd,Chlab)
c-----------------------------------------------------------------------
c     saves standard deviation of the month-to-month change vector CHVEC
c     (from IB to IE) to the seasonal adjustment diagnostics in the udg
c     file (FH representing the file handle).  Chlab is the label used
c     to denote which change vector is being saved.
c     Written by BCM (July 2007)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONEHND
      PARAMETER(ONEHND=100D0)
c-----------------------------------------------------------------------
      CHARACTER Chlab*(*)
      DOUBLE PRECISION Chvec(PLEN),chsd,tempch(PLEN)
      INTEGER i,Iagr,Muladd,Ib,Ie
c-----------------------------------------------------------------------
      DOUBLE PRECISION sdev
      EXTERNAL sdev
c-----------------------------------------------------------------------
c     copy change vector into tempch, multiply by 100 if necessary
c-----------------------------------------------------------------------
      CALL copy(Chvec,Ie,1,tempch)
      IF(Muladd.ne.1)THEN
       DO i=Ib,Ie
        tempch(i)=tempch(i)*ONEHND
       END DO
      END IF
c-----------------------------------------------------------------------
c     generate standard deviation of change vector tempch
c-----------------------------------------------------------------------
      chsd = sdev(tempch,Ib,Ie,1,1)
c-----------------------------------------------------------------------|
      IF(Iagr.eq.4)THEN
       WRITE(Nform,1010)'chsd.i',Chlab,chsd
      ELSE
       WRITE(Nform,1010)'chsd.',Chlab,chsd
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(a,a,': ',e21.14)
      END
      