C     Last change:  BCM  17 Apr 2003   11:27 pm
      SUBROUTINE vtc(Stc,Stci)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- VARIABLE TREND CYCLE ROUTINE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'x11opt.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0)
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      LOGICAL lsame
      DOUBLE PRECISION apcc,apci,r,Stc,Stci,Temp
      INTEGER i,ib,ie,ie1
      DIMENSION Temp(PLEN),Stc(PLEN),Stci(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      COMMON /work  / Temp
c-----------------------------------------------------------------------
C --- IF THE SERIES IS MONTHLY APPLY A 13-TERM HENDERSON. IF IT IS
C --- QUARTERLY APPLY A 5-TERM HENDERSON.
c-----------------------------------------------------------------------
      lsame=F
      Nterm=Ny+1
      CALL hndtrn(Stc,Stci,Pos1bk,Posffc,Nterm,Tic,F,lsame)
c-----------------------------------------------------------------------
C --- DROP END  TERMS AND CALCULATE IRREGULAR SERIES.
c-----------------------------------------------------------------------
      ib=Pos1bk+Nterm/2
      ie=Posffc-Nterm/2
      CALL divsub(Temp,Stci,Stc,ib,ie)
c-----------------------------------------------------------------------
C --- CALCULATE IBAR/CBAR RATIO.
c-----------------------------------------------------------------------
      ie1=Posfob-Nterm/2-1
      apcc=ZERO
      apci=ZERO
      IF(Muladd.eq.0)THEN
       DO i=ib,ie1
        apcc=apcc+abs(Stc(i+1)-Stc(i))/Stc(i)
        apci=apci+abs(Temp(i+1)-Temp(i))/Temp(i)
       END DO
      ELSE
       DO i=ib,ie1
        apcc=apcc+abs(Stc(i+1)-Stc(i))
        apci=apci+abs(Temp(i+1)-Temp(i))
       END DO
      END IF
      IF(dpeq(apcc,ZERO))THEN 
       Ratic=999D0
       r=Ratic
      ELSE
       Ratic=apci/apcc
       r=Ratic*12/Ny
      END IF
c-----------------------------------------------------------------------
C --- CHECK IF TREND CYCLE MOVING AVERAGE PRESELECTED.
c-----------------------------------------------------------------------
      IF(Ktcopt.le.0)THEN
       IF((Kpart.eq.2.and.r.ge.ONE).or.(r.ge.ONE.and.r.lt.3.5D0))THEN
        lsame=T
       ELSE IF(r.lt.ONE)THEN
        IF(Ny.eq.12)THEN
         Nterm=9
         Tic=ONE
        END IF
       ELSE
        Tic=4.5D0
        Nterm=23
        IF(Ny.eq.4)Nterm=7
       END IF
      ELSE IF(Ktcopt.eq.Nterm)THEN
       lsame=T
      ELSE
       Nterm=Ktcopt
      END IF
c-----------------------------------------------------------------------
c     Generate and apply symmetric henderson filter and end weights
c-----------------------------------------------------------------------
*   10 CALL hndtrn(Stc,Stci,Pos1bk,Posffc,Nterm,Tic,T,lsame)
      CALL hndtrn(Stc,Stci,Pos1bk,Posffc,Nterm,Tic,T,lsame)
      RETURN
      END
