C     Last change:  BCM  29 Sep 97   10:13 am
      SUBROUTINE extend(Trnsrs,Begxy,Orix,Extok,Lam,Fcst,Bcst)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11opt.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ZERO
      PARAMETER(F=.false.,T=.true.,ZERO=0D0)
c     ------------------------------------------------------------------
      LOGICAL Extok
      INTEGER i,Begxy,fhnote
      DOUBLE PRECISION Orix,Trnsrs,Fcst,Bcst,Lam,bcst2
      DIMENSION Fcst(PFCST),Bcst(PFCST),Orix(PLEN),Trnsrs(PLEN),
     &          Begxy(2),bcst2(PFCST)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
c     calculate ending date of series
c     ------------------------------------------------------------------
      Extok=T
      fhnote=STDERR
      IF(Lquiet)fhnote=0
c-----------------------------------------------------------------------
c     If multiplicative SA and transformation not log, check forecasts
c     to see if they are negative.  If so, print warning message and 
c     do not perform forecast extension.
c-----------------------------------------------------------------------
      IF(Nfcst.gt.0)THEN
       IF((.not.dpeq(Lam,ZERO)).and.Muladd.ne.1)THEN
        i=1
        DO WHILE (i.le.Nfcst.and.Extok)
         IF(Psuadd.and.Fcst(i).lt.ZERO)THEN
          CALL wWritln('Forecast extension cannot be done for '//
     &                 'pseudo-additive seasonal',fhnote,Mt2,T,F)
          CALL writln('         adjustment due to negative values '//
     &                'found in forecasts.',fhnote,Mt2,F,T)
          Extok=F
         ELSE IF(Fcst(i).le.ZERO)THEN 
          CALL wWritln('Forecast extension cannot be done for '//
     &                 'multiplicative or log-',fhnote,Mt2,F,F)
          CALL writln('         additive seasonal adjustment due to '//
     &                'negative or zero values ',fhnote,Mt2,F,F)
          CALL writln('         found in forecasts.',STDERR,Mt2,F,T)
          Extok=F
         END IF
         i=i+1
        END DO
       END IF
      END IF
c-----------------------------------------------------------------------
c     If multiplicative SA and transformation not log, check forecasts
c     to see if they are negative.  If so, print warning message and 
c     do not perform forecast extension.
c-----------------------------------------------------------------------
      IF(Nbcst.gt.0.and.Extok)THEN
       IF((.not.dpeq(Lam,ZERO)).and.Muladd.ne.1)THEN
        i=1
        DO WHILE (i.le.Nbcst.and.Extok)
         IF(Psuadd.and.Bcst(i).lt.ZERO)THEN
          CALL wWritln('Backcast extension cannot be done for '//
     &                 'pseudo-additive seasonal',fhnote,Mt2,T,F)
          CALL writln('         adjustment due to negative values '//
     &                'found in backcasts.',fhnote,Mt2,F,T)
          Extok=F
         ELSE IF(Bcst(i).le.ZERO)THEN 
          CALL wWritln('Backcast extension cannot be done for '//
     &                 'multiplicative or log-',fhnote,Mt2,T,F)
          CALL writln('         additive seasonal adjustment due to '//
     &                'negative or zero values ',fhnote,Mt2,F,F)
          CALL writln('         found in backcasts.',STDERR,Mt2,F,F)
          Extok=F
         END IF
         i=i+1
        END DO
       END IF
      END IF
c-----------------------------------------------------------------------
c     copy transformed series to original vector
c-----------------------------------------------------------------------
      CALL copy(Trnsrs,Nspobs,1,Orix(Pos1ob))
c-----------------------------------------------------------------------
c   Append forecasts, backcasts to series
c-----------------------------------------------------------------------
      IF(.not.Extok)RETURN
      IF(Nfcst.gt.0)CALL copy(Fcst,Nfcst,1,Orix(Posfob+1))
c-----------------------------------------------------------------------
c     Append backcasts
c-----------------------------------------------------------------------
      IF(Nbcst.gt.0)THEN
c-----------------------------------------------------------------------
c     adjust Xy dates for backcasts
c-----------------------------------------------------------------------
       Begxy(YR)=Begbak(YR)
       Begxy(MO)=Begbak(MO)
c-----------------------------------------------------------------------
c     copy backcasts to beginning of series.
c-----------------------------------------------------------------------
       CALL revrse(Bcst,Nbcst,1,bcst2)
       CALL copy(bcst2,Nbcst,1,Orix(Pos1bk))
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
      END
