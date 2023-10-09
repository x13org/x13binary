C     Last change:  BCM  17 Apr 2003   11:21 pm
**==sfmsr.f    processed by SPAG 4.03F  at 08:51 on 15 Sep 1994
      SUBROUTINE sfmsr(Sts,Stsi,Lfda,Llda,Lldaf,Lprt,Lsav)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     This subroutine implements the MSR seasonal filter selection
c     option from X-11-ARIMA/88
c     This implementation designed by BCM - January 1993
c     Added Sts, Stsi to calling arguments - July 2005
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'work2.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      CHARACTER*3 filter
      DOUBLE PRECISION Sts,Stsi
      LOGICAL Lprt,Lsav
      INTEGER Lfda,Llda,Lldaf,llda1,i
      DIMENSION filter(3),Sts(PLEN),Stsi(PLEN)
c     ------------------------------------------------------------------
      DATA filter/'3x3','3x5','3x9'/
c     ------------------------------------------------------------------
c     If MSR seasonal filter selection option is selected, calculate
c     a global MSR and try to select a seasonal filter length.
c     ------------------------------------------------------------------
      IF(Lterm.eq.6)THEN
       IF(Lmsr.eq.6)THEN
        IF(Lprt)THEN
         CALL writTagOneLine(Mt1,'h3','@',
     &                 'Final Seasonal Filter Selection via GLOBAL MSR')
        END IF
c     ------------------------------------------------------------------
c     set llda1 to the end of the last whole year
c     ------------------------------------------------------------------
        llda1=Llda-mod(Llda,Ny)
        i=1
        DO WHILE (Lterm.eq.6)
c     ------------------------------------------------------------------
c     If span to be tested is less than 5 years long, use a 3x5
c     seasonal filter.
c     ------------------------------------------------------------------
         IF((llda1-Lfda+1).lt.(5*Ny))THEN
          Lterm=2
          IF(Lprt)THEN
           IF(i.gt.1)CALL writTag(Mt1,'</p>')
           CALL mkPOneLine(Mt1,'center',
     &                     '***  Not enough data to continue, '//
     &                     '3x5 seasonal filter selected  ***')
          END IF
         ELSE
          CALL vsfa(Stsi,Lfda,llda1,Ny)
          IF(Lsav)WRITE(Nform,1010)i,Ratis
          IF(Ratis.le.2.5D0)THEN
           Lterm=1
           IF(Lprt)THEN
            IF(i.eq.1)CALL mkPClass(Mt1,'indent')
            WRITE(Mt1,1030)i,Ratis,filter(Lterm)
           END IF
          ELSE IF(Ratis.ge.6.5D0)THEN
           Lterm=3
           IF(Lprt)THEN
            IF(i.eq.1)CALL mkPClass(Mt1,'indent')
            WRITE(Mt1,1030)i,Ratis,filter(Lterm)
           END IF
          ELSE IF(Ratis.ge.3.5D0.and.Ratis.le.5.5D0)THEN
           Lterm=2
           IF(Lprt)THEN
            IF(i.eq.1)CALL mkPClass(Mt1,'indent')
            WRITE(Mt1,1030)i,Ratis,filter(Lterm)
           END IF
           L3x5=.true.
          ELSE
c     ------------------------------------------------------------------
c     If Global MSR meets none of the criteria, drop a year from the
c     end of the series and try again.
c     ------------------------------------------------------------------
           llda1=llda1-Ny
           IF(Lprt)THEN
            IF(i.eq.1)CALL mkPClass(Mt1,'indent')
            WRITE(Mt1,1040)i,Ratis,Cbr
           END IF
           i=i+1
          END IF
         END IF
        END DO
        DO i=1,Ny
         IF(Lter(i).eq.6)Lter(i)=Lterm
         IF(L3x5.and.(Lter(i).ne.0.and.Lter(i).ne.2))L3x5=.false.
        END DO
c     ------------------------------------------------------------------
c     Save seasonal filter length decision
c     ------------------------------------------------------------------
        IF(Lsav)WRITE(Nform,1050)filter(Lterm)
       ELSE
c     ------------------------------------------------------------------
c     If this is a sliding spans run, reset seasonal filter length to
c     the selection made for the entire series.
c     ------------------------------------------------------------------
        Lterm=Lmsr
        DO i=1,Ny
         IF(Lter(i).eq.6)Lter(i)=Lmsr
        END DO
       END IF
      END IF
c     ------------------------------------------------------------------
      CALL vsfa(Stsi,Lfda,Llda,Ny)
      CALL vsfb(Sts,Stsi,Lfda,Lldaf,Ny)
c     ------------------------------------------------------------------
      RETURN
c     ------------------------------------------------------------------
 1010 FORMAT('autosf.msr',i2.2,': ',f6.2)
 1030 FORMAT(' <strong>Pass No. ',i2,':</strong>  Global MSR = ',f6.2,
     &       ', <strong>',a3,' seasonal filter</strong> selected.</p>')
 1040 FORMAT(' <strong>Pass No. ',i2,':</strong>  Global MSR = ',f6.2,a)
 1050 FORMAT('sfmsr: ',a3)
c     ------------------------------------------------------------------
      END
