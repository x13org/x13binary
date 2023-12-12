C     Last change:  BCM  26 Feb 1999    9:38 am
      SUBROUTINE ssphdr(Iagr,Ncol,Nlen,Ssfxrg,Nssfxr,Lyy,Lyy2,
     &                  Ssinit,Ssdiff,Lncset,Lnlset,Lprt,Lsav)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C  *****  PRINTS HEADING THAT IDENTIFIES WHICH OPTIONS ARE BEING USED
C  *****  IN A GIVEN SLIDING SPANS ANALYSIS.
c-----------------------------------------------------------------------
      INTEGER ZERO,MINUS1,MINUS2
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.,ZERO=0,MINUS1=-1,MINUS2=-2)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
c     correct dimension length for Ssfxrg (BCM May 2007)
      CHARACTER setstr*(13)
      INTEGER Ssfxrg,Nssfxr,Ncol,Nlen,i,Iagr,Ssinit
      LOGICAL Lyy,Lyy2,Lprt,Lsav,Lncset,Lnlset,Ssdiff
      DIMENSION Ssfxrg(4)
c----------------------------------------------------------------------
c     Save sliding spans information for diagnostic program
c----------------------------------------------------------------------
      IF(Lsav)THEN
       WRITE(Nform,1030)'sspans','yes'
       WRITE(Nform,1010)Ncol,Nlen,Im,Iyr
       WRITE(Nform,1020)(Cut(i,1),i=1,5)
       IF(Itd.eq.1)THEN
        WRITE(Nform,1030)'sstd','yes'
       ELSE
        WRITE(Nform,1030)'sstd','no'
       END IF
       IF(Ssdiff)THEN
        WRITE(Nform,1030)'ssdiff','yes'
       ELSE
        WRITE(Nform,1030)'ssdiff','no'
       END IF
      END IF
      IF(.not.Lprt)RETURN
c     ------------------------------------------------------------------
c     Generate entries for index, skip links
c     ------------------------------------------------------------------
      IF(Iagr.lt.5)CALL genSkip(1079)
      IF(Iagr.eq.5)CALL genSkip(1080)
c-----------------------------------------------------------------------
      IF(Iagr.lt.5)
     &   CALL writTagOneLine(Mt1,'h2','@','Sliding spans analysis')
      IF(Iagr.eq.5)
     &   CALL writTagOneLine(Mt1,'h2','@',
     &            'Sliding spans analysis: Direct seasonal adjustment ')
      CALL writTagOneLine(Mt1,'h3','@',
     &                  'S 0. Summary of options selected for this run')
      CALL mkPClass(Mt1,'indent')
c-----------------------------------------------------------------------
      IF(Lncset)THEN
       setstr = '(set by user)'
      ELSE
       setstr = '             '
      END IF
      WRITE(Mt1,1070)'Number',Ncol,setstr,Cbr
      IF(Lnlset)THEN
       setstr = '(set by user)'
      ELSE
       setstr = '             '
      END IF
      WRITE(Mt1,1070)'Length',Nlen,setstr,Cbr
c----------------------------------------------------------------------
      IF(Nsea.eq.12)THEN
       WRITE(Mt1,1080)'Month',Im,Cbr,Iyr,Cbr
       IF(Ic.gt.(Im+Nsea))WRITE(Mt1,1090)'Month',Icm,Cbr,Icyr,Cbr
      ELSE IF(Nsea.eq.4)THEN
       WRITE(Mt1,1080)'Quarter',Im,Cbr,Iyr,Cbr
       IF(Ic.gt.(Im+Nsea))WRITE(Mt1,1090)'Quarter',Icm,Cbr,Icyr,Cbr
      END IF
c----------------------------------------------------------------------
      IF(Itd.eq.1)
     &   CALL mkPOneLine(Mt1,'indent','Trading day factors analyzed')
      IF((Itd.eq.1.or.Ihol.eq.1.or.Muladd.eq.1).and.Iyrt.gt.0)
     &   CALL mkPOneLine(Mt1,'indent','Seasonally adjusted series '//
     &                   'with revised yearly totals used in this '//
     &                   'analysis.')
c-----------------------------------------------------------------------
      CALL mkPClass(Mt1,'indent')
      IF(Lyy.and.Lyy2)THEN
       WRITE(Mt1,1050)'Year-to-year changes analyzed for direct and '//
     &                'indirect seasonal adjustments.',Cbr
      ELSE IF(Lyy2) THEN
       WRITE(Mt1,1050)'Year-to-year changes analyzed for '//
     &                'indirect seasonal adjustments only.',Cbr
      ELSE IF(Lyy.and.Iagr.eq.5) THEN
       WRITE(Mt1,1050)'Year-to-year changes analyzed for '//
     &                'direct seasonal adjustments only.',Cbr
      ELSE IF(Lyy) THEN
       WRITE(Mt1,1050)'Year-to-year changes analyzed.',Cbr
      END IF
      WRITE(Mt1,1050)'Name of series being adjusted : '//Serno(1:Nser),
     &               '</p>'
c----------------------------------------------------------------------
      IF(Ssinit.eq.1)THEN
       CALL mkPOneLine(Mt1,'indent','regARIMA model coefficients '//
     &                 'held fixed during sliding spans analysis.')
      ELSE IF(Nssfxr.gt.0)THEN
       CALL mkPOneLine(Mt1,'indent','Regressors held fixed during '//
     &                 'sliding spans analysis:')
       CALL writTagClass(Mt1,'ul','indent')
       DO i=1,Nssfxr
        IF(Ssfxrg(i).eq.1)THEN
         WRITE(Mt1,1141)'Trading Day'
        ELSE IF(Ssfxrg(i).eq.2)THEN
         WRITE(Mt1,1141)'Holiday'
        ELSE IF(Ssfxrg(i).eq.3)THEN
         WRITE(Mt1,1141)'User-defined regressors'
        ELSE IF(Ssfxrg(i).eq.4)THEN
         WRITE(Mt1,1141)'Outliers'
        END IF
       END DO
       CALL writTag(Mt1,'</ul>')
      END IF
c----------------------------------------------------------------------
      IF(Ncol.lt.4)THEN
       IF(Lncset)THEN
        WRITE(Mt1,1130)'By choice of the user'
       ELSE
        WRITE(Mt1,1130)'Due to the series length'
       END IF
      END IF
c----------------------------------------------------------------------
      IF(Itd.EQ.MINUS1.and.Ihol.le.ZERO)THEN
       CALL nWritln('Since the trading day coefficients are fixed '//
     &       'in the sliding spans',Mt1,Mt2,T,F)
       CALL writln(' analysis, the trading day statistics of the '//
     &       'sliding spans analysis',Mt1,Mt2,F,F)
       CALL writln(' are not printed.',Mt1,Mt2,F,T)
       WRITE(Mt1,2000)
       WRITE(Mt2,2000)
      ELSE IF(Ihol.EQ.MINUS1.and.Itd.le.ZERO)THEN
       CALL nWritln('Since the holiday coefficients are fixed in '//
     &              'the sliding spans analysis,',Mt1,Mt2,T,F)
       CALL writln(' the spans statistics for the seasonally '//
     &             'adjusted series have',Mt1,Mt2,F,F)
       CALL writln(' the same values as the corresponding '//
     &             'statistics for the seasonal factors.',Mt1,Mt2,F,T)
       WRITE(Mt1,2001)
       WRITE(Mt2,2001)
      END IF
      IF(Itd.eq.MINUS2.and.IHOL.eq.MINUS2)THEN
       CALL eWritln('Length of sliding span is too short for '//
     &              'trading day and holiday estimation',Mt1,Mt2,T,T)
       WRITE(Mt1,2002)
       WRITE(Mt2,2002)
      ELSE IF(Itd.eq.MINUS2)THEN
       CALL eWritln('Length of sliding span is too short for '//
     &              'trading day estimation',Mt1,Mt2,T,T)
       WRITE(Mt1,2002)
       WRITE(Mt2,2002)
      ELSE IF(Ihol.eq.MINUS2)THEN
       CALL eWritln('Length of sliding span is too short for '//
     &              'holiday estimation',Mt1,Mt2,T,T)
       WRITE(Mt1,2002)
       WRITE(Mt2,2002)
      END IF
c----------------------------------------------------------------------
 1010 FORMAT('ssa: ',4I5)
 1020 FORMAT('sscut: ',5F7.2)
 1030 FORMAT(a,': ',a)
 1050 FORMAT(a,a)
 1070 FORMAT(a,' of spans : ',i5,3x,a,a)
 1080 FORMAT(a,' of first observation in first span : ',i5,a,/,
     &       '  Year of first observation in first span : ',i5,a)
 1090 FORMAT(a,' of first observation used in sliding spans ',
     &       'comparison : ',i5,a,/,
     &       '  Year of first observation used in sliding spans ',
     &       'comparison : ',i5,a)
 1130 FORMAT(/,'<p><strong>WARNING:</strong> ',a,
     &         ', fewer than four spans have been used',
     &       /,' to compile the measures generated below.</p>',//,
     &       '<p>In this situation, the threshold values used to ',
     &       'determine',/,
     &       ' adjustability (15%, 25%, 40%) which appear with ',
     &       'the summary',/,
     &       ' tables should be lowered.</p>')
 1141 FORMAT('<li>',a,'</li>')
 2000 FORMAT(/,'<p>In addition, the spans statistics for the ',
     &       'seasonally adjusted',/,
     &       ' series have the same values as the ',
     &       'corresponding statistics',/,
     &       ' for the seasonal factors.  In this case, the ',
     &       'statistics for the',/,
     &       ' seasonally adjusted series are not printed.</p>')
 2001 FORMAT('<p>In this case, the statistics for the ',/,
     &       '   seasonally adjusted  series are not printed.</p>',/)
 2002 FORMAT(/,'<p>At least five years of data are needed.</p>')
      RETURN
      END
