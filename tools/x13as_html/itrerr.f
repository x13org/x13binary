C     Last change:  BCM  10 Feb 1999    4:06 pm
      SUBROUTINE itrerr(Errstr,Nestr,Lauto,Issap,Irev)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     This subroutine prints out an error message if the number of
c     iterations or function evaluations is too large.  
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'stdio.i'
c     ------------------------------------------------------------------
      CHARACTER Errstr*(20)
      LOGICAL Lauto,lparma
      INTEGER Issap,Irev,fh2,Nestr
c     ------------------------------------------------------------------
      fh2=0
      IF(.not.Lauto)fh2=Mt1
      lparma=F
c     ------------------------------------------------------------------
      IF(.not.Lauto)WRITE(Mt1,1010)Charhr
      WRITE(Mt2,1010)Charhr
      IF(Issap.eq.2)THEN
       IF(.not.Lauto)WRITE(STDERR,1020)Errstr(1:Nestr)
       CALL eWritln('Estimation failed to converge -- maximum '//
     &              Errstr(1:Nestr)//' reached',fh2,Mt2,T,F)
       CALL writln(' during sliding spans analysis.',fh2,Mt2,T,F)
      ELSE IF(Irev.eq.4)THEN
       IF(.not.Lauto)WRITE(STDERR,1030)Errstr(1:Nestr)
       CALL eWritln('Estimation failed to converge -- maximum '//
     &              Errstr(1:Nestr)//' reached',fh2,Mt2,T,F)
       CALL writln(' during history analysis.',fh2,Mt2,T,F)
      ELSE
       IF(.not.Lauto)WRITE(STDERR,1040)Errstr(1:Nestr)
       CALL eWritln('Estimation failed to converge -- maximum '//
     &              Errstr(1:Nestr)//' reached',fh2,Mt2,T,T)
      END IF
      IF(.not.Lauto.and.Issap.lt.2.and.Irev.lt.4)
     &   CALL mkPOneLine(Mt2,'@',
     &                   'Parameter values and log likelihood at '//
     &                   'last iteration follow.')
      CALL writln(' Rerun program trying one of the following:',
     &            fh2,Mt2,T,T)
      IF(.not.Lauto)WRITE(Mt1,1060)
      WRITE(Mt2,1060)
      IF(Lauto)THEN
       WRITE(Mt2,1070)
       CALL mkPOneLine(Mt2,'@',
     &                 'See '//MDLSEC//' of the '//PRGNAM//' '//
     &                 DOCNAM//' for more discussion.')
       WRITE(Mt2,1010)Charhr
      ELSE
       IF(Issap.eq.2.or.Irev.eq.4)THEN
        WRITE(Mt1,1080)
        WRITE(Mt2,1080)
       ELSE
        WRITE(Mt1,1090)'in the log file'
        WRITE(Mt2,1090)'below'
        lparma=T
       END IF
       WRITE(Mt1,1070)
       WRITE(Mt2,1070)
       CALL writln('See '//MDLSEC//' of the '//PRGNAM//' '//DOCNAM//
     &             ' for more discussion.',Mt1,Mt2,T,T)
       IF(lparma)CALL prARMA(Mt2,T)
       WRITE(Mt1,1010)Charhr
       WRITE(Mt2,1010)Charhr
      END IF
c     ------------------------------------------------------------------
 1010 FORMAT(/,a)
 1020 FORMAT(/,' ERROR: Estimation failed to converge -- maximum ',a,
     &         ' reached',/,'        during sliding spans analysis.')
 1030 FORMAT(/,' ERROR: Estimation failed to converge -- maximum ',a,
     &         ' reached',/,'        during history analysis.')
 1040 FORMAT(/,' ERROR: Estimation failed to converge -- maximum ',a,
     &         ' reached.')
 1060 FORMAT('<ol class="indent">',/,
     &       3x,'<li>Allow more iterations (set a larger value of ',
     &       'maxiter).</li>')
 1070 FORMAT(3x,'<li>Try a different model.</li>',/,'</ol>')
 1080 FORMAT(3x,'<li>Fix the values of the <abbr title="',
     &          'autoregressive moving average">ARMA</abbr> ',
     &          'coefficients to those obtained',/,
     &       7x,'while estimating the full series (set fixmdl=yes)',
     &          '</li>')
 1090 FORMAT(3x,'<li>Use initial values for <abbr title=',
     &          '"autoregressive moving average">ARMA</abbr> ',
     &          'parameters as given ',a,'.</li>')
c     ------------------------------------------------------------------
      RETURN
      END
