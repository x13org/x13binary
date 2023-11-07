      SUBROUTINE savtpk(Iagr,Lsumm,Cstuk,Cttuk,Cstk90,Cttk90,
     &                  Cstuki,Cttuki,Csti90,Ctti90)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     save spectral peak information in log file and/or .xdg/.mdg file
c     ------------------------------------------------------------------
      INCLUDE 'htmlout.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'spcsvl.i'
c     ------------------------------------------------------------------
      CHARACTER Cstuk*(35),Cttuk*(35),Cstk90*(35),Cttk90*(35),
     &          Cstuki*(35),Cttuki*(35),Csti90*(35),Ctti90*(35)
      INTEGER Iagr,Lsumm,nstuk,nttuk,nstk90,nttk90,
     &                 nstuki,nttuki,nsti90,ntti90
c     ------------------------------------------------------------------
      INTEGER nblank
      LOGICAL istrue
      EXTERNAL istrue,nblank
c-----------------------------------------------------------------------
      nstuk=nblank(Cstuk)
      nttuk=nblank(Cttuk)
      nstk90=nblank(Cstk90)
      nttk90=nblank(Cttk90)
      IF(Iagr.gt.3)THEN
       nstuki=nblank(Cstuki)
       nttuki=nblank(Cttuki)
       nsti90=nblank(Csti90)
       ntti90=nblank(Ctti90)
      END IF
c     ------------------------------------------------------------------
      IF(istrue(Svltab,LSLTPK,LSLITP))THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1000)Inlgfl
       CALL mkTableTag(Ng,'w60','Summary of Tukey Spectral Peaks')
       CALL mkCaption(Ng,'Summary of Tukey Spectral Peaks')
      END IF
c     ------------------------------------------------------------------
      IF(Iagr.lt.3.and.Svltab(LSLTPK))THEN
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellId(Ng,0,2,'col99','@','@',
     &                     'For Peak Probability > 0.99')
       CALL writTag(Ng,'</tr>')
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellId(Ng,0,0,'stuksp','@','@',
     &                     'Seasonal Tukey Spectral Peaks')
       CALL mkTableCellHeader(Ng,'col99 stuksp','center',Cstuk(1:nstuk))
       CALL writTag(Ng,'</tr>')
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellId(Ng,0,0,'tdtuksp','@','@',
     &       '<abbr title="trading day">TD</abbr> Tukey Spectral Peaks')
       CALL mkTableCellHeader(Ng,'col99 tdtuksp','center',
     &                        Cttuk(1:nttuk))
       CALL writTag(Ng,'</tr>')
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellId(Ng,0,2,'col90','@','@',
     &                     'For Peak Probability > 0.90')
       CALL writTag(Ng,'</tr>')
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellId(Ng,0,0,'stuksp90','@','@',
     &                     'Seasonal Tukey Spectral Peaks')
       CALL mkTableCellHeader(Ng,'col90 stuksp90','center',
     &                        Cstk90(1:nstk90))
       CALL writTag(Ng,'</tr>')
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellId(Ng,0,0,'tdtuksp90','@','@',
     &       '<abbr title="trading day">TD</abbr> Tukey Spectral Peaks')
       CALL mkTableCellHeader(Ng,'col90 tdtuksp90','center',
     &                        Cttk90(1:nttk90))
       CALL writTag(Ng,'</tr>')
      ELSE IF (Iagr.gt.3)THEN
       IF(Svltab(LSLTPK).or.Svltab(LSLDTP).or.Svltab(LSLITP))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,2,'col99','@','@',
     &                     'For Peak Probability > 0.99')
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLTPK).or.Svltab(LSLDTP))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,0,'stukspdir','@','@',
     &                      'Seasonal Tukey Spectral Peaks (direct)')
        CALL mkTableCellHeader(Ng,'col99 stukspdir','center',
     &                         Cstuk(1:nstuk))
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,0,'tdtukspdir','@','@',
     &       '<abbr title="trading day">TD</abbr> Tukey Spectral Peaks')
        CALL mkTableCellHeader(Ng,'col99 tdtukspdir','center',
     &                         Cttuk(1:nttuk))
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLTPK).or.Svltab(LSLITP))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,0,'stukspind','@','@',
     &                      'Seasonal Tukey Spectral Peaks (indirect)')
        CALL mkTableCellHeader(Ng,'col99 stukspind','center',
     &                         Cstuki(1:nstuki))
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,0,'tdtukspind','@','@',
     &       '<abbr title="trading day">TD</abbr> Tukey Spectral '//
     &       'Peaks (indirect)')
        CALL mkTableCellHeader(Ng,'col99 tdtukspind','center',
     &                         Cttuki(1:nttuki))
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLTPK).or.Svltab(LSLDTP).or.Svltab(LSLITP))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,2,'col90','@','@',
     &                     'For Peak Probability > 0.90')
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLTPK).or.Svltab(LSLDTP))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,0,'stukspdir90','@','@',
     &                      'Seasonal Tukey Spectral Peaks (direct)')
        CALL mkTableCellHeader(Ng,'col90 stukspdir90','center',
     &                         Cstk90(1:nstk90))
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,0,'tdtukspdir90','@','@',
     &       '<abbr title="trading day">TD</abbr> Tukey Spectral Peaks')
        CALL mkTableCellHeader(Ng,'col90 tdtukspdir90','center',
     &                         Cttk90(1:nttk90))
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLTPK).or.Svltab(LSLITP))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,0,'stukspind90','@','@',
     &                      'Seasonal Tukey Spectral Peaks (indirect)')
        CALL mkTableCellHeader(Ng,'col90 stukspind90','center',
     &                         Csti90(1:nsti90))
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellId(Ng,0,0,'tdtukspind90','@','@',
     &       '<abbr title="trading day">TD</abbr> Tukey Spectral '//
     &       'Peaks (indirect)')
        CALL mkTableCellHeader(Ng,'col90 tdtukspind90','center',
     &                         Ctti90(1:ntti90))
        CALL writTag(Ng,'</tr>')
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(istrue(Svltab,LSLTPK,LSLITP))THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       WRITE(Nform,1010)'peaks.tukey.seas: ',Cstuk(1:nstuk)
       WRITE(Nform,1010)'peaks.tukey.td: ',Cttuk(1:nttuk)
       IF(Iagr.gt.3)THEN
        WRITE(Nform,1010)'peaks.tukey.seas.ind: ',Cstuki(1:nstuki)
        WRITE(Nform,1010)'peaks.tukey.td.ind: ',Cttuki(1:nttuki)
       END IF
       WRITE(Nform,1010)'peaks.tukey.p90.seas: ',Cstk90(1:nstk90)
       WRITE(Nform,1010)'peaks.tukey.p90.td: ',Cttk90(1:nttk90)
       IF(Iagr.gt.3)THEN
        WRITE(Nform,1010)'peaks.tukey.p90.seas.ind: ',Csti90(1:nsti90)
        WRITE(Nform,1010)'peaks.tukey.p90.td.ind: ',Ctti90(1:ntti90)
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgspc',i6.6,'">')
 1010 FORMAT(a,a)
c-----------------------------------------------------------------------
      RETURN
      END
