      SUBROUTINE savpk(Iagr,Lsumm,Nspdir,Ntpdir)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     save spectral peak information in log file and/or .xdg/.mdg file
c     ------------------------------------------------------------------
      INCLUDE 'rho.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'spcsvl.i'
c     ------------------------------------------------------------------
      INTEGER Iagr,Lsumm,Nspdir,Ntpdir
c     ------------------------------------------------------------------
      LOGICAL istrue
      EXTERNAL istrue
c-----------------------------------------------------------------------
      IF(Ntpeak.eq.0)THEN
       Ctpeak(1:4)='none'
       Ntpeak=4
      ELSE
       Ntpeak=Ntpeak-1
      END IF
      IF(Nspeak.eq.0)THEN
       Cspeak(1:4)='none'
       Nspeak=4
      ELSE
       Nspeak=Nspeak-1
      END IF
c     ------------------------------------------------------------------
      IF(istrue(Svltab,LSLSPK,LSLISP))THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1000)Inlgfl
       CALL mkTableTag(Ng,'w60','Summary of Spectral Peaks')
       CALL mkCaption(Ng,'Summary of Spectral Peaks')
      END IF
c     ------------------------------------------------------------------
      IF(Iagr.lt.3.and.Svltab(LSLSPK))THEN
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                        'Seasonal Spectral Peaks')
       CALL mkTableCell(Ng,'center',Cspeak(1:Nspeak))
       CALL writTag(Ng,'</tr>')
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &             '<abbr title="trading day">TD</abbr> Spectral Peaks')
       CALL mkTableCell(Ng,'center',Ctpeak(1:Ntpeak))
       CALL writTag(Ng,'</tr>')
      ELSE IF (Iagr.gt.3)THEN
       IF(Svltab(LSLSPK).or.Svltab(LSLDSP))THEN
        IF(Cspeak(1:Nspeak).eq.'none')THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal Spectral Peaks (direct)')
         CALL mkTableCell(Ng,'center',Cspeak(1:Nspeak))
         CALL writTag(Ng,'</tr>')
        ELSE IF(Nspdir.eq.0)THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal Spectral Peaks (direct)')
         CALL mkTableCell(Ng,'center','none')
         CALL writTag(Ng,'</tr>')
        ELSE IF(Nspdir.eq.Nspeak)THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal Spectral Peaks (direct)')
         CALL mkTableCell(Ng,'center',Cspeak(1:Nspeak))
         CALL writTag(Ng,'</tr>')
        ELSE
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal Spectral Peaks (direct)')
         CALL mkTableCell(Ng,'center',Cspeak(1:Nspdir))
         CALL writTag(Ng,'</tr>')
        END IF
        IF(Ctpeak(1:Ntpeak).eq.'none')THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &           '<abbr title="trading day">TD</abbr> Spectral '//
     &           'Peaks (direct)')
         CALL mkTableCell(Ng,'center',Ctpeak(1:Ntpeak))
         CALL writTag(Ng,'</tr>')
        ELSE IF(Ntpdir.eq.0)THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &           '<abbr title="trading day">TD</abbr> Spectral '//
     &           'Peaks (direct)')
         CALL mkTableCell(Ng,'center','none')
         CALL writTag(Ng,'</tr>')
        ELSE IF(Ntpdir.eq.Ntpeak)THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &           '<abbr title="trading day">TD</abbr> Spectral '//
     &           'Peaks (direct)')
         CALL mkTableCell(Ng,'center',Ctpeak(1:Ntpeak))
         CALL writTag(Ng,'</tr>')
        ELSE
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &           '<abbr title="trading day">TD</abbr> Spectral '//
     &           'Peaks (direct)')
         CALL mkTableCell(Ng,'center',Ctpeak(1:Ntpdir))
         CALL writTag(Ng,'</tr>')
        END IF
       END IF
       IF(Svltab(LSLSPK).or.Svltab(LSLISP))THEN
        IF(Cspeak(1:Nspeak).eq.'none')THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal Spectral Peaks (indirect)')
         CALL mkTableCell(Ng,'center',Cspeak(1:Nspeak))
         CALL writTag(Ng,'</tr>')
        ELSE IF(Nspdir.eq.0)THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal Spectral Peaks (indirect)')
         CALL mkTableCell(Ng,'center',Cspeak(1:Nspeak))
         CALL writTag(Ng,'</tr>')
        ELSE IF(Nspdir.eq.Nspeak)THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal Spectral Peaks (indirect)')
         CALL mkTableCell(Ng,'center','none')
         CALL writTag(Ng,'</tr>')
        ELSE
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal Spectral Peaks (indirect)')
         CALL mkTableCell(Ng,'center',Cspeak((Nspdir+1):Nspeak))
         CALL writTag(Ng,'</tr>')
        END IF
        IF(Ctpeak(1:Ntpeak).eq.'none')THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &           '<abbr title="trading day">TD</abbr> Spectral '//
     &           'Peaks (indirect)')
         CALL mkTableCell(Ng,'center',Ctpeak(1:Ntpeak))
         CALL writTag(Ng,'</tr>')
        ELSE IF(Ntpdir.eq.0)THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &           '<abbr title="trading day">TD</abbr> Spectral '//
     &           'Peaks (indirect)')
         CALL mkTableCell(Ng,'center',Ctpeak(1:Ntpeak))
         CALL writTag(Ng,'</tr>')
        ELSE IF(Ntpdir.eq.Ntpeak)THEN
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &           '<abbr title="trading day">TD</abbr> Spectral '//
     &           'Peaks (indirect)')
         CALL mkTableCell(Ng,'center','none')
         CALL writTag(Ng,'</tr>')
        ELSE
         CALL writTag(Ng,'<tr>')
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &           '<abbr title="trading day">TD</abbr> Spectral '//
     &           'Peaks (indirect)')
         CALL mkTableCell(Ng,'center',Ctpeak((Ntpdir+1):Ntpeak))
         CALL writTag(Ng,'</tr>')
        END IF
       END IF
      END IF
      IF(istrue(Svltab,LSLSPK,LSLISP))THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       WRITE(Nform,1010)'peaks.seas: ',Cspeak(1:Nspeak)
       WRITE(Nform,1010)'peaks.td: ',Ctpeak(1:Ntpeak)
       IF(Iagr.gt.3)THEN
        IF(Cspeak(1:Nspeak).eq.'none')THEN
         WRITE(Nform,1010)'peaks.seas.dir: ',Cspeak(1:Nspeak)
         WRITE(Nform,1010)'peaks.seas.ind: ',Cspeak(1:Nspeak)
        ELSE IF(Nspdir.eq.0)THEN
         WRITE(Nform,1010)'peaks.seas.dir: ','none'
         WRITE(Nform,1010)'peaks.seas.ind: ',Cspeak(1:Nspeak)
        ELSE IF(Nspdir.eq.Nspeak)THEN
         WRITE(Nform,1010)'peaks.seas.dir: ',Cspeak(1:Nspeak)
         WRITE(Nform,1010)'peaks.seas.ind: ','none'
        ELSE
         WRITE(Nform,1010)'peaks.seas.dir: ',Cspeak(1:Nspdir)
         WRITE(Nform,1010)'peaks.seas.ind: ',Cspeak((Nspdir+1):Nspeak)
        END IF
        IF(Ctpeak(1:Ntpeak).eq.'none')THEN
         WRITE(Nform,1010)'peaks.td.dir: ',Ctpeak(1:Ntpeak)
         WRITE(Nform,1010)'peaks.td.ind: ',Ctpeak(1:Ntpeak)
        ELSE IF(Ntpdir.eq.0)THEN
         WRITE(Nform,1010)'peaks.td.dir: ','none'
         WRITE(Nform,1010)'peaks.td.ind: ',Ctpeak(1:Ntpeak)
        ELSE IF(Ntpdir.eq.Ntpeak)THEN
         WRITE(Nform,1010)'peaks.td.dir: ',Ctpeak(1:Ntpeak)
         WRITE(Nform,1010)'peaks.td.ind: ','none'
        ELSE
         WRITE(Nform,1010)'peaks.td.dir: ',Ctpeak(1:Ntpdir)
         WRITE(Nform,1010)'peaks.td.ind: ',Ctpeak((Ntpdir+1):Ntpeak)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgspc',i6.6,'">')
 1010 FORMAT(a,a)
c-----------------------------------------------------------------------
      RETURN
      END
