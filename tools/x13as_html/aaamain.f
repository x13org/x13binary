C     Last change: Nov, 2021, add a logical variable to test if this is
C     the first .spc which has composite spec, program only processes
C     to this file. The .spcs after this file will be ignored.
C     previous change:  BCM  15 Oct 1998   12:21 pm
**==aa0001.f    processed by SPAG 4.03F  at 10:52 on 28 Sep 1994
      BLOCK DATA INX12
      IMPLICIT NONE
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'chrt.cmn'
      INCLUDE 'ssap.cmn'
C-----------------------------------------------------------------------
      INTEGER j,ii
C-----------------------------------------------------------------------
      DATA Ialpha/'j','f','m','a','m','j','j','a','s','o','n','d'/
      DATA Ialphq/'1','2','3','4'/
      DATA I1,I4,I7/'*','I','.'/
      DATA Imid/2,6,10,14,18,22,26,30,34,38,42,46,50,54/
      DATA F1/'(1x,i2,a1,i4,2x, (f9.2,2x),3x,f9.2,2x,a10)'/
      DATA F2/'(11x, (1x,i2,a1,i4,1x,a1,1x),4x,a7,3x,a8)'/
      DATA F3/'(1x,i2,a1,i4,2x, (e10.4,1x),3x,f9.2,2x,a10)'/
      DATA(Cut(1,ii),ii=1,4)/3D0,4D0,5D0,6D0/
      DATA(Cut(2,ii),ii=1,4)/2D0,3D0,4D0,5D0/
      DATA(Cut(3,ii),ii=1,4)/3D0,4D0,5D0,6D0/
      DATA(Cut(4,ii),ii=1,4)/3D0,5D0,7D0,10D0/
      DATA(Cut(5,ii),ii=1,4)/3D0,4D0,5D0,6D0/
      DATA(Ch(j),j=1,NEST)/'%','+','#','$','@'/
C-----------------------------------------------------------------------
      END
**==x12a.f    processed by SPAG 4.03F  at 10:53 on 28 Sep 1994
      PROGRAM x12a
      IMPLICIT NONE
C-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'cchars.i'
      INCLUDE 'build.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.prm'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'seatop.cmn'
      INCLUDE 'htmlfile.cmn'
      INCLUDE 'nsums.i'
C-----------------------------------------------------------------------
      LOGICAL lmeta,lchkin,rok,lcomp,ldata,lgraf,lexgrf,gmtok,x11agr,
     &        fok,samepth,l1stcomp
      CHARACTER insrs*(PFILCR),outsrs*(PFILCR),mtafil*(PFILCR),
     &          datsrs*(PFILCR),dtafil*(PFILCR),dattim*(24),tfmt*(10),
     &          grfdir*(PFILCR),xb*(PFILCR)
      INTEGER i,failed,nfail,unopnd,nopen,n1,n2,n3,xfail,indx,indx2,
     &        nmtfil,n4,i1,i2,ilghdr,ifoot,fh,livec
      DIMENSION outsrs(PSRS),insrs(PSRS),datsrs(PSRS),failed(PSRS),
     &          unopnd(PSRS),livec(PSRS)
C-----------------------------------------------------------------------
      CHARACTER*24 cvdttm
      INTEGER nblank,lstpth
      EXTERNAL nblank,lstpth,cvdttm
C-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
C-----------------------------------------------------------------------
c     Initialize variables
C-----------------------------------------------------------------------
      Mt2=0
      Ng=8
      fh=7
      STDERR=6
      nfail=0
      xfail=0
      nopen=0
      Lfatal=F
      x11agr=T
      l1stcomp=F
      CALL setchr(' ',PFILCR,Infile)
      CALL setchr(' ',PFILCR,Cursrs)
      CALL setchr(' ',PFILCR,Curgrf)
      CALL setchr(' ',PFILCR,grfdir)
      CALL setchr(' ',PFILCR,dtafil)
      CALL setchr(' ',PFILCR,xb)
      TABCHR=CHAR(9)
      Ierhdr=NOTSET
      Crvend=CNOTST
      dattim='                        '
      CALL fdate(dattim)
      dattim=cvdttm(dattim)
cunix
      CHREOF=char(4)
      NEWLIN=char(10)
C     ------------------------------------------------------------------
cdos
cdos      CHREOF=char(26)
cdos      NEWLIN=char(13)
      Idxlog=1
      Inlgfl=0
C-----------------------------------------------------------------------
C     Print out introductory header giving version number of program.
C-----------------------------------------------------------------------
      WRITE(STDOUT,1000)PRGNAM,VERNUM,BUILD,dattim
C-----------------------------------------------------------------------
c     Get options specified on the command line.
C-----------------------------------------------------------------------
      CALL getxop(lmeta,lchkin,lcomp,Lsumm,Lmdsum,Lnoprt,ldata,dtafil,
     &            lgraf,grfdir,Lcmpaq,Ltimer)
      IF(Lfatal)STOP
      IF(Lxhtml)THEN
       Cbr='<br />'
       Charhr='<hr />'
      ELSE
       Cbr=' <br> '
       Charhr=' <hr> '
      END IF
C-----------------------------------------------------------------------
c     If input is from metafile, get list of series names
C-----------------------------------------------------------------------
      IF(lmeta)THEN
       CALL gtmtfl(insrs,outsrs,datsrs,mtafil,ldata,dtafil)
       IF(Lfatal)STOP
       nmtfil=nblank(mtafil)
       logfil=mtafil(1:(nmtfil-4))
      ELSE
C-----------------------------------------------------------------------
c     Else, set up variables for using a single file.
C-----------------------------------------------------------------------
       Imeta=1
       insrs(1)=Infile
       outsrs(1)=Cursrs
       logfil=outsrs(1) 
       mtafil=' '
       nmtfil=1
      END IF
C-----------------------------------------------------------------------
c     initialize variables for Lmdsum
C-----------------------------------------------------------------------
      nSeatsSer=0
      noTratadas=0
      call inicSumS()
C-----------------------------------------------------------------------
c     open log file for all X-13A-S runs.  First, get path information
C-----------------------------------------------------------------------
      nlgfil=nblank(logfil)
      CALL cnvfil(logfil,nlgfil,LogfHTML,NlfHTML,Nlflast)
      logfil(nlgfil+1:nlgfil+9)='_log.html'
      nlgfil=nlgfil+9
      OPEN(Ng,FILE=logfil(1:nlgfil),STATUS='UNKNOWN',ERR=20)
      IF(Lmeta)THEN
       CALL mkHead(Ng,logfil(1:nlgfil),PRGNAM//' Log File ('//
     &             logfil(1:nlgfil)//')',T,2,-1,T)
      ELSE
       CALL mkHead(Ng,logfil(1:nlgfil),PRGNAM//' Log File ('//
     &             logfil(1:nlgfil)//')',T,2,-1,F)
      END IF

      IF(Lmeta.and.Imeta.gt.1)CALL makDivId(Ng,'content','@')
      IF(.not.lchkin)THEN
       ilghdr=67+nblank(VERNUM)+nblank(BUILD)
       CALL writTagOneLine(Ng,'h1','center',
     &                     'Log for '//PRGNAM//' program (Version '//
     &                     VERNUM//' Build '//BUILD//')')
       CALL writTagOneLine(Ng,'h2','center',dattim)
       CALL mkPOneLine(Ng,'@','&nbsp;')
       i2=ilghdr/2
      END IF
C-----------------------------------------------------------------------
c     Process all the series.
C-----------------------------------------------------------------------
      DO i=1,Imeta
       rok=T
       dtafil=' '
       IF (l1stcomp) THEN
        WRITE(STDERR,1090)
        WRITE(Ng,1090)
        Imeta = i-1
        exit
       END IF
       IF(lmeta)THEN
        Infile=insrs(i)
        Cursrs=outsrs(i)
        IF(ldata)dtafil=datsrs(i)
       END IF
       n1=nblank(Infile)
       n2=nblank(Cursrs)
       CALL cnvfil(Cursrs,n2,CsrsHTML,NcsHTML,Ncslast)
C-----------------------------------------------------------------------
c      Set up graphics variables
C-----------------------------------------------------------------------
       gmtok=T
       fok=T
       IF(lgraf)THEN
        Ngrfcr=nblank(grfdir)
        Curgrf(1:Ngrfcr)=grfdir(1:Ngrfcr)
        n4=lstpth(grfdir,Ngrfcr)
        IF(n4.lt.Ngrfcr)THEN
         Ngrfcr=Ngrfcr+1
cdos  backslash for directory
cdos         Curgrf(Ngrfcr:Ngrfcr)='\\'
cunix forward slash for directory
         Curgrf(Ngrfcr:Ngrfcr)='/'
        END IF
        n4=lstpth(Cursrs,n2)
        Curgrf((Ngrfcr+1):(Ngrfcr+(n2-n4)))=Cursrs((n4+1):n2)
        Ngrfcr=Ngrfcr+n2-n4
        INQUIRE(FILE=Curgrf(1:Ngrfcr)//'.gmt',EXIST=lexgrf)
        CALL fopen(Curgrf(1:Ngrfcr)//'.gmt','graphical meta file',
     &             'UNKNOWN',Grfout,gmtok)
        IF(.not.gmtok)CALL abend
       END IF
C-----------------------------------------------------------------------
       IF(gmtok)THEN
        CALL makeAnchor(Ng,Idxlog,'pos')
        IF(Lmeta)THEN
         IF(Imeta.gt.1)THEN
          CALL makeSkipLink(Ng,Idxlog,'Log Entry',F)
          CALL makeAnchor(Ng,Idxlog,'skip')
          livec(Idxlog)=i
         END IF
        END IF
c         write(*,*) 'enter profiler'
c         call profiler(0,'Profiler.txt')
        Idxlog=Idxlog+1
        CALL x12run(i,unopnd,nopen,lchkin,lcomp,rok,fok,n1,nfail,ldata,
     &              dtafil,mtafil,nmtfil,dattim,x11agr,lgraf,lexgrf,
     &              l1stcomp)
       ELSE
        fok=F
       END IF
C-----------------------------------------------------------------------
c     print error message if there was an input error.
C-----------------------------------------------------------------------
       IF(Lfatal)THEN
        IF(rok)THEN
         WRITE(STDOUT,*)' Program error(s) halt execution for ',
     &                  Infile(1:n1),'.spc'
         xfail=xfail+1
        END IF
        IF(gmtok.and.fok)
     &     WRITE(STDOUT,1020)' Check error file '//Cursrs(1:n2)//
     &                       '_err.html'
        nfail=nfail+1
        failed(nfail)=i
c-----------------------------------------------------------------------
       ELSE IF(.not.lchkin)THEN
        dattim='                        '
        CALL fdate(dattim)
        dattim=cvdttm(dattim)
        WRITE(STDOUT,1020)' Execution complete for '//Infile(1:n1)//
     &                    '.spc at '//dattim
       END IF
c-----------------------------------------------------------------------
c     Generate footnotes
c-----------------------------------------------------------------------
       IF(fok)THEN
        CALL makeAnchor(Mt1,Idxtab,'pos')
        IF(Infoot.gt.0)THEN
         WRITE(Mt1,1050)
         DO ifoot=1,Infoot
          CALL genfoot(Mt1,ifoot,Vfoot(ifoot))
         END DO
         CALL writTag(Mt1,'</dl>')
        END IF
c-----------------------------------------------------------------------
c     Generate index for output file
c-----------------------------------------------------------------------
        CALL writTag(Mt1,'</div>')
        CALL makDivId(Mt1,'rightnavigation','@')
        IF(Idxtab.eq.1)THEN
c         IF(Lnoprt)THEN
          CALL writTagOneLine(Mt1,'p','@',
     &                        'No entries in index.')
c         ELSE
c          CALL writTagOneLine(Mt1,'p','@',
c     &                        'No entries in index due to input error.')
c         END IF
        ELSE
         n1=lstpth(Cursrs,Nfilcr)+1
         CALL makeAnchor(Mt1,-1,'index')
         CALL writTagOneLine(Mt1,'h2','@',
     &                       'Index for '//Cursrs(n1:Nfilcr)//'.html')
         CALL writTagClass(Mt1,'ul','indentnav')
         indx=1
         indx2=1
*         write(Mtprof,*)'  idxtab = ',Idxtab
         DO WHILE (indx.lt.Idxtab)
*          write(Mtprof,*)'  indx, Vindx(indx) = ', indx, Vindx(indx)
          CALL genIndex(Mt1,indx,Vindx(indx))
          indx2=indx2+1
          if(indx2.gt.PNINDX)STOP
         END DO
         CALL writTag(Mt1,'</ul>')
        END IF
        CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
        CALL writTag(Mt1,Charhr)
        CALL writTagClass(Mt1,'ul','indentnav')
        n3=Ncslast+1
        WRITE(Mt1,1070)CsrsHTML(n3:NcsHTML)//'_err.html',
     &                 'Error Messages'
        n4=Nlflast+1
        samepth=n3.eq.n4
        IF (samepth) THEN
         IF (n3.gt.1) samepth=Cursrs(1:n3).eq.Logfil(1:n4)
        END IF
        IF (samepth) THEN
         WRITE(Mt1,1060)LogfHTML(1:NlfHTML)//'_log.html#pos',i,
     &                  'Log Entry'
        ELSE
         WRITE(Mt1,1080)LogfHTML(1:NlfHTML)//'_log.html#pos',
     &                  i,'Log Entry','*',1,'*',1
        END IF
        CALL writTag(Mt1,'</ul>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
        IF(.not.samepth)THEN
         CALL mkPOneLine(Mt1,'indentnav',
     &                   '(*) Link to Log File specified above only '//
     &                   'valid if complete paths specified for '//
     &                   'output and meta files.')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
         WRITE(Mt1,1050)
         CALL genfoot(Mt1,1,PLGLNK)
         CALL writTag(Mt1,'</dl>')
        END IF
        CALL writTag(Mt1,'</div>')
c-----------------------------------------------------------------------
        CALL writTag(Mt1,'</body>')
        CALL writTag(Mt1,'</html>')
        CALL writTag(Mt2,'</body>')
        CALL writTag(Mt2,'</html>')
C-----------------------------------------------------------------------
c     Close all files
C-----------------------------------------------------------------------
        CALL fclose(-1)
       END IF
      END DO
C-----------------------------------------------------------------------
      IF((.not.Lquiet).and.nfail.gt.xfail)WRITE(STDERR,1030)
C-----------------------------------------------------------------------
*      IF(fok)THEN
*       STOP
*      END IF
      CALL makeAnchor(Ng,Idxlog,'pos')
      IF(Imeta.gt.1)THEN
       IF(nopen.gt.0.or.nfail.gt.0)THEN
        CALL makeSkipLink(Ng,Idxlog,'Log Entry',F)
        CALL makeAnchor(Ng,Idxlog,'skip')
        livec(Idxlog)=-1
        Idxlog=Idxlog+1
        CALL prtlog(Ng,insrs,outsrs,nopen,unopnd,nfail,failed,
     &              mtafil,nmtfil,samepth)
        CALL makeAnchor(Ng,Idxlog,'pos')
       END IF
       CALL mkMetaHTMLFile(fh,insrs,outsrs,datsrs,nopen,unopnd,ldata,
     &                     mtafil,nmtfil-4)
c-----------------------------------------------------------------------
       if (Lmdsum.and.nSeatsSer.gt.25) then
*          write(*,*)'  Lmdsum=T  nSeatsSer = ',nSeatsSer
          call writeSumS(mtafil,nmtfil-4,nSeatsSer,noTratadas,wSposBphi,
     $            wSstochTD,wSstatseas,wSrmod,wSxl)
*       else
*          write(*,*)'  Lmdsum=F  nSeatsSer = ',nSeatsSer
       end if
      END IF
C-----------------------------------------------------------------------
      IF(lgraf.and.Lsumm.gt.0)THEN
       WRITE(Ng,1040)
      END IF
C-----------------------------------------------------------------------
c     generate index for log file if 
C-----------------------------------------------------------------------
      IF(Imeta.gt.1)THEN
       CALL writTag(Ng,'</div>')
       CALL makDivId(Ng,'rightnavigation','@')
       CALL makeAnchor(Ng,-1,'index')
       CALL writTagOneLine(Ng,'h1','center',
     &                     'Index for Log File '//logfil(1:nlgfil))
       CALL mkPOneLine(Ng,'@','&nbsp;')
       CALL writTagClass(Ng,'ul','indentnav')
       indx=1
       DO WHILE (indx.lt.Idxlog)
        IF(livec(indx).gt.0)THEN
         Cursrs=outsrs(livec(indx))
         n2=nblank(Cursrs)
         n3=lstpth(Cursrs,n2)+1
         CALL makeIndexLink(Ng,indx,LogfHTML(1:NlfHTML)//'_log.html',
     &                      'Log entry for '//Cursrs(n3:n2),T,F)
        ELSE
         CALL makeIndexLink(Ng,indx,LogfHTML(1:NlfHTML)//'_log.html',
     &                      'Errors related to input files in '//
     &                      logfil(1:nlgfil),T,F)
        END IF
       END DO
       CALL writTag(ng,'</ul>')
       CALL mkPOneLine(ng,'@','&nbsp;')
       CALL writTag(ng,'</div>')
      END IF
C-----------------------------------------------------------------------
      CALL writTag(Ng,'</body>')
      CALL writTag(Ng,'</html>')
      CALL fstop()
      STOP
C-----------------------------------------------------------------------
   20 WRITE(STDERR,1020)' Unable to open '//logfil(1:nlgfil)
      CALL abend
      CALL fstop()
      STOP
C-----------------------------------------------------------------------
 1000 FORMAT(/,1x,a,' Seasonal Adjustment Program',/,
     &        ' Version Number ',a,' Build ',a,/,' Execution began ',a)
 1020 FORMAT(/,a)
 1030 FORMAT(/,
     &  ' NOTE:  Correct input errors in the order they are detected',/,
     &  '        since the first one or two may be responsible for',/,
     &  '        the others (especially if there are errors in the',/,
     &  '        SERIES or COMPOSITE spec).',/)
 1040 FORMAT(/,'<p><strong>NOTE:</strong> The diagnostic files ',
     &        'produced by the -s option are stored in the',/,
     &        ' directory specified by the graphics (-g) option.</p>')
 1050 FORMAT(/,'<dl id="content-footnotes">')
 1060 FORMAT('<li> <a href="',a,i5.5,'"> ',a,' </a> </li>')
 1070 FORMAT('<li> <a href="',a,'"> ',a,' </a> </li>')
 1080 FORMAT('<li> <a href="',a,i5.5,'">',a,'</a> &nbsp; ',a,/,
     &      '<a href="#footnote',i4.4,
     &      '" class="longdesc">','Link to definition of ',a,'</a>',/
     &      '<a name="foot',i4.4,'"></a></li>')
 1090 FORMAT(/,
     &  ' WARNING: X-13 will not process any series in a metafile ',/,
     &  '          following a spec for a composite adjustment.',/)
C-----------------------------------------------------------------------
      END
