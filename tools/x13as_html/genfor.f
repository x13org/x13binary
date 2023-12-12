C     Last change:  BCM   1 Jun 1998    4:08 pm
**==genfor.f    processed by SPAG 4.03F  at 11:36 on 10 Jun 1994
      SUBROUTINE genfor(Ok,Lchkin,Isrs)
      IMPLICIT NONE
c     ------------------------------------------------------------------
C --- THIS SUBROUTINE PRINTS THE HEADINGS FOR THE VARIOUS FILES AND
C --- INITIALIZES VALUES.
C --- THE UNIT MT IS THE CONTROL CARD INPUT FILE
C ---          MT1 IS THE MAIN PRINTOUT
C ---          MT2 IS THE LOG
C ---          NG IS THE FILE CONTAINING ALL THE Q STATISTICS
C ---          NFORM CONTAINS ALL THE F TABLES FOR THE RUN
C ---          NR IS THE TAPE INPUT FILE AND IS DEFINED IN THE ROUTINE
C ---                  INPUT
c ---  Note - not all of these variables are set in this routine (BCM)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'agr.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'filetb.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     ------------------------------------------------------------------
      CHARACTER ext*5,fil*(PFILCR)
      LOGICAL Ok,lok,Lchkin
      INTEGER Isrs,nfil,n1
c     ------------------------------------------------------------------
      INTEGER nblank,lstpth
      EXTERNAL nblank,lstpth
c     ------------------------------------------------------------------
c     For first series, initialize values
c     ------------------------------------------------------------------
      IF(Isrs.eq.1)THEN
*       Newpg=char(12)
       Iagr=0
       Nform=0
      END IF
c     ------------------------------------------------------------------
      Mt1=0
      Mt2=0
c     ------------------------------------------------------------------
c     Check filenames for improper file extensions
c     ------------------------------------------------------------------
      Nfilcr=nblank(Cursrs)
      nfil=nblank(Infile)
      IF(nfil.gt.3)THEN
       ext(1:4)=Infile((nfil-3):nfil)
       IF((ext(1:2).eq.'.s'.or.ext(1:2).eq.'.S').and.
     &   (ext(3:3).eq.'p'.or.ext(3:3).eq.'P').and.
     &   (ext(4:4).eq.'c'.or.ext(4:4).eq.'C'))THEN
        WRITE(STDERR,1010)'input spec',ext(1:4)
 1010   FORMAT(' ERROR: Enter ',a,' filename without "',a,
     &         '" file extension.')
        Ok=F
       END IF
      END IF
      IF(Nfilcr.gt.4)THEN
       ext=Cursrs((Nfilcr-4):Nfilcr)
       IF((ext(1:2).eq.'.h'.or.ext(1:2).eq.'.H').and.
     &   (ext(3:3).eq.'t'.or.ext(3:3).eq.'T').and.
     &   (ext(4:4).eq.'m'.or.ext(4:4).eq.'M').and.
     &   (ext(5:5).eq.'l'.or.ext(5:5).eq.'L'))THEN
        WRITE(STDERR,1010)'output',ext
        Ok=F
       END IF
      END IF
      IF(nfil.eq.0)THEN
       WRITE(STDERR,1011)
 1011  FORMAT('  No filename specified for input specification file.')
       Ok=F
      ELSE IF(Nfilcr.eq.0)THEN
       WRITE(STDERR,1012)
 1012  FORMAT('  No output filename specified.')
       Ok=F
      END IF
      IF(.not.Ok)THEN
       CALL abend
       RETURN
      END IF
c     ------------------------------------------------------------------
c     Try to open output file
c     ------------------------------------------------------------------
      fil=Cursrs(1:Nfilcr)//'.html'
      nfil=Nfilcr+5
      INQUIRE(FILE=fil(1:nfil),EXIST=Lexout)
      CALL fopen(fil(1:nfil),'program output file','UNKNOWN',Mt1,lok)
      Ok=Ok.and.lok
      IF(Ok)THEN
       n1=lstpth(fil,nfil)+1
       CALL mkHead(Mt1,fil(n1:nfil),
     &             PRGNAM//' Output File ('//fil(n1:nfil)//')',T,2,-1,T)
       CALL makDivId(Mt1,'content','@')
      END IF
c     ------------------------------------------------------------------
c     Try to open spec file
c     ------------------------------------------------------------------
      IF(Ok)THEN
       nfil=nblank(Infile)
       Infile=Infile(1:nfil)//'.spc'
       nfil=nfil+4
       CALL fopen(Infile(1:nfil),'input spec file','OLD',Mt,lok)
       Ok=Ok.and.lok
      END IF
c     ------------------------------------------------------------------
c     Try to open error file
c     ------------------------------------------------------------------
      IF(Ok)THEN
       fil=Cursrs(1:Nfilcr)//'_err.html'
       nfil=Nfilcr+9
       INQUIRE(FILE=fil(1:nfil),EXIST=Lexerr)
       CALL fopen(fil(1:nfil),'program error file','UNKNOWN',Mt2,lok)
       Ok=Ok.and.lok
       IF(Ok)THEN
        n1=lstpth(fil,nfil)+1
        CALL mkHead(Mt2,fil(n1:nfil),
     &              PRGNAM//' Error File ('//fil(1:nfil)//')',F,2,-1,F)
       END IF
      END IF
*c     ------------------------------------------------------------------
*c     Try to open index file
*c     ------------------------------------------------------------------
*      IF(Ok)THEN
*       fil=Cursrs(1:Nfilcr)//'_index.html'
*       nfil=Nfilcr+11
*       INQUIRE(FILE=fil(1:nfil),EXIST=Lexerr)
*       CALL fopen(fil(1:nfil),'program Index file','UNKNOWN',Mt0,lok)
*       Ok=Ok.and.lok
*       IF(Ok)then
*        n1=lstpth(fil,nfil)+1
*        CALL mkHead(Mt0,fil(n1:nfil),
*     &              PRGNAM//' Index File ('//fil(n1:nfil)//')',F,2,-1)
*        n1=lstpth(Cursrs,Nfilcr)+1
*        CALL writTagOneLine(Mt0,'h2','@',
*     &                      'Index for '//Cursrs(n1:Nfilcr)//'.html')
*        CALL writTagClass(Mt0,'ul','indent')
*       END IF
*      END IF
c     ------------------------------------------------------------------
c     Print out summary of files opened by this routine if all files
c     have been opened
c     ------------------------------------------------------------------
      IF(Ok)THEN
       nfil=nblank(Infile)
       WRITE(STDOUT,1030)Infile(1:nfil),Cursrs(1:Nfilcr)//'.html',
     &                   Cursrs(1:Nfilcr)//'_err.html'
 1030  FORMAT(/,'  Reading input spec file from ',a,/,
     &        '  Storing any program output into ',a,/,
     &        '  Storing any program error messages into ',a)
c     ------------------------------------------------------------------
       CALL writTagOneLine(Mt2,'h1','center',
     &                 'Error messages generated from processing the '//
     &                 PRGNAM//' spec file '//Cbr//Infile(1:nfil)//':')
c     ------------------------------------------------------------------
c     If all files have not been opened, close all files and return
c     ------------------------------------------------------------------
      ELSE
       CALL fclose(-1)
       RETURN
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
