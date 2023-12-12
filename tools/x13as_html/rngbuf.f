C     Last change:  BCM  13 May 2005    1:42 pm
      LOGICAL FUNCTION rngbuf(Cmd,Linno,Lin,Linln)
c-----------------------------------------------------------------------
c     rngbuf.f, Release 1, Subroutine Version 1.2, Modified 03 Feb 1995.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'lex.i'
      INCLUDE 'cchars.i'
      INCLUDE 'stdio.i'
c     -----------------------------------------------------------------
      LOGICAL PFAIL,PSCCD
      PARAMETER(PFAIL=.false.,PSCCD=.true.)
c     -----------------------------------------------------------------
      CHARACTER Lin*(*),nxtchr
      INTEGER Cmd,i,Linln,Linno
c     -----------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      LOGICAL psteof
      CHARACTER buf(0:PBUFSZ-1)*(LINLEN)
      INTEGER bufln(0:PBUFSZ-1),begbuf,endbuf,crntbf,crntln
      SAVE buf,bufln,begbuf,endbuf,crntbf,crntln,psteof
c-----------------------------------------------------------------------
c     Which command
c-----------------------------------------------------------------------
      rngbuf=PSCCD
c     -----------------------------------------------------------------
      GO TO(10,20,50,60),Cmd
      WRITE(STDERR,*)'System error:  illegal buffer request,',Cmd
      CALL abend()
      RETURN
c-----------------------------------------------------------------------
c     Initialize the Buffer
c-----------------------------------------------------------------------
   10 crntln=0
      begbuf=PBUFSZ-1
      endbuf=PBUFSZ-1
      crntbf=endbuf
      bufln(0)=0
      psteof=.false.
      Lexok=.true.
      GO TO 80
c-----------------------------------------------------------------------
c     Read the next line and fail if EOF
c-----------------------------------------------------------------------
   20 IF(psteof)GO TO 70
c-----------------------------------------------------------------------
c     Read in a new line if necessary
c-----------------------------------------------------------------------
      IF(crntbf.eq.endbuf)THEN
       READ(Inputx,1010,END=70)Lin
 1010  FORMAT(a)
c     -----------------------------------------------------------------
       endbuf=mod(endbuf+1,PBUFSZ)
       IF(begbuf.eq.endbuf)begbuf=mod(begbuf+1,PBUFSZ)
       crntbf=mod(crntbf+1,PBUFSZ)
c-----------------------------------------------------------------------
c     Tack on an EOL
c-----------------------------------------------------------------------
       Linln=nblank(Lin)
       Linln=Linln+1
       IF(Linln.gt.LINLEN)THEN
        WRITE(STDERR,*)' ERROR: Input record longer than limit :',
     &                 LINLEN
        CALL abend()
        RETURN  
       END IF
       Lin(Linln:Linln)=NEWLIN
c-----------------------------------------------------------------------
c     Filter out all unprintable characters
c-----------------------------------------------------------------------
       i=1
c     -----------------------------------------------------------------
       DO WHILE (.true.)
*        i=i+1
c     -----------------------------------------------------------------
        IF(i.lt.Linln)THEN
         nxtchr=Lin(i:i)
c     -----------------------------------------------------------------
c     Change by BCM to allow tab characters to be read in spec file
c     and not skipped over - May 2005
c     -----------------------------------------------------------------
         IF((nxtchr.lt.' '.or.nxtchr.gt.'~').and.
     &      (.not.(nxtchr.eq.TABCHR)))THEN
C          CALL inpter(PERROR,Pos,'Skipped over unprintable character her
C     &e',.true.)
          Lin(i:Linln-1)=Lin(i+1:Linln)
          Linln=Linln-1
c     -----------------------------------------------------------------
         ELSE
          i=i+1
         END IF
         GO TO 30
        END IF
c     -----------------------------------------------------------------
        GO TO 40
   30   CONTINUE
       END DO
c-----------------------------------------------------------------------
c     Store the next line in the buffer and return
c-----------------------------------------------------------------------
   40  buf(endbuf)=Lin
       bufln(endbuf)=Linln
c     -----------------------------------------------------------------
      ELSE
       crntbf=mod(crntbf+1,PBUFSZ)
       Lin=buf(crntbf)
       Linln=bufln(crntbf)
      END IF
c     -----------------------------------------------------------------
      crntln=crntln+1
      Linno=crntln
      GO TO 80
c-----------------------------------------------------------------------
c     Push back the last line on to the stack
c-----------------------------------------------------------------------
   50 IF(crntbf.eq.begbuf)THEN
       rngbuf=PFAIL
       Linln=0
c-----------------------------------------------------------------------
c     If we are at the EOF we don't need to back up, just return the
c last line in the file.
c-----------------------------------------------------------------------
      ELSE
       IF(.not.psteof)THEN
        crntbf=mod(crntbf+PBUFSZ-1,PBUFSZ)
        crntln=crntln-1
       END IF
c     -----------------------------------------------------------------
       Lin=buf(crntbf)
       Linln=bufln(crntbf)
       Linno=crntln
       psteof=.false.
      END IF
      GO TO 80
c-----------------------------------------------------------------------
c     Get a line Lineno and fail if the line isn't in the buffer
c-----------------------------------------------------------------------
   60 IF(Linno.lt.crntln-mod(crntbf+PBUFSZ-begbuf,PBUFSZ).or.
     &   Linno.gt.crntln)THEN
       rngbuf=PFAIL
       Linln=0
c     -----------------------------------------------------------------
      ELSE
       i=mod(crntbf+Linno-crntln+PBUFSZ,PBUFSZ)
       Lin=buf(i)
       Linln=bufln(i)
      END IF
c     END CASE STATEMENT
      GO TO 80
c-----------------------------------------------------------------------
c     Reached the EOF
c-----------------------------------------------------------------------
   70 Lin(1:1)=CHREOF
      Linln=1
      psteof=.true.
      rngbuf=PFAIL
c     -----------------------------------------------------------------
   80 RETURN
      END
