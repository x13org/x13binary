C     Last change:  BCM  16 Feb 1999   11:15 am
      SUBROUTINE gtmtfl(Insrs,Outsrs,Datsrs,Mtafil,Ldata,Dtafil)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c     Process metafile.  Get a list of the input filenames, and set
c     the output filenames.
C-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
C-----------------------------------------------------------------------
      CHARACTER Insrs*(PFILCR),Outsrs*(PFILCR),Datsrs*(PFILCR),blnk*1,
     &          mtalin*(PFILCR),Dtafil*(PFILCR),ext*4,Mtafil*(PFILCR),
     &          quot*1
      LOGICAL ok,Ldata
      INTEGER i,i2,meta,j,n,nmeta,nfil,ichr,nchr,n1,n2
      DIMENSION Insrs(PSRS),Outsrs(PSRS),Datsrs(PSRS)
C-----------------------------------------------------------------------
      INTEGER nblank,lstpth
      EXTERNAL nblank,lstpth
c-----------------------------------------------------------------------
      blnk=' '
      quot='"'
      meta=NOTSET
      ok=.true.
C-----------------------------------------------------------------------
c     First, attempt to open metafile
C-----------------------------------------------------------------------
      IF(Ldata)THEN
       nfil=nblank(Dtafil)
       IF(nfil.gt.3)THEN
        ext=Dtafil((nfil-3):nfil)
        IF((ext(1:2).eq.'.d'.or.ext(1:2).eq.'.D').and.
     &     (ext(3:3).eq.'t'.or.ext(3:3).eq.'T').and. 
     &     (ext(4:4).eq.'a'.or.ext(4:4).eq.'A'))THEN
         WRITE(STDERR,1010)'data',ext
         ok=.false.
        END IF
       END IF
       IF(ok)THEN
        Mtafil=Dtafil(1:nfil)//'.dta'
        nfil=nfil+4
        WRITE(STDOUT,1000)
        CALL fopen(Mtafil(1:nfil),'data metafile','OLD',meta,ok)
       END IF
      ELSE
       nfil=nblank(Infile)
       IF(nfil.gt.3)THEN
        ext=Infile((nfil-3):nfil)
        IF((ext(1:2).eq.'.m'.or.ext(1:2).eq.'.M').and.
     &    (ext(3:3).eq.'t'.or.ext(3:3).eq.'T').and. 
     &    (ext(4:4).eq.'a'.or.ext(4:4).eq.'A'))THEN
         WRITE(STDERR,1010)'input',ext
         ok=.false.
        END IF
       END IF
       IF(ok)THEN
        Mtafil=Infile(1:nfil)//'.mta'
        nfil=nfil+4
        WRITE(STDOUT,1000)
        CALL fopen(Mtafil(1:nfil),'input metafile','OLD',meta,ok)
       END IF
      END IF
      IF(.not.ok)THEN
       CALL abend
       RETURN
      END IF
C-----------------------------------------------------------------------
c     Read each line of the metafile
C-----------------------------------------------------------------------
      Imeta=0
      DO WHILE (.true.)
       READ(meta,'(a)',END=10,ERR=20)mtalin
       nmeta=nblank(mtalin)
       IF(Imeta.lt.PSRS)THEN
        Imeta=Imeta+1
       ELSE
        IF(nmeta.gt.0)THEN
         IF(Ldata)THEN
          WRITE(STDERR,1040)PSRS
         ELSE
          WRITE(STDERR,1050)PSRS
         END IF
         GO TO 10
        END IF
       END IF
       Outsrs(Imeta)=blnk
C-----------------------------------------------------------------------
c     If this is a blank line (line of length zero), decrement the 
c     series counter and process the next line. 
C-----------------------------------------------------------------------
       IF(nmeta.eq.0)THEN
        Imeta=Imeta-1
       ELSE
C-----------------------------------------------------------------------
c     If the first character of the line is a quotation mark,
c     Find the next quotation mark.
c     November 2005 - BCM
C-----------------------------------------------------------------------
        IF(mtalin(1:1).eq.quot)THEN
         i=2
         DO WHILE (mtalin(i:i).ne.quot.and.i.le.nmeta)
          i=i+1
         END DO
         IF (i.eq.nmeta.and.mtalin(nmeta:nmeta).ne.quot)THEN
          IF(Ldata)THEN
           WRITE(STDERR,1021)'data',Mtafil(1:nfil)
          ELSE
           WRITE(STDERR,1021)'input',Mtafil(1:nfil)
          END IF
          CALL abend
          RETURN
         END IF
C-----------------------------------------------------------------------
c     Set the length of the first string.  
C-----------------------------------------------------------------------
         n=i
         n1=2
         n2=n-1
        ELSE
C-----------------------------------------------------------------------
c     Find the first blank or not set character
C-----------------------------------------------------------------------
         i=1
         DO WHILE (mtalin(i:i).ne.blnk.and.i.le.nmeta)
          i=i+1
         END DO
C-----------------------------------------------------------------------
c     If the first character of a line is a blank character, print an 
c     error message 
C-----------------------------------------------------------------------
         IF(i.eq.1)THEN
          IF(Ldata)THEN
           WRITE(STDERR,1020)' data',Mtafil(1:nfil)
          ELSE
           WRITE(STDERR,1020)'n input',Mtafil(1:nfil)
          END IF
          CALL abend
          RETURN
         END IF
C-----------------------------------------------------------------------
c     Set the length of the first string.  
C-----------------------------------------------------------------------
         n=i-1
         n1=1
         n2=n
        END IF
C-----------------------------------------------------------------------
c     If this is an input metafile, store the series name in the 
c     variable series.  Else, store as an element of Dtasrs
C-----------------------------------------------------------------------
        IF(Ldata)THEN
         Datsrs(Imeta)=mtalin(n1:n2)
         Insrs(Imeta)=Infile
        ELSE
         Insrs(Imeta)=mtalin(n1:n2)
        END IF
C-----------------------------------------------------------------------
c     Is the end of the first string the end of the line?  If so,
c     set output names.
C-----------------------------------------------------------------------
        IF(nmeta.eq.n)THEN
c     ------------------------------------------------------------------
c     If data metafile is used, get the path and filename from the 
c     datafile to use as the output file name.
c     ------------------------------------------------------------------
         IF(Ldata)THEN
          ichr=lstpth(mtalin,n)+1
          DO i2=n2,ichr,-1
           IF(mtalin(i2:i2).eq.'.')THEN
            nchr=i2-1
            GO TO 30
           END IF
          END DO
          nchr=n2
   30     Outsrs(Imeta)=mtalin(n1:nchr)
         ELSE
c     ------------------------------------------------------------------
c     If an input metafile is used, set the output file to be the same 
c     as the spec file.
c     ------------------------------------------------------------------
          Outsrs(Imeta)=mtalin(n1:n2)
         END IF
C-----------------------------------------------------------------------
c     If not, find the position of the next non-blank character
C-----------------------------------------------------------------------
        ELSE
         IF(mtalin(i:i).eq.quot)i=i+1
         DO WHILE (mtalin(i:i).eq.blnk)
          i=i+1
         END DO
C-----------------------------------------------------------------------
C     Check to see if there are any more blanks in the line
C-----------------------------------------------------------------------
         j=i
         IF(mtalin(j:j).eq.quot)THEN
          j=j+1
          DO WHILE (mtalin(j:j).ne.quot.and.j.le.nmeta)
           j=j+1
          END DO
          IF (i.eq.nmeta.and.mtalin(nmeta:nmeta).ne.quot)THEN
           IF(Ldata)THEN
            WRITE(STDERR,1021)'data',Mtafil(1:nfil)
           ELSE
            WRITE(STDERR,1021)'input',Mtafil(1:nfil)
           END IF
           CALL abend
           RETURN
          END IF
C-----------------------------------------------------------------------
c     Store the output file name in the array Outsrs
C-----------------------------------------------------------------------
          Outsrs(Imeta)=mtalin((i+1):(j-1))
         ELSE
C-----------------------------------------------------------------------
          DO WHILE (mtalin(j:j).ne.blnk.and.j.le.nmeta)
           j=j+1
          END DO
C-----------------------------------------------------------------------
c     Store the output file name in the array Outsrs
C-----------------------------------------------------------------------
          Outsrs(Imeta)=mtalin(i:(j-1))
         END IF
        END IF
       END IF
      END DO
C-----------------------------------------------------------------------
c     Close metafile and return to main driver
C-----------------------------------------------------------------------
   10 CALL fclose(meta)
      RETURN
C-----------------------------------------------------------------------
c     print error message for read error
C-----------------------------------------------------------------------
   20 IF(Ldata)THEN
       WRITE(STDERR,1030)'a data ',Mtafil(1:nfil)
      ELSE
       WRITE(STDERR,1030)'an input ',Mtafil(1:nfil)
      END IF
      CALL abend
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT(' ')
 1010 FORMAT(/,' ERROR: Enter ',a,' metafile name without "',a,
     &         '" file extension.')
 1020 FORMAT(/,' ERROR: The first entry in each line of a',a,
     &         ' metafile must be left ',
     &       /,'        justified.  Correct the metafile and rerun ',a,
     &         '.')
 1021 FORMAT(/,' ERROR: Closing quotation mark not found in this ',a,
     &         ' metafile.',
     &       /,'        Correct the metafile and rerun ',a,'.')
 1030 FORMAT(/,' ERROR: Read error encountered in ',a,'metafile ',a,'.')
 1040 FORMAT(' WARNING: Number of series in data metafile exceeds ',
     &       'program limit.',/
     &      '          Only the first ',i5,' series will be processed.')
 1050 FORMAT(' WARNING: Number of spec files in metafile exceeds ',
     &       'program limit.',/
     &      '          Only the first ',i5,' series will be processed.')
      END
