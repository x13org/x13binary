C     Last change:  BCM  23 Sep 1998    2:53 pm
**==fopen.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE fopen(Fil,Fildes,Flstat,Handle,Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Opens a file with the given options and assigns a file handle
c Parameters and include files
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
c PFILE   i  Maximum number of files that can be opened at a time
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(T=.true.,F=.FALSE.)
C-----------------------------------------------------------------------
c Namelist Input Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c fil     c  Filename
c handle  i  Unit number of the next available file
c Flstat    c  Status of the file, either new, old, unknown, or scratch
c-----------------------------------------------------------------------
      CHARACTER Fildes*(*),Fil*(*),Flstat*(*)
      INTEGER Handle
c-----------------------------------------------------------------------
c Local Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c ifile   i  Index for the current file
c intfil  l  Switch to set up the file list first time through
c flpext  c  Constructed file name plus the extension if there is one
c nextcr  i  Number of characters in the extension
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      LOGICAL Locok,intfil
      INTEGER ifile,nchr
      SAVE intfil
      DATA intfil/.true./
c-----------------------------------------------------------------------
c     If this is the first call to the routine then initialize the list
c of files.
c-----------------------------------------------------------------------
      Locok=T
      IF(intfil)THEN
       DO ifile=1,PFILE
        Fillst(ifile)=ifile+9
       END DO
c-----------------------------------------------------------------------
c     Changed because of log file (BCM Dec 1994)
c-----------------------------------------------------------------------
       Nfile=1
c-----------------------------------------------------------------------
       intfil=F
       Opnsin=F
       Opnsot=F
      END IF
c-----------------------------------------------------------------------
c     Length of the file name
c-----------------------------------------------------------------------
      nchr=nblank(Fil)
c-----------------------------------------------------------------------
c     Open standard in.  Note that a FORTRAN file cannot be opened with
c read only status. This may cause a file without write permissions to
c fail.
c-----------------------------------------------------------------------
      IF(Handle.eq.STDIN)THEN
       IF(Infile.ne.'STDIN')THEN
        OPEN(UNIT=Handle,FILE=Fil(1:nchr),STATUS='OLD',ERR=10)
        Opnsin=T
       END IF
c     ------------------------------------------------------------------
c     Open Standard out
c-----------------------------------------------------------------------
      ELSE IF(Handle.eq.STDOUT)THEN
       IF(Infile.ne.'STDOUT')OPEN(UNIT=Handle,FILE=Fil(1:nchr),STATUS=
     &                            'UNKNOWN',ERR=10)
       Opnsot=T
c     ------------------------------------------------------------------
      ELSE IF(Nfile.ge.PFILE)THEN
       WRITE(STDERR,1010)Nfile,PFILE
 1010  FORMAT(/,' ERROR: Too many open files',i3,'>',i3,'.')
       IF(Mt2.gt.0)THEN
        CALL errhdr
        WRITE(Mt2,1011)Nfile,PFILE
       END IF
 1011  FORMAT(/,'<p><strong>ERROR:</strong> Too many open files ',i3,
     &          ' > ',i3,'.</p>')
       GO TO 20
c     ------------------------------------------------------------------
      ELSE
       Nfile=Nfile+1
       Handle=Fillst(Nfile)
       OPEN(UNIT=Handle,FILE=Fil(1:nchr),STATUS=Flstat,ERR=10)
      END IF
c-----------------------------------------------------------------------
c     Write out the the file and description
c-----------------------------------------------------------------------
      IF(Flstat(1:3).eq.'OLD'.or.Flstat(1:3).eq.'old')THEN
       IF(Mt1.gt.0)THEN
        WRITE(Mt1,*)' <p>Reading ',Fildes,' from <em>',Fil(1:nchr),
     &              '</em></p>'
       ELSE
        WRITE(STDOUT,*)' Reading ',Fildes,' from ',Fil(1:nchr)
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
c     Error return
c-----------------------------------------------------------------------
   10 IF(Flstat.eq.'NEW'.or.Flstat.eq.'new')THEN
       WRITE(STDERR,1020)Fildes,Fil(1:nchr)
 1020  FORMAT(/,' ERROR: ',a,' ',a,' already exists.',/)
       IF(Mt2.gt.0)THEN
        CALL errhdr
        WRITE(Mt2,1021)Fildes,Fil(1:nchr)
       END IF
 1021  FORMAT(/,' <p><strong>ERROR:</strong> ',a,' ',a,
     &          ' already exists.</p>',/)
c     ------------------------------------------------------------------
      ELSE
       WRITE(STDERR,1030)Fildes,Fil(1:nchr)
 1030  FORMAT(/,' ERROR: Unable to open ',a,', ',a,'.',/)
       IF(Mt2.gt.0.and.Mt2.ne.Handle)THEN
        CALL errhdr
        WRITE(Mt2,1031)Fildes,Fil(1:nchr)
       END IF
 1031  FORMAT(/,' <p><strong>ERROR:</strong> Unable to open ',a,', ',a,
     &          '.</p>',/)
      END IF
c     ------------------------------------------------------------------
   20 Locok=F
      RETURN
      END
