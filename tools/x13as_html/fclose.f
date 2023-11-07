C     Last change:  BCM  28 Oct 97    4:14 pm
**==fclose.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE fclose(Handle)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Opens a file with the given options and assigns a file handle
c-----------------------------------------------------------------------
c Parameters and include files
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
c-----------------------------------------------------------------------
c Namelist Input Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c handle  i  Unit number of the next available file
c-----------------------------------------------------------------------
      INTEGER Handle
c-----------------------------------------------------------------------
c Local Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c ifile   i  Index for the current file handle
c jfile   i  Index for the current file
c-----------------------------------------------------------------------
      INTEGER ifile,jfile
c-----------------------------------------------------------------------
c     If handle=ALLFIL close all the files
c-----------------------------------------------------------------------
      IF(Handle.eq.ALLFIL)THEN
       DO WHILE (Nfile.gt.1)
        CLOSE(Fillst(Nfile))
        Nfile=Nfile-1
       END DO
       IF(Opnsin)CLOSE(STDIN)
       IF(Opnsot)CLOSE(STDOUT)
c-----------------------------------------------------------------------
c     Else close the file with the unit number handle
c-----------------------------------------------------------------------
      ELSE IF(Handle.eq.STDIN)THEN
       IF(Opnsin)CLOSE(STDIN)
      ELSE IF(Handle.eq.STDOUT)THEN
       IF(Opnsot)CLOSE(STDOUT)
      ELSE
       DO ifile=1,Nfile
        IF(Fillst(ifile).eq.Handle)THEN
c-----------------------------------------------------------------------
c     If the file is in the list of open files then close it and
c put the file handle in the unopened files part of the list
c-----------------------------------------------------------------------
         CLOSE(Handle)
c     ------------------------------------------------------------------
         DO jfile=ifile,Nfile-1
          Fillst(jfile)=Fillst(jfile+1)
         END DO
c     ------------------------------------------------------------------
         Fillst(Nfile)=Handle
         Nfile=Nfile-1
         GO TO 10
        END IF
       END DO
c     ------------------------------------------------------------------
       PRINT 1010,Handle
c Later we are going to want to call the files by there file names
c because the user won't know what the unit numbers are.
 1010  FORMAT(/,' File',i3,' not found to close')
      END IF
c     ------------------------------------------------------------------
   10 RETURN
      END
