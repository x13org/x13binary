**==fstop.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE fstop()
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Opens a file with the given options and assigns a file handle
c-----------------------------------------------------------------------
c Parameters and include files
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
c-----------------------------------------------------------------------
c Local Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c ifile   i  Index for the current file
c-----------------------------------------------------------------------
      INTEGER ifile
c-----------------------------------------------------------------------
c     Close all the open files and stop
c-----------------------------------------------------------------------
      DO ifile=1,Nfile
       CLOSE(Fillst(ifile))
      END DO
      Nfile=0
c     ------------------------------------------------------------------
      RETURN
      END
