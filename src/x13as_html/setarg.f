C     Last change:  BCM   2 Dec 97    7:19 am
      SUBROUTINE setarg()
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Set up pointer, character variables for getarg routines.
c     BCM November 2005 -
c       Allow quotation marks surrounding arguments to include
c       spaces in director/file names
c-----------------------------------------------------------------------
      INCLUDE 'getarg.prm'
      INCLUDE 'getarg.cmn'
      INCLUDE 'stdio.i'
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      CHARACTER cmdln*(CLEN)
      INTEGER frstch,lastch,next,xlen,nblnk,nquote
      LOGICAL lquote
c-----------------------------------------------------------------------
      Arg = 
     &   '                                                             '
      CALL getcl(cmdln)
c     ------------------------------------------------------------------
      frstch = 1
      next = 1
      Narg = 0
      Ptr(Narg) = 1
      nblnk = 0
c   ---  initialize variables to allow for quotation marks
c   ---  BCM - November 2005
      lquote = F
      nquote = 0
c     ------------------------------------------------------------------
   10 lastch = index(cmdln(frstch:CLEN) , ' ') + frstch - 1
      xlen = lastch - frstch + 1
      IF( xlen.eq.1 )RETURN
c   ---  if a quotation mark is found in an earlier argument,
c   ---  check to see if the first character is a quotation mark and
c   ---  print out message to correct program flags and stop processing
c   ---  BCM - November 2005
      IF(lquote)THEN
       IF(cmdln(frstch:frstch).eq.'"')THEN
        WRITE(STDERR,1010)
     &   ' ERROR: Improper number of quotation marks in program flags.'
        WRITE(STDERR,1020)
     &   '        Check position of quotation marks in flags.'
        CALL abend
        RETURN
       END IF
      ELSE
c   ---  if a quotation mark is not found in an earlier argument,
c   ---  check to see if the first character is a quotation mark
c   ---  BCM - November 2005
       lquote = cmdln(frstch:frstch).eq.'"'
       IF(lquote)THEN
c   ---  if a quotation mark is found in the first character,
c   ---  adjust pointer variables and set lquote to true
c   ---  BCM - November 2005
        frstch = frstch + 1
        xlen = xlen - 1
        nquote = nquote + 1
       END IF
      END IF
      IF(cmdln((lastch-1):(lastch-1)).eq.'"') THEN
c   ---  if a quotation mark is found in the final character,
c   ---  adjust pointer variables and set lquote to false
c   ---  BCM - November 2005
       lastch=lastch-2
       xlen = xlen - 1
       nquote = nquote + 1
       lquote = F
      END IF
      Arg(next:next + xlen - 1) = cmdln(frstch:lastch)
      next = next + xlen
c   ---  only update pointers of data dictionary if closing quote is
c   ---  not found or no quotation marks found
c   ---  BCM - November 2005
      IF(.not.lquote)THEN
       Narg = Narg + 1
       Ptr(Narg) = next
      END IF
      frstch = next + nblnk + nquote
      DO WHILE ( .true. )
        IF( cmdln(frstch:frstch).eq.' ' )THEN
          frstch = frstch + 1
c   ---  if a quotation mark was found, add space to data dictionary
c   ---  BCM - November 2005
          IF(lquote)THEN
           Arg(next:next)=' '
           next=next+1
          ELSE
           nblnk = nblnk + 1
          END IF
          IF( frstch.le.CLEN )GO TO 20
c   ---  if a quotation mark was found and a closing quote is not found,
c   ---  print out error message and stop processing.
c   ---  BCM - November 2005
          IF(lquote)THEN
           WRITE(STDERR,1010)
     &      ' ERROR: Closing quotation mark not found in program flags.'
           CALL abend
          END IF
          RETURN
        END IF
        GO TO 10
   20   CONTINUE
      END DO
 1010 FORMAT(/,a)
 1020 FORMAT(a)
      END
