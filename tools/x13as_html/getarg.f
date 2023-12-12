C     Last change:  BCM   2 Dec 97    7:19 am
      SUBROUTINE getarg(N,Chr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Matches UNIX getarg subroutine p. 280 in 3F Sun OS Manuel.
c Note, this does not return the command name when narg is 0.
c-----------------------------------------------------------------------
      INCLUDE 'getarg.prm'
      INCLUDE 'getarg.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      INTEGER N
      CHARACTER Chr*(CLEN)
c-----------------------------------------------------------------------
      LOGICAL frstar
      SAVE frstar
      DATA frstar/.true./
c-----------------------------------------------------------------------
C     Call subroutine which sets up argument list (PC Version)
C-----------------------------------------------------------------------
      IF(frstar)THEN
       CALL setarg()
       IF(Lfatal)RETURN
       frstar=.false.
      END IF
c-----------------------------------------------------------------------
      Chr='                                                             
     &
     &'
      if (Narg.ge.N) Chr = Arg(Ptr(n-1):(ptr(n)-1))
      RETURN
      END 
       
