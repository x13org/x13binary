C     Last change:  BCM   2 Feb 98    9:10 am
      INTEGER FUNCTION strinx(Chksub,Chrvec,Ptrvec,Begstr,Endstr,Str)
c-----------------------------------------------------------------------
c     strinx.f, Release 1, Subroutine Version 1.4, Modified 20 Oct 1994.
c-----------------------------------------------------------------------
c     Return the index value that matches the string str or 0
c----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'lex.i'
      CHARACTER Chrvec*(*),Str*(*)
      LOGICAL Chksub,cmpstr
      INTEGER begchr,Begstr,endchr,nchrm1,Endstr,Ptrvec
      DIMENSION Ptrvec(0:Endstr)
      EXTERNAL cmpstr
c     -----------------------------------------------------------------
      nchrm1=len(Str)-1
      DO strinx=Begstr,Endstr
       begchr=Ptrvec(strinx-1)
       endchr=Ptrvec(strinx)-1
       IF(Chksub)endchr=min(endchr,begchr+nchrm1)
c----------------------------------------------------------------------
c* This will make the grammer case sensitive but it didn't improve
c* the speed so we didn't implement it.
c*       IF(Str.eq.Chrvec(begchr:endchr))GO TO 10
c----------------------------------------------------------------------
       IF(endchr.ge.begchr.and.cmpstr(NAME,Str,Chrvec(begchr:endchr)))
     &   GO TO 10
      END DO
      strinx=0
c     -----------------------------------------------------------------
   10 RETURN
      END

