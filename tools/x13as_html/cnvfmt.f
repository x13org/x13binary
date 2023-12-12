C     Last change:  BCM  12 Mar 98    9:51 am
      SUBROUTINE cnvfmt(Base,Xfmt,Fobs,Fsum,Fpos,Nfmt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     converts base format to format used in X-13A-S table routine.
c     Author: Brian C. Monsell
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      CHARACTER base*(110),xfmt*(110),Fobs*(*),Fsum*(*)
      INTEGER begpos,begxf,Fpos,nfmt,i,n1,n2
c-----------------------------------------------------------------------
      n1=nblank(Fobs)
      n2=nblank(Fsum)
c-----------------------------------------------------------------------
c     initialize starting position of base segment, x12 format segment.
c-----------------------------------------------------------------------
      begpos=1
      begxf=1
c-----------------------------------------------------------------------
c     loop through base format
c-----------------------------------------------------------------------
      DO i=1,Fpos
c-----------------------------------------------------------------------
c     search for conversion code (@ for observation fmt, # for summary
c     fmt).
c-----------------------------------------------------------------------
       IF(Base(i:i).eq.'@'.or.Base(i:i).eq.'#')THEN
c-----------------------------------------------------------------------
c     When code is found, update x12 format with base format before the
c     last occurance of the code.
c-----------------------------------------------------------------------
        Xfmt(begxf:(begxf+i-begpos-1))=Base(begpos:(i-1))
        begxf=begxf+i-begpos
c-----------------------------------------------------------------------
c     Insert proper format instead of code
c-----------------------------------------------------------------------
        IF(Base(i:i).eq.'@')THEN
         Xfmt(begxf:(begxf+n1-1))=Fobs
         begxf=begxf+n1
        ELSE
         Xfmt(begxf:(begxf+n2-1))=Fsum
         begxf=begxf+n2
        END IF
        begpos=i+1
c-----------------------------------------------------------------------
c     if i is last position of base format, append the final portion
c     of the format to the x12 format.
c-----------------------------------------------------------------------
       ELSE IF(i.eq.Fpos)THEN
        Xfmt(begxf:(begxf+i-begpos))=Base(begpos:i)
        Nfmt=begxf+i-begpos
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
