C     Last change:  BCM  15 Jan 98   12:01 pm
**==savspp.f    processed by SPAG 4.03F  at 10:40 on 20 Oct 1994
      SUBROUTINE savspp(Itbl,Sx,Frq,Nfrq,Lab,Lgraf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      CHARACTER outstr*50,Lab*(*)
      INTEGER fh,i,ipos,Itbl,Nfrq
      DOUBLE PRECISION Sx,Frq
      DIMENSION Sx(0:(Nfrq-1)),Frq(0:(Nfrq-1))
      LOGICAL lok,Lgraf
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
c     Check to see if this is a spectrum.  If so, store in a special
c     /rdb format.
c     ------------------------------------------------------------------
      CALL opnfil(.true.,Lgraf,Itbl,fh,lok)
      IF(.not.lok)THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Print header information based on type of spectral plot
c-----------------------------------------------------------------------
      WRITE(fh,1010)'Pos',TABCHR,'Frequency',TABCHR,Lab
      WRITE(fh,1010)'---',TABCHR,'-----------------------',TABCHR,
     &              '-----------------------'
c-----------------------------------------------------------------------
c     Save spectrum
c-----------------------------------------------------------------------
      DO i=0,Nfrq-1
       ipos=1
       CALL itoc(i,outstr,ipos)
       IF(Lfatal)RETURN
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       CALL dtoc(Frq(i),outstr,ipos)
       IF(Lfatal)RETURN
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       CALL dtoc(Sx(i),outstr,ipos)
       IF(Lfatal)RETURN
       WRITE(fh,1010)outstr(1:ipos-1)
      END DO
c-----------------------------------------------------------------------
      CALL fclose(fh)
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(a:,a,a,a,a)
      END

