      SUBROUTINE svfltd(Vfreq,Vdiag,Fltptr,Lgraf,Fltidx,Flthdr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.TRUE.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION vfreq(0:1200),vdiag(0:1200,2)
      LOGICAL locok,Lgraf
      INTEGER i,fhc,ipos,Fltptr,Fltidx
      CHARACTER flthdr*(*),outstr*(50)
c-----------------------------------------------------------------------
      CALL opnfil(T,Lgraf,Fltptr,fhc,locok)
      IF (.not.locok) THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
      write(fhc,1000)'freq',TABCHR,Flthdr
      WRITE(fhc,1000)'----------------------',TABCHR,
     &               '----------------------'
c-----------------------------------------------------------------------
      do 400 i=0,1200
       ipos=1
       CALL setchr(' ',50,outstr)
       CALL dtoc(Vfreq(i),outstr,ipos)
       IF(Lfatal)RETURN
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       CALL dtoc(Vdiag(i,Fltidx),outstr,ipos)
       IF(Lfatal)RETURN
       write(fhc,1001)outstr(1:(ipos-1))
 400  continue
c-----------------------------------------------------------------------
      CALL fclose(fhc)
c-----------------------------------------------------------------------
 1000 format(3a)
 1001 format(a)
c-----------------------------------------------------------------------
      RETURN
      END
       