      SUBROUTINE svflt(Pos1ob,Posfob,Vflt,Fltptr,Lgraf,Fltidx,Flthdr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.TRUE.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Vflt(1200,2)
      LOGICAL locok,Lgraf
      INTEGER i,fhc,ipos,Fltptr,Fltidx,Pos1ob,Posfob,nz
      CHARACTER flthdr*(*),outstr*(50)
c-----------------------------------------------------------------------
      CALL opnfil(T,Lgraf,Fltptr,fhc,locok)
      IF (.not.locok) THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
      write(fhc,1000)'index',TABCHR,Flthdr
      WRITE(fhc,1000)'-----',TABCHR,'----------------------'
c-----------------------------------------------------------------------
      nz=Posfob-Pos1ob+1
      do 400 i=1,nz
       ipos=1
       CALL setchr(' ',50,outstr)
       CALL itoc(1-i,outstr,ipos)
       IF(Lfatal)RETURN
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       CALL dtoc(Vflt(i,Fltidx),outstr,ipos)
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
       