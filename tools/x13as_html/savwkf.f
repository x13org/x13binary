      SUBROUTINE savwkf(Nptr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Save the Wiener-Kolmogrov filters from SEATS into a file.
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER PSTLEN
      PARAMETER(T=.true.,F=.false.,PSTLEN=120)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'cchars.i'
      INCLUDE 'seatmd.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER str*(PSTLEN)
      INTEGER fh,ipos,icol,nlen,ncol,Nptr
      LOGICAL locok
c-----------------------------------------------------------------------
c	open file
c-----------------------------------------------------------------------
      CALL opnfil(T,F,Nptr,fh,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Print header for end filters
c-----------------------------------------------------------------------
      CALL setchr(' ',PSTLEN,str)
      str(1:3)='lag'
      ipos=4
      nlen=NOTSET
      ncol=0
      IF(Ntcwkf.gt.0)THEN
       str(ipos:ipos)=TABCHR
       ipos=ipos+1
       IF(nlen.eq.NOTSET)nlen=Ntcwkf
       str(ipos:ipos+11)='TC_WK_Filter'
       ipos=ipos+12
       ncol=ncol+1
      END IF
      IF(Nsawkf.gt.0)THEN
       str(ipos:ipos)=TABCHR
       ipos=ipos+1
       IF(nlen.eq.NOTSET)nlen=Nsawkf
       str(ipos:ipos+11)='SA_WK_Filter'
       ipos=ipos+12
       ncol=ncol+1
      END IF
      IF(Nswkf.gt.0)THEN
       str(ipos:ipos)=TABCHR
       ipos=ipos+1
       IF(nlen.eq.NOTSET)nlen=Nswkf
       str(ipos:ipos+10)='S_WK_Filter'
       ipos=ipos+11
       ncol=ncol+1
      END IF
      IF(Ntrwkf.gt.0)THEN
       str(ipos:ipos)=TABCHR
       ipos=ipos+1
       IF(nlen.eq.NOTSET)nlen=Ntrwkf
       str(ipos:ipos+11)='TR_WK_Filter'
       ipos=ipos+12
       ncol=ncol+1
      END IF
      IF(Nirwkf.gt.0)THEN
       str(ipos:ipos)=TABCHR
       ipos=ipos+1
       IF(nlen.eq.NOTSET)nlen=Nirwkf
       str(ipos:ipos+12)='IRR_WK_Filter'
       ipos=ipos+13
       ncol=ncol+1
      END IF
      WRITE(fh,1000)str(1:(ipos-1))
      WRITE(fh,1000)'---',
     &              (TABCHR,'-----------------------',icol=1,ncol)
c-----------------------------------------------------------------------
      DO icol=1,nlen
       ipos=1
       CALL itoc(icol,str,ipos)
       IF(Ntcwkf.gt.0)THEN
        str(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Tcwkf(icol),str,ipos)
        IF(Lfatal)RETURN
       END IF
       IF(Nsawkf.gt.0)THEN
        str(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Sawkf(icol),str,ipos)
        IF(Lfatal)RETURN
       END IF
       IF(Nswkf.gt.0)THEN
        str(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Swkf(icol),str,ipos)
        IF(Lfatal)RETURN
       END IF
       IF(Ntrwkf.gt.0)THEN
        str(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Trwkf(icol),str,ipos)
        IF(Lfatal)RETURN
       END IF
       IF(Nirwkf.gt.0)THEN
        str(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Irwkf(icol),str,ipos)
        IF(Lfatal)RETURN
       END IF
       WRITE(fh,1000)str(1:(ipos-1))
      END DO
c-----------------------------------------------------------------------
 1000 FORMAT(1000a)
      CALL fclose(fh)
      RETURN
      END
