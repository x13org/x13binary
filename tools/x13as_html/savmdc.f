      SUBROUTINE savmdc(Nptr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Save the Wiener-Kolmogrov filters from SEATS into a file.
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
*      INCLUDE 'cchars.i'
      INCLUDE 'seatmd.cmn'
*      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      INTEGER fh,icol,Nptr
      LOGICAL locok
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c	open file
c-----------------------------------------------------------------------
      CALL opnfil(T,F,Nptr,fh,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c	Save model estimates associated with the trend cycle
c-----------------------------------------------------------------------
      IF(Ntcnum.gt.0)THEN
       WRITE(fh,1010)'ntcnum: ',Ntcnum
       DO icol=1,Ntcnum
        WRITE(fh,1020)'tcnum.',icol-1,Tcnum(icol)
       END DO
      END IF
      IF(Ntcden.gt.0)THEN
       WRITE(fh,1010)'ntcden: ',Ntcden
       DO icol=1,Ntcden
        WRITE(fh,1020)'tcden.',icol-1,Tcden(icol)
       END DO
      END IF
      IF(.not.dpeq(Tcvar,DNOTST))WRITE(fh,1030)'tcvar: ',Tcvar
c-----------------------------------------------------------------------
c	Save model estimates associated with the seasonal
c-----------------------------------------------------------------------
      IF(Nsnum.gt.0)THEN
       WRITE(fh,1010)'nsnum: ',Nsnum
       DO icol=1,Nsnum
        WRITE(fh,1020)'snum.',icol-1,Snum(icol)
       END DO
      END IF
      IF(Nsden.gt.0)THEN
       WRITE(fh,1010)'nsden: ',Nsden
       DO icol=1,Nsden
        WRITE(fh,1020)'sden.',icol-1,Sden(icol)
       END DO
      END IF
      IF(.not.dpeq(Svar,DNOTST))WRITE(fh,1030)'svar: ',Svar
c-----------------------------------------------------------------------
c	save model estimates associated with the seasonally adjusted
c       component
c-----------------------------------------------------------------------
      IF(Nsanum.gt.0)THEN
       WRITE(fh,1010)'nsanum: ',Nsanum
       DO icol=1,Nsanum
        WRITE(fh,1020)'sanum.',icol-1,Sanum(icol)
       END DO
      END IF
      IF(Nsaden.gt.0)THEN
       WRITE(fh,1010)'nsaden: ',Nsaden
       DO icol=1,Nsaden
        WRITE(fh,1020)'saden.',icol-1,Saden(icol)
       END DO
      END IF
      IF(.not.dpeq(Savar,DNOTST))WRITE(fh,1030)'savar: ',Savar
c-----------------------------------------------------------------------
c	Transitory component estimates
c-----------------------------------------------------------------------
      IF(Ntrnum.gt.0)THEN
       WRITE(fh,1010)'ntrnum: ',Ntrnum
       DO icol=1,Ntrnum
        WRITE(fh,1020)'trnum.',icol-1,Trnum(icol)
       END DO
      END IF
      IF(Ntrden.gt.0)THEN
       WRITE(fh,1010)'ntrden: ',Ntrden
       DO icol=1,Ntrden
        WRITE(fh,1020)'trden.',icol-1,Trden(icol)
       END DO
      END IF
      IF(.not.dpeq(Trvar,DNOTST))WRITE(fh,1030)'trvar: ',Trvar
c-----------------------------------------------------------------------
c	irregular estimates
c-----------------------------------------------------------------------
      IF(.not.dpeq(Irrvar,DNOTST))WRITE(fh,1030)'irrvar: ',Irrvar
c-----------------------------------------------------------------------
 1010 FORMAT(a,i3)
 1020 FORMAT(a,i3.3,': ',e22.15)
 1030 FORMAT(a,e22.15)
c-----------------------------------------------------------------------
      RETURN
      END

