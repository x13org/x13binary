C     Last change:  BCM   6 Aug 1998    7:33 am
      SUBROUTINE getdat(Havesp,Sp,Idate,Argok,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     getdat.f, Release 1, Subroutine Version 1.3, Modified 20 Oct 1994.
c-----------------------------------------------------------------------
c     Puts the date in character format for outlier variables and
c printouts.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'lex.i'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      LOGICAL Argok,Havesp,Inptok
      CHARACTER datstr*11
      INTEGER Idate,ipos,llstps,nchr,Sp
      DIMENSION Idate(2),llstps(2)
c     ------------------------------------------------------------------
      Argok=T
      ipos=1
      CALL cpyint(Lstpos,2,1,llstps)
c-----------------------------------------------------------------------
c     Initialize datstr and nchr - change by BCM June 2003
c-----------------------------------------------------------------------
      CALL setchr(' ',11,datstr)
      nchr=1
c-----------------------------------------------------------------------
c     Case for nonseasonal data
c-----------------------------------------------------------------------
      IF(Nxtktp.eq.INTGR)THEN
       IF(Havesp.and.Sp.ne.1)THEN
        CALL inpter(PERRNP,Lstpos,
     &            'Invalid date, seasonal period of data not annual.',T)
        Argok=F
c     ------------------------------------------------------------------
       ELSE
        IF(.not.Havesp)THEN
         Havesp=T
         Sp=1
        END IF
        nchr=Nxtkln
        datstr=Nxttok(1:Nxtkln)
       END IF
c-----------------------------------------------------------------------
c     Case for monthly  data because monthly abbreviations used.
c We know this because lex didn't pull off anything after the decimal,
c ie if there were a numeric period 67.3 then it would have pick up
c the period also.
c-----------------------------------------------------------------------
      ELSE IF(Nxtktp.eq.DBL)THEN
       IF(Nxttok(Nxtkln:Nxtkln).ne.'.')THEN
        IF(Havesp.and.Sp.eq.1)THEN
         CALL inpter(PERROR,llstps,
     &               'Invalid date, no period for nonseasonal data',T)
         Argok=F
c     ------------------------------------------------------------------
        ELSE IF(.not.Havesp)THEN
         Sp=PSP
        END IF
c     ------------------------------------------------------------------
        nchr=Nxtkln
        datstr=Nxttok(1:Nxtkln)
       ELSE IF(Havesp.and.Sp.ne.12)THEN
        CALL inpter(PERROR,Lstpos,
     &           'Invalid date, seasonal period of data not monthly.',T)
        Argok=F
c     ------------------------------------------------------------------
       ELSE
        IF(.not.Havesp)THEN
         Havesp=T
         Sp=12
        END IF
        nchr=Nxtkln
        datstr=Nxttok(1:Nxtkln)
        CALL lex()
c     ------------------------------------------------------------------
        IF(Nxtktp.ne.NAME)THEN
         CALL inpter(PERROR,llstps,
     &               'Invalid date, expected a monthly abbreviation',T)
         Argok=F
c-----------------------------------------------------------------------
c     Case for seasonal but we can't determine the period.  The default
c if monthly though
c-----------------------------------------------------------------------
        ELSE
         datstr(nchr+1:nchr+Nxtkln)=Nxttok(1:Nxtkln)
         nchr=nchr+Nxtkln
        END IF
c     ------------------------------------------------------------------
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Argok)THEN
       CALL ctodat(datstr(1:nchr),Sp,ipos,Idate,Argok)
c-----------------------------------------------------------------------
c     Check to see if quotes are found, change error message to mention
c     quotes if they are found - change by BCM June 2003
c-----------------------------------------------------------------------
       IF(.not.Argok)THEN
        IF(Nxtktp.eq.QUOTE)THEN
         CALL inpter(PERROR,Lstpos,
     &               'Not a valid date - remove quotes.',T)
        ELSE
         CALL inpter(PERROR,Lstpos,'Not a valid date',T)
        END IF
       END IF
c-----------------------------------------------------------------------
      END IF
      CALL lex()
c     ------------------------------------------------------------------
      Inptok=Argok.and.Inptok
      RETURN
      END
