      SUBROUTINE mkspky(Itbl,Spcstr,Nspstr,Iagr,Lseats)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Itbl,Nspstr,Iagr,Lsumm
      LOGICAL Lseats
      CHARACTER Spcstr*(10)
c-----------------------------------------------------------------------
      IF(Itbl.eq.1)THEN
       IF(Iagr.lt.4)THEN
        Nspstr=6
        Spcstr(1:Nspstr)='spcori'
       ELSE
        Nspstr=7
        Spcstr(1:Nspstr)='spccomp'
       END IF
      ELSE IF (Itbl.eq.2)THEN
       IF(Iagr.lt.4)THEN
        Nspstr=5
        Spcstr(1:Nspstr)='spcsa'
       ELSE
        Nspstr=8
        Spcstr(1:Nspstr)='spcindsa'
       END IF
      ELSE IF (Itbl.eq.3)THEN
       IF(Iagr.lt.4)THEN
        Nspstr=6
        Spcstr(1:Nspstr)='spcirr'
       ELSE
        Nspstr=9
        Spcstr(1:Nspstr)='spcindirr'
       END IF
      ELSE
       IF(Lseats)THEN
        Nspstr=9
        Spcstr(1:Nspstr)='spcextrsd'
       ELSE
        Nspstr=6
        Spcstr(1:Nspstr)='spcrsd'
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      