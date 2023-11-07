      SUBROUTINE mktdlb(Tdstr,Ntdchr,Tdabb,Ntdabb,Itdtst,Aicstk,Aicrgm,
     &                  Tdzero,Sp)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Generate string for label of trading day effect within AIC test
c     for trading day
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER tdstr*(30),datstr*(10),tdabb*(80)
      INTEGER Itdtst,Aicrgm,Aicstk,Ntdchr,nchdat,Tdzero,Sp,ntdabb
      DIMENSION Aicrgm(2)
c-----------------------------------------------------------------------
      CALL setchr(' ',30,tdstr)
      CALL setchr(' ',80,tdabb)
c-----------------------------------------------------------------------
      IF(Itdtst.eq.1)THEN
       ntdchr=2
       tdstr(1:ntdchr)='td'
       ntdabb=11
       tdabb(1:ntdabb)='trading day'
      ELSE IF(Itdtst.eq.2)THEN
       ntdchr=10
       tdstr(1:ntdchr)='tdnolpyear'
       ntdabb=24
       tdabb(1:ntdabb)='trading day no leap year'
      ELSE IF(Itdtst.eq.4)THEN
       ntdchr=7
       tdstr(1:ntdchr)='td1coef'
       ntdabb=27
       tdabb(1:ntdabb)='trading day one coefficient'
      ELSE IF(Itdtst.eq.5)THEN
       ntdchr=11
       tdstr(1:ntdchr)='td1nolpyear'
       ntdabb=40
       tdabb(1:ntdabb)='trading day one coefficient no leap year'
      ELSE
       IF(Itdtst.eq.6)THEN
        ntdchr=13
        tdstr(1:ntdchr)='tdstock1coef['
        ntdabb=53
        tdabb(1:ntdabb)=
     &   'stock trading day one coefficient, with stock day of '
       ELSE
        ntdchr=8
        tdstr(1:ntdchr)='tdstock['
        ntdabb=37
        tdabb(1:ntdabb)='stock trading day, with stock day of '
       END IF
       ntdchr=ntdchr+1
       CALL itoc(Aicstk,tdstr,ntdchr)
       IF(Lfatal)RETURN
       tdstr(ntdchr:ntdchr)=']'
       ntdabb=ntdabb+1
       CALL itoc(Aicstk,tdabb,ntdabb)
       IF(Lfatal)RETURN
       ntdabb=ntdabb-1
      END IF
      IF(Aicrgm(1).ne.NOTSET)THEN
       CALL wrtdat(Aicrgm,Sp,datstr,nchdat)
       IF(Lfatal)RETURN
       IF(Tdzero.eq.0)THEN
        tdstr((ntdchr+1):(ntdchr+nchdat+2))='/'//datstr(1:nchdat)//'/'
        ntdchr=ntdchr+nchdat+2
        tdabb((ntdabb+1):(ntdabb+nchdat+22))=', change of regime at '
     &     //datstr(1:nchdat)
        ntdabb=ntdabb+nchdat+22
       ELSE IF(Tdzero.eq.1)THEN
        tdstr((ntdchr+1):(ntdchr+nchdat+3))='/'//datstr(1:nchdat)//'//'
        ntdchr=ntdchr+nchdat+3
        tdabb((ntdabb+1):(ntdabb+nchdat+13))=', zero after '
     &     //datstr(1:nchdat)
        ntdabb=ntdabb+nchdat+13
       ELSE IF(Tdzero.eq.2)THEN
        tdstr((ntdchr+1):(ntdchr+nchdat+4))='//'//datstr(1:nchdat)//'//'
        ntdchr=ntdchr+nchdat+4
        tdabb((ntdabb+1):(ntdabb+nchdat+22))=', change of regime at '
     &     //datstr(1:nchdat)
        ntdabb=ntdabb+nchdat+22
       ELSE
        tdstr((ntdchr+1):(ntdchr+nchdat+3))='//'//datstr(1:nchdat)//'/'
        ntdchr=ntdchr+nchdat+3
        tdabb((ntdabb+1):(ntdabb+nchdat+14))=', zero before '
     &     //datstr(1:nchdat)
        ntdabb=ntdabb+nchdat+14
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      