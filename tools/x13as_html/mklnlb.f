      SUBROUTINE mklnlb(Lnstr,Nlnchr,Lnudg,Nlnudg,Lnabb,Nlnabb,Nlnab0,
     &                  Lomtst,Aicrgm,Lnzero,Sp)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Generate string for label of lom/loq/lpyear effect within AIC test
c     for lom/loq/lpyear regressors (BCM March 2008)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER Lnstr*(30),Lnudg*(6),Lnabb*(50),datstr*(10)
      INTEGER Lomtst,Aicrgm,Nlnchr,Nlnudg,Nlnabb,Nlnab0,nchdat,Lnzero,Sp
      DIMENSION Aicrgm(2)
c-----------------------------------------------------------------------
c    Initialize Lnstr with blanks
c-----------------------------------------------------------------------
      CALL setchr(' ',30,Lnstr)
      CALL setchr(' ',6,Lnudg)
      CALL setchr(' ',6,Lnabb)
c-----------------------------------------------------------------------
c    Set base of Lnstr
c-----------------------------------------------------------------------
      IF(Lomtst.eq.1)THEN
       Nlnchr=3
       Lnstr(1:Nlnchr)='lom'
       Nlnabb=16
       Lnabb(1:Nlnabb)='Length-of-month'
      ELSE IF(Lomtst.eq.2)THEN
       Nlnchr=3
       Lnstr(1:Nlnchr)='loq'
       Nlnabb=18
       Lnabb(1:Nlnabb)='Length-of-quarter'
      ELSE IF(Lomtst.eq.3)THEN
       Nlnchr=6
       Lnstr(1:Nlnchr)='lpyear'
       Nlnabb=9
       Lnabb(1:Nlnabb)='Leap Year'
      END IF
c-----------------------------------------------------------------------
      Lnudg(1:Nlnchr)=Lnstr(1:Nlnchr)
      Nlnudg=Nlnchr
      Nlnab0=Nlnabb
c-----------------------------------------------------------------------
c    Add change of regime date, if necessary
c-----------------------------------------------------------------------
      IF(Aicrgm(1).ne.NOTSET)THEN
       CALL wrtdat(Aicrgm,Sp,datstr,nchdat)
       IF(Lfatal)RETURN
       IF(Lnzero.eq.0)THEN
        Lnstr((Nlnchr+1):(Nlnchr+nchdat+2))='/'//datstr(1:nchdat)//'/'
        Nlnchr=Nlnchr+nchdat+2
        Lnabb((Nlnabb+1):(Nlnabb+nchdat+22))=', change of regime at '
     &     //datstr(1:nchdat)
        Nlnabb=Nlnabb+nchdat+22
       ELSE IF(Lnzero.eq.1)THEN
        Lnstr((Nlnchr+1):(Nlnchr+nchdat+3))='/'//datstr(1:nchdat)//'//'
        Nlnchr=Nlnchr+nchdat+3
        Lnabb((Nlnabb+1):(Nlnabb+nchdat+13))=', zero after '
     &     //datstr(1:nchdat)
        Nlnabb=Nlnabb+nchdat+13
       ELSE IF(Lnzero.eq.2)THEN
        Lnstr((Nlnchr+1):(Nlnchr+nchdat+4))='//'//datstr(1:nchdat)//'//'
        Nlnchr=Nlnchr+nchdat+4
        Lnabb((Nlnabb+1):(Nlnabb+nchdat+22))=', change of regime at '
     &     //datstr(1:nchdat)
        Nlnabb=Nlnabb+nchdat+22
       ELSE
        Lnstr((Nlnchr+1):(Nlnchr+nchdat+3))='//'//datstr(1:nchdat)//'/'
        Nlnchr=Nlnchr+nchdat+3
        Lnabb((Nlnabb+1):(Nlnabb+nchdat+14))=', zero before '
     &     //datstr(1:nchdat)
        Nlnabb=Nlnabb+nchdat+14
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      