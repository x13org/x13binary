      SUBROUTINE pragr2(Z,Ib,Ie,Ibsav,Iesav,Ktabl,Itype,Nop,Iagr,Ext,
     &                  Extagr,Extp,Extagp,Muladd,Dvec,Lgraf)
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Add argument Ibsav, allow saving of backcasts (BCM, October 2006)
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      LOGICAL Lgraf
      INTEGER Ib,Ie,Ibsav,Iesav,Ktabl,Itype,Nop,Iagr,Ext,Extagr,Extp,
     &        Extagp,Muladd,lastf,frstf
      DOUBLE PRECISION Z,Dvec
      DIMENSION Z(*),Dvec(*)
c-----------------------------------------------------------------------
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      frstf=Ib
      IF(Savbct.and.Ibsav.lt.Ib)frstf=Ibsav
      lastf=Ie
      IF(Savfct.and.Iesav.gt.Ie)lastf=Iesav
c-----------------------------------------------------------------------
      IF(Iagr.lt.4)THEN
       IF(Prttab(Ext).or.Prttab(Extp))
     &    CALL table(Z,Ib,Ie,Ktabl,Itype,Nop,Dvec,Ext)
       IF(.not.Lfatal.and.Savtab(Ext))
     &    CALL punch(Z,frstf,lastf,Ext,F,F)
       IF(.not.Lfatal.and.Savtab(Extp))
     &    CALL punch(Z,frstf,lastf,Extp,F,Muladd.ne.1)
       IF(.not.Lfatal.and.Lgraf)CALL punch(Z,frstf,lastf,Ext,Lgraf,F)
      ELSE IF(Iagr.eq.4)THEN
       IF(Prttab(Extagr).or.Prttab(Extagp))
     &    CALL table(Z,Ib,Ie,Ktabl,Itype,Nop,Dvec,Extagr)
       IF(.not.Lfatal.and.Savtab(Extagr))
     &    CALL punch(Z,frstf,lastf,Extagr,F,F)
       IF(.not.Lfatal.and.Savtab(Extagp))
     &    CALL punch(Z,frstf,lastf,Extagp,F,Muladd.ne.1)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Z,frstf,lastf,Extagr,Lgraf,F)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
