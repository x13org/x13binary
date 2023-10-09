C     Last change:  BCM  21 Oct 97    8:57 am
**==prtagr.f    processed by SPAG 4.03F  at 12:05 on 12 Jul 1994
      SUBROUTINE prtagr(Z,Ib,Ie,Ibsav,Iesav,Ktabl,Itype,Nop,Iagr,Ext,
     &                  Extagr,Dvec,Lgraf)
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      LOGICAL Lgraf
      INTEGER Ib,Ie,Ibsav,Iesav,Ktabl,Itype,Nop,Iagr,Ext,Extagr,lastf,
     &        frstf
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
       IF(Prttab(Ext))CALL table(Z,Ib,Ie,Ktabl,Itype,Nop,Dvec,Ext)
       IF(.not.Lfatal.and.Savtab(Ext))CALL punch(Z,frstf,lastf,Ext,F,F)
       IF(.not.Lfatal.and.Lgraf)CALL punch(Z,frstf,lastf,Ext,Lgraf,F)
      ELSE IF(Iagr.eq.4)THEN
       IF(Prttab(Extagr))CALL table(Z,Ib,Ie,Ktabl,Itype,Nop,Dvec,Extagr)
       IF(.not.Lfatal.and.Savtab(Extagr))
     &    CALL punch(Z,frstf,lastf,Extagr,F,F)
       IF(.not.Lfatal.and.Lgraf)CALL punch(Z,frstf,lastf,Extagr,Lgraf,F)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
