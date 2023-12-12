      SUBROUTINE setapt(Nb,Nf,Begspn,Sp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     set pointers for indirect adjustment, based on number of
c     backcasts (Nb) and number of forecasts (Nf)
c-----------------------------------------------------------------------
      INTEGER MO
      PARAMETER(MO=2)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'agr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11ptr.cmn'
c-----------------------------------------------------------------------
      INTEGER Begspn,Nb,Nf,Sp
      DIMENSION Begspn(2)
c-----------------------------------------------------------------------
      IF(Indnfc.eq.NOTSET)THEN
       Indnfc=Nf
       Indnbc=Nb
       Ind1bk=Pos1ob-Nb
       Ind1ob=Pos1ob
       Indfob=Posfob
       Indffc=Posfob+Nf
       CALL addate(Begspn,Sp,-Nb,Ibgbk2)
       IF(Ibgbk2(MO).gt.1)Ibgbk2(MO)=1
      ELSE
       IF(Indnbc.gt.Nbcst)THEN
        Indnbc=Nbcst
        Ind1bk=Ind1ob-Nbcst
        CALL cpyint(Begbak,2,1,Ibgbk)
*        CALL cpyint(Begbk2,2,1,Ibgbk2)
       END IF
       IF(Indnfc.gt.Nfcst)THEN
        Indnfc=Nfcst
        Indffc=Indfob+Nfcst
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      