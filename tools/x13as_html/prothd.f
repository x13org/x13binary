C     Last change:  SRD  25 Jan 100    2:59 pm
*      SUBROUTINE prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ltstso,Ladd1,
*     &                  Critvl)
      SUBROUTINE prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ladd1,Critvl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Print outlier identification header
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c     ------------------------------------------------------------------
*      LOGICAL Ladd1,Ltstao,Ltstls,Ltsttc,Ltstso
      LOGICAL Ladd1,Ltstao,Ltstls,Ltsttc
      INTEGER Begtst,Endtst,idate,itmp,itmp2
      DOUBLE PRECISION Critvl
      DIMENSION Begtst(2),Endtst(2),idate(2),Critvl(POTLR)
c     ------------------------------------------------------------------
*      CHARACTER OTTDIC*120,outstr*19
      CHARACTER OTTDIC*57,outstr*19
      INTEGER ottind,ottptr,POTT,nstr
*      PARAMETER(POTT=15)
      PARAMETER(POTT=7)
      DIMENSION ottptr(0:POTT)
*      PARAMETER(OTTDIC='AO onlyLS onlyAO and LSTC onlyAO and TCLS and TC
*     &AO, LS and TCSO onlyAO and SOLS and SOAO, LS and TCTC and SOAO, TC
*     & and SOLS, TC and SOAll types')
*      DATA ottptr/1,8,15,24,31,40,49,62,69,78,87,100,109,122,135,144/
      PARAMETER(OTTDIC=
     &      'AO onlyLS onlyAO and LSTC onlyAO and TCLS and TCAll types')
      DATA ottptr/1,8,15,24,31,40,49,58/
c     ------------------------------------------------------------------
      CALL dfdate(Begtst,Begspn,Sp,itmp)
      itmp=max(itmp,0)
      CALL addate(Begspn,Sp,itmp,idate)
      CALL dfdate(Endtst,idate,Sp,itmp2)
      IF(Ltstao)itmp2=itmp2+1
      itmp=min(itmp2,Nspobs-itmp)
      CALL genSkip(1081)
      CALL prtshd('OUTLIER DETECTION',idate,Sp,itmp)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
      IF(itmp.le.1)Ltstls=F
      IF(itmp.le.0)THEN
       Ltstao=F
       Ltsttc=F
*       Ltstso=F
      END IF
c     ------------------------------------------------------------------
      ottind=0
      IF(Ltstao)ottind=ottind+1
      IF(Ltstls)ottind=ottind+2
      IF(Ltsttc)ottind=ottind+4
*      IF(Ltstso)ottind=ottind+8
      CALL getstr(OTTDIC,ottptr,POTT,ottind,outstr,nstr)
c     ------------------------------------------------------------------
      CALL mkPClass(Mt1,'indent')
      WRITE(Mt1,1010)outstr(1:nstr)//Cbr
 1010 FORMAT('  Types : ',a)
c     ------------------------------------------------------------------
      IF(Ladd1)THEN
       WRITE(Mt1,1020)'add one'
 1020  FORMAT('  Method : ',a)
      ELSE
       WRITE(Mt1,1020)'add all'
      END IF
c     ------------------------------------------------------------------
      IF(Ltstao)WRITE(Mt1,1030)Cbr,'AO',Critvl(AO)
      IF(Ltstls)WRITE(Mt1,1030)Cbr,'LS',Critvl(LS)
      IF(Ltsttc)WRITE(Mt1,1030)Cbr,'TC',Critvl(TC)
*      IF(Ltstso)WRITE(Mt1,1030)Cbr,'SO',Critvl(SO)
 1030 FORMAT(a,' Critical |t| for ',a,' outliers : ',f12.2)
c     ------------------------------------------------------------------
      CALL writTag(Mt1,'</p>')
c     ------------------------------------------------------------------
      RETURN
      END
