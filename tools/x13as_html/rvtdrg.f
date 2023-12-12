      SUBROUTINE rvtdrg(Revptr,ChRgGp,NRgGp,RGrpNm,Grptot,ChTDrg,NTDrg,
     &                  Nrvtdrg,Cnctdrg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'usrreg.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INTEGER Revptr,itd,icol,begcol,endcol,igrp,ngrpcr,NRgGp,RGrpNm,
     &        ntmpcr,ictmp,NchARMA,Nrvtdrg,iusr,rtype,NTDrg,thisG,oldtd,
     &        ncolcr,Grptot
      LOGICAL isTD,isLen,isUser,isGood
      CHARACTER ChRgGp*(PGRPCR),ChTDrg*(PCOLCR),grpstr*(PGRPCR),
     &          tmpttl*(PCOLCR),colstr*(PCOLCR)
      DOUBLE PRECISION Cnctdrg,sumTD
      DIMENSION ChTDrg(PARIMA),NTDrg(16),Cnctdrg(16,PREV),ChRgGp(5),
     &          NRgGp(5),RgrpNm(5)
c-----------------------------------------------------------------------
      itd=0
      iusr=0
      thisG=0
c-----------------------------------------------------------------------
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       rtype=Rgvrtp(begcol)
       isTD=rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &      rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.rtype.eq.PRATST.or.
     &      rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &      rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRA1ST.or.
     &      rtype.eq.PRGUTD
       isLen=rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &      rtype.eq.PRGTLY.or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.
     &      rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.
     &      rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.rtype.eq.PRATLY.or.
     &      rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.rtype.eq.PRGULY
       isUser=rtype.eq.PRGTUD.or.(rtype.ge.PRGTUH.and.rtype.le.PRGUH5)
     &      .or.rtype.eq.PRGTUS.or.rtype.eq.PRGUTD.or.rtype.eq.PRGULM
     &      .or.rtype.eq.PRGULQ.or.rtype.eq.PRGULY.or.rtype.eq.PRGUAO
     &      .or.rtype.eq.PRGULS.or.rtype.eq.PRGUCN.or.rtype.eq.PRGUCY
     &      .or.rtype.eq.PRGUSO
       IF(isTD.or.isLen.or.isUser)THEN
        oldtd=itd
        sumTD=0D0
        DO icol=begcol,endcol
         isGood=F
         IF(isUser)THEN
          iusr=iusr+1
          IF(rtype.eq.PRGTUD)rtype=Usrtyp(iusr)
          IF(rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.
     &       rtype.eq.PRGULQ.or.rtype.eq.PRGULY)THEN
           IF(.not.Regfx(icol))isGood=T
          END IF
         ELSE
          IF(.not.Regfx(icol))isGood=T
         END IF
         IF(isGood)THEN
          itd=itd+1
          Cnctdrg(itd,Revptr)=B(icol)
          CALL getstr(Colttl,Colptr,Ncoltl,icol,colstr,ncolcr)
          IF(Lfatal)RETURN
          ChTDrg(itd)=colstr(1:ncolcr)
          NTDrg(itd)=ncolcr
          sumTD=sumTD+B(icol)
         END IF
        END DO
        IF(itd.gt.oldtd)THEN
         thisG=thisG+1
         CALL getstr(Grpttl,Grpptr,Ngrp,igrp,grpstr,ngrpcr)
         IF(Lfatal)RETURN
         ChRgGp(thisG)=grpstr(1:ngrpcr)
         NRgGp(thisG)=ngrpcr
         RGrpNm(thisG)=endcol-begcol+1
        END IF
        IF(isTD)THEN
         itd=itd+1
         Cnctdrg(itd,Revptr)=0D0-sumTD
         IF((grpstr(1:min(11,ngrpcr)).eq.'Trading Day'.or.
     &       grpstr(1:min(17,ngrpcr)).eq.'Stock Trading Day').and.
     &       begcol.lt.endcol)THEN
          ncolcr=3
          colstr(1:ncolcr)='Sun'
          IF(((.not.Fulltd).and.index(grpstr(1:ngrpcr),'(before').gt.0)
     &       .or.index(grpstr(1:ngrpcr),'(change for before').gt.0)THEN
           ncolcr=5
           colstr(1:ncolcr)='Sun I'
          ELSE IF(index(grpstr(1:ngrpcr),'(starting').gt.0
     &         .or.index(grpstr(1:ngrpcr),'(change for after').gt.0)THEN
           ncolcr=6
           colstr(1:ncolcr)='Sun II'
          END IF
         ELSE IF((grpstr(1:min(25,ngrpcr)).eq.
     &            '1-Coefficient Trading Day'.or.
     &            grpstr(1:min(31,ngrpcr)).eq.
     &            '1-Coefficient Stock Trading Day').and.
     &            begcol.eq.endcol)THEN
          ncolcr=7
          colstr(1:ncolcr)='Sat/Sun'
          IF(((.not.Fulltd).and.index(grpstr(1:ngrpcr),'(before').gt.0)
     &       .or.index(grpstr(1:ngrpcr),'(change for before').gt.0)THEN
           ncolcr=9
           colstr(1:ncolcr)='Sat/Sun I'
          ELSE IF(index(grpstr(1:ngrpcr),'(starting').gt.0
     &         .or.index(grpstr(1:ngrpcr),'(change for after').gt.0)THEN
           ncolcr=10
           colstr(1:ncolcr)='Sat/Sun II'
          END IF
         END IF
         ChTDrg(itd)=colstr(1:ncolcr)
         NTDrg(itd)=ncolcr
         RGrpNm(thisG)=RGrpNm(thisG)+1
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      Nrvtdrg=itd
      Grptot=thisG
c-----------------------------------------------------------------------
      RETURN
      END
