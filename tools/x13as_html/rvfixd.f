C     Last change:  BCM   8 Dec 1998    4:02 pm
      SUBROUTINE rvfixd(Tdfix,Holfix,Otlfix,Usrfix,Iregfx,Regfx,Nb,
     &                  Rgvrtp,Nusrrg,Usrtyp,Ncusrx,Userfx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER (T=.true.)
c-----------------------------------------------------------------------
      LOGICAL Tdfix,Holfix,Otlfix,Usrfix,allfix,Regfx,Userfx
      INTEGER i,Iregfx,iusr,Nb,Rgvrtp,Nusrrg,Usrtyp,rtype,Ncusrx
      DIMENSION Rgvrtp(*),Usrtyp(*),Regfx(*)
c-----------------------------------------------------------------------
      iusr=1
      allfix=T
      DO i=1,Nb
       rtype=Rgvrtp(i)
       IF(Nusrrg.gt.0)THEN
        IF(rtype.eq.PRGTUD)THEN
         rtype=Usrtyp(iusr)
         iusr=iusr+1
        ELSE IF((rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.
     &           rtype.eq.PRGTUS)THEN
         iusr=iusr+1
        END IF
       END IF
       IF((Tdfix.AND.((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.
     &    rtype.eq.PRRTTD.or.rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.
     &    rtype.eq.PRATST.or.rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.
     &    rtype.eq.PRA1TD.or.rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.
     &    rtype.eq.PRA1ST).or.(rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.
     &    rtype.eq.PRGTLQ.or.rtype.eq.PRGTLY.or.rtype.eq.PRRTLQ.or.
     &    rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRATSL.or.
     &    rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.rtype.eq.PRATLQ.or.
     &    rtype.eq.PRATLY).or.rtype.eq.PRGUTD.or.rtype.eq.PRGULY.or.
     &    rtype.eq.PRGULM.or.rtype.eq.PRGULQ)).or.
     &    (Holfix.AND.(rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.
     &                 rtype.eq.PRGTES.or.rtype.eq.PRGTLD.or.
     &                 rtype.eq.PRGTTH.or.
     &                (rtype.ge.PRGTUH.and.rtype.le.PRGUH5))).or.
     &    (Usrfix.AND.(rtype.eq.PRGTUD.or.rtype.eq.PRGTUS.or.
     &                (rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.
     &                 rtype.eq.PRGUTD.or.rtype.eq.PRGULY.or.
     &                 rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &                 rtype.eq.PRGUAO.or.rtype.eq.PRGULS.or.
     &                 rtype.eq.PRGUCN.or.rtype.eq.PRGUCY.or.
     &                 rtype.eq.PRGUSO)).or.
     &    (Otlfix.AND.(rtype.eq.PRGTAO.or.rtype.eq.PRGTLS.or.
     &    rtype.eq.PRGTRP.or.rtype.eq.PRGTTC.or.rtype.eq.PRGTSO.or.
     &    rtype.eq.PRGTAL.or.rtype.eq.PRGTAA.or.rtype.eq.PRGTAT.or.
     &    rtype.eq.PRGTQD.or.rtype.eq.PRGTQI.or.rtype.eq.PRGTTL.or.
     &    rtype.eq.PRGUAO.or.rtype.eq.PRGULS.or.rtype.eq.PRGUSO)))THEN
        Regfx(i)=T
        IF(Iregfx.le.1)Iregfx=2
       END IF
       allfix=allfix.and.Regfx(i)
      END DO
      IF(allfix.and.Iregfx.eq.2)Iregfx=3
      IF(.not.Userfx)Userfx=(Usrfix.and.Ncusrx.gt.0)
c-----------------------------------------------------------------------
      RETURN
      END
