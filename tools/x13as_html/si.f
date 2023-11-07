C     Last change:  BCM  15 Apr 2005   11:46 am
**==si.f    processed by SPAG 4.03F  at 12:06 on 12 Jul 1994
      SUBROUTINE si(Ksect,Kfda,Klda,Nyr,Iforc,Nbcst,Kersa1,Ksdev1,Lfd1,
     &              Lld1,Kfulsm,Kfdax,Kldax)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE CALCULATES THE SEASONALS FROM THE SI ESTIMATES
C --- FOR PART B.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'xtrm.cmn'
      INCLUDE 'x11tbl.i'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      LOGICAL F
      PARAMETER(F=.false.,ONE=1D0,ZERO=0D0)
c-----------------------------------------------------------------------
      INTEGER Iforc,iftny,k,Nbcst,Kfda,Klda,Ksect,l,lfd,Lfd1,lld,Lld1,
     &        llda,Kersa1,Ksdev1,Nyr,iv,l1,l2,i,Kfulsm,Kfdax,Kldax
      DOUBLE PRECISION Temp,dvec
      DIMENSION Temp(PLEN),dvec(1)
c     ------------------------------------------------------------------
      COMMON /work  / Temp
c     ------------------------------------------------------------------
      dvec(1)=ZERO
      lfd=Lfd1+(Nyr/2)
      lld=Lld1-(Nyr/2)
      l=Lld1-Lfd1+1
      llda=Klda
      IF(Iforc.ne.0.and.Ksect.eq.1)llda=Klda-Iforc
c     ------------------------------------------------------------------
      IF(Kfulsm.lt.2)THEN
       IF(Ksect.eq.2)CALL vsfa(Stsi,Kfda,llda,Nyr)
       CALL vsfb(Sts,Stsi,Kfda,Klda,Nyr)
      END IF
c     ------------------------------------------------------------------
      k=Ksect*5-2
      iftny=15*Nyr
      IF(Ksect.eq.2)THEN
       IF(Prttab(LX11B8))CALL table(Stsi,Lfd1,Lld1,k,1,1,dvec,LX11B8)
       IF(.not.Lfatal.and.Savtab(LX11B8))
     &    CALL punch(Stsi,Lfd1,Lld1,LX11B8,F,F)
      ELSE IF(Iforc.eq.0)THEN
       IF(Prttab(LX11B3))CALL table(Stsi,lfd,lld,k,1,1,dvec,LX11B3)
       IF(.not.Lfatal.and.Savtab(LX11B3))
     &    CALL punch(Stsi,lfd,lld,LX11B3,F,F)
      ELSE IF(l.le.iftny.and.Nbcst.gt.0)THEN
       IF(Prttab(LX11B3))CALL table(Stsi,Lfd1,Lld1,k,1,1,dvec,LX11B3)
       IF(.not.Lfatal.and.Savtab(LX11B3))
     &    CALL punch(Stsi,Lfd1,Lld1,LX11B3,F,F)
      ELSE
       IF(Prttab(LX11B3))CALL table(Stsi,lfd,Lld1,k,1,1,dvec,LX11B3)
       IF(.not.Lfatal.and.Savtab(LX11B3))
     &    CALL punch(Stsi,lfd,Lld1,LX11B3,F,F)
      END IF
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
      IF(Kfulsm.eq.2)THEN
       CALL copy(Stsi,Klda,1,Sti)
      ELSE
       IF(Psuadd)THEN
        DO i=Kfda,Klda
         Sti(i)=Stsi(i)-Sts(i)+ONE
        END DO
       ELSE
        CALL divsub(Sti,Stsi,Sts,Kfda,Klda)
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Ksect.eq.1.and.Ksdev.lt.4)THEN
       CALL vtest(Sti,iv,Kfdax,Kldax)
       CALL entsch(Kersa1,Ksdev1,Kersa,Ksdev,iv)
      END IF
      CALL xtrm(Sti,Kfda,Klda,Kfdax,Kldax)
      CALL replac(Stsi,Temp,Stwt,Kfda,Klda,Nyr)
      k=k+1
      IF(Ksect.eq.1.and.Prttab(LX11B4))THEN
       l1=lfd
       l2=Lld1
       IF(Iforc.eq.0)THEN
        l2=lld
       ELSE IF(l.le.iftny.and.Nbcst.gt.0)THEN
        l1=Lfd1
       END IF
       IF(Ksdev.eq.0)THEN
        CALL table(Temp,l1,l2,k,1,4,Stdev,LX11B4)
       ELSE
        CALL table(Temp,l1,l2,k,1,5,dvec,LX11B4)
       END IF
      ELSE IF(Prttab(LX11B9))THEN
       IF(Ksdev.eq.0)THEN
        CALL table(Temp,Lfd1,Lld1,k,1,4,Stdev,LX11B9)
       ELSE
        CALL table(Temp,Lfd1,Lld1,k,1,5,dvec,LX11B9)
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(.not.Lfatal.and.Kfulsm.lt.2)CALL vsfb(Sts,Stsi,Kfda,Klda,Nyr)
c     ------------------------------------------------------------------
      RETURN
      END
