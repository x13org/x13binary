C     Last change:  SRD  31 Dec 2001    7:48 am
**==x11plt.f    processed by SPAG 4.03F  at 09:55 on  1 Mar 1994
      SUBROUTINE x11plt(Z1,Z2,Ib,Ie,Tblptr,Itb,Lfac,Ptype,Ptype2)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Driver routine to print X11 plots
c     ------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'x11tbl.i'
c     ------------------------------------------------------------------
      CHARACTER tblttl*(PTTLEN),ttl*(PTTLEN),subttl*(PTTLEN)
      LOGICAL ltd,lhol,subhdr
      DOUBLE PRECISION Z1(*),Z2(*)
      INTEGER Ib,Ie,Lfac,Ptype,Ptype2,Tblptr,nttl,ntbttl,Itb,nsttl,nnls
      DIMENSION ttl(2),nttl(2)
c     ------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c     ------------------------------------------------------------------
c     Print page heading
c     ------------------------------------------------------------------
      nnls=Nls-Nramp
c     ------------------------------------------------------------------
c     Set up title vector
c     ------------------------------------------------------------------
      CALL getdes(Tblptr,tblttl,ntbttl,F)
      IF(Lfatal)RETURN
      CALL setchr(' ',PTTLEN,subttl)
      ttl(1)=tblttl(1:ntbttl)
      nttl(1)=ntbttl
      ttl(2)=' '
      nttl(2)=0
      IF(Tblptr.eq.LXETRP.and.Itb.eq.1.and.Adjls.eq.1)THEN
       nsttl=7
       subttl(1:nsttl)='      ('
       IF(nnls.gt.0)THEN
        subttl((nsttl+1):(nsttl+2))='LS'
        nsttl=nsttl+2
        Subhdr=T
       END IF
       IF(Nramp.gt.0)THEN
        IF(Subhdr)THEN
         subttl((nsttl+1):(nsttl+1))=','
         nsttl=nsttl+1
        ELSE
         Subhdr=T
        END IF
        subttl((nsttl+1):(nsttl+4))='ramp'
        nsttl=nsttl+4
       END IF
       subttl((nsttl+1):(nsttl+18))='outliers included)'
       nsttl=nsttl+18
      ELSE IF(Tblptr.eq.LXEIRP.and.Itb.eq.1.AND.
     &        (Adjao.eq.1.or.Adjtc.eq.1))THEN
       Subhdr=T
       IF(Adjao.eq.1.and.Adjtc.eq.1)THEN
        subttl='      (AO & TC outliers included)'
        ntbttl=28
       ELSE IF(Adjao.eq.1)THEN
        subttl='      (AO outliers included)'
        ntbttl=33
       ELSE IF(Adjtc.eq.1)THEN
        subttl='      (TC outliers included)'
        ntbttl=28
       END IF
      ELSE IF(Tblptr.eq.LXESAP)THEN
       subhdr=F
       ltd=Adjtd.eq.1.or.(Ixreg.gt.0.and.Axrghl)
       lhol=Finhol.and.(Khol.eq.2.or.(Ixreg.gt.0.and.Axrghl).or.
     &      Adjhol.eq.1)
       nsttl=0
       IF (ltd.or.lhol.OR.Finao.or.Finls.or.Fintc.or.Finusr) THEN
        nsttl=26
        subttl(1:nsttl)='        (also adjusted for'
        IF(ltd)THEN
         subttl((nsttl+1):(nsttl+12))=' trading day'
         subhdr=T
         nsttl=nsttl+12
        END IF
        IF(lhol)THEN
         IF(subhdr)THEN
          subttl((nsttl+1):(nsttl+1))=','
          nsttl=nsttl+1
         ELSE
          subhdr=T
         END IF
         subttl((nsttl+1):(nsttl+8))=' holiday'
         nsttl=nsttl+8
        END IF
        IF(Finao.or.Finls.or.Fintc)THEN
         IF(subhdr)THEN
          subttl(nsttl+1:nsttl+1)=','
          nsttl=nsttl+1
         ELSE
          subhdr=T
         END IF
         IF(Finao.and.Finls.and.Fintc)THEN
          subttl((nsttl+1):(nsttl+20))=' AO, TC & LS outlier'
          nsttl=nsttl+20
         ELSE IF(Finls.and.Fintc)THEN
          subttl((nsttl+1):(nsttl+16))=' TC & LS outlier'
          nsttl=nsttl+16
         ELSE IF(Finao.and.Fintc)THEN
          subttl((nsttl+1):(nsttl+16))=' TC & AO outlier'
          nsttl=nsttl+16
         ELSE IF(Finao.and.Finls)THEN
          subttl((nsttl+1):(nsttl+16))=' AO & LS outlier'
          nsttl=nsttl+16
         ELSE IF(Finls)THEN
          subttl((nsttl+1):(nsttl+11))=' LS outlier'
          nsttl=nsttl+11
         ELSE IF(Fintc)THEN
          subttl((nsttl+1):(nsttl+11))=' TC outlier'
          nsttl=nsttl+11
         ELSE
          subttl((nsttl+1):(nsttl+11))=' AO outlier'
          nsttl=nsttl+11
         END IF
        END IF
        IF(Finusr)THEN
         IF(subhdr)THEN
          subttl(nsttl+1:nsttl+1)=','
          nsttl=nsttl+1
         ELSE
          subhdr=T
         END IF
         subttl((nsttl+1):(nsttl+21))=' user-defined effects'
         nsttl=nsttl+21
        END IF
        subttl(nsttl+1:nsttl+1)=')'
        nsttl=nsttl+1
       END IF
       ttl(2)=subttl
       nttl(2)=nsttl
      END IF
c     ------------------------------------------------------------------
c     Set up and print plot
c     ------------------------------------------------------------------
      CALL grzlst(Ib,Ie,Lyr,Z1,Z2,Ny*9,Ny,Lfac)
      CALL genSkip(Tblptr)
      CALL chrt(ttl,nttl,Ptype,Ptype2,Ny)
c     ------------------------------------------------------------------
      RETURN
      END
