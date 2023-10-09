C     Last change:  BCM   8 Dec 1998    4:22 pm
      SUBROUTINE mkshdr(Subttl,Nsttl,Ktabl,Itype,Subhdr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'prior.prm'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'prior.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'picktd.cmn'
c-----------------------------------------------------------------------
      CHARACTER Subttl*(*)
      INTEGER Nsttl,Ktabl,Itype,nnls
      LOGICAL Subhdr,ltd,lhol,lreg,lxreg,lpri
c-----------------------------------------------------------------------
      Subhdr=F
      nnls=Nls-Nramp
      IF(Kpart.eq.4.and.Ktabl.eq.11.and.(Kfulsm.eq.0.or.Kfulsm.eq.2))
     &   THEN
       ltd=Adjtd.eq.1.or.(Ixreg.gt.0.and.Axrghl)
       lhol=Finhol.and.(Khol.eq.2.or.(Ixreg.gt.0.and.Axrghl).or.
     &      Adjhol.eq.1)
       IF(ltd.or.lhol.OR.Finao.or.Finls.or.Finusr)THEN
        IF(Itype.gt.1)THEN
         Nsttl=26
         Subttl(1:Nsttl)='        (also adjusted for'
        ELSE
         Nsttl=24
         Subttl(1:Nsttl)='      (also adjusted for'
        END IF
        IF(ltd)THEN
         IF(Kswv.gt.0)THEN
          Subttl((Nsttl+1):(Nsttl+21))=' combined trading day'
          Nsttl=Nsttl+21
         ELSE
          Subttl((Nsttl+1):(Nsttl+12))=' trading day'
          Nsttl=Nsttl+12
         END IF
         Subhdr=T
        ELSE IF(Kswv.gt.0)THEN
         Subttl((Nsttl+1):(Nsttl+18))=' prior trading day'
         Nsttl=Nsttl+18
         Subhdr=T
        END IF
        IF(lhol)THEN
         IF(Subhdr)THEN
          Subttl((Nsttl+1):(Nsttl+1))=','
          Nsttl=Nsttl+1
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+8))=' holiday'
         Nsttl=Nsttl+8
        END IF
        IF(Finao.or.Fintc.or.Finls)THEN
         IF(Finls)THEN
          IF(nnls.gt.0)THEN
           IF(Subhdr)THEN
            Subttl((Nsttl+1):(Nsttl+1))=','
            Nsttl=Nsttl+1
           ELSE
            Subhdr=T
           END IF
           Subttl((Nsttl+1):(Nsttl+3))=' LS'
           Nsttl=Nsttl+3
          END IF
          IF(Nramp.gt.0)THEN
           IF(Subhdr)THEN
            Subttl((Nsttl+1):(Nsttl+1))=','
            Nsttl=Nsttl+1
           ELSE
            Subhdr=T
           END IF
           Subttl((Nsttl+1):(Nsttl+5))=' ramp'
           Nsttl=Nsttl+5
          END IF
         END IF
         IF(Fintc)THEN
          IF(Subhdr)THEN
           Subttl((Nsttl+1):(Nsttl+1))=','
           Nsttl=Nsttl+1
          ELSE
           Subhdr=T
          END IF
          Subttl((Nsttl+1):(Nsttl+3))=' TC'
          Nsttl=Nsttl+3
         END IF
         IF(Finao)THEN
          IF(Subhdr)THEN
           Subttl((Nsttl+1):(Nsttl+1))=','
           Nsttl=Nsttl+1
          ELSE
           Subhdr=T
          END IF
          Subttl((Nsttl+1):(Nsttl+5))=' AO'
          Nsttl=Nsttl+3
         END IF
         Subttl((Nsttl+1):(Nsttl+8))=' outlier'
         Nsttl=Nsttl+8
        END IF
        IF(Finusr)THEN
         IF(Subhdr)THEN
          Subttl(Nsttl+1:Nsttl+1)=','
          Nsttl=Nsttl+1
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+21))=' user-defined effects'
         Nsttl=Nsttl+21
        END IF
        Subttl(Nsttl+1:Nsttl+1)=')'
        Nsttl=Nsttl+1
       END IF
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.2.and.Ktabl.eq.1)THEN
       lreg=Adjtd.eq.1.or.Adjao.eq.1.or.Adjls.eq.1.or.Adjtc.eq.1.or.
     &      Adjso.eq.1.or.Adjhol.eq.1.or.Adjusr.eq.1.or.Adjsea.eq.1
       lxreg=Ixreg.gt.0.AND.(Axrghl.or.Axrgtd)
       lpri=Priadj.gt.1.OR.Nprtyp.gt.0
       IF(lpri.OR.lreg.or.lxreg.or.Khol.eq.2)THEN
        Nsttl=18
        Subttl(1:Nsttl)='     (adjusted for'
        IF(lpri)THEN
         Subhdr=T
         Subttl((Nsttl+1):(Nsttl+6))=' prior'
         Nsttl=Nsttl+6
        END IF
        IF(lreg)THEN
         IF(Subhdr)THEN
          Subttl((Nsttl+1):(Nsttl+1))=','
          Nsttl=Nsttl+1
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+9))=' regARIMA'
         Nsttl=Nsttl+9
        END IF
        IF(lxreg)THEN
         IF(Subhdr)THEN
          Subttl(Nsttl+1:Nsttl+1)=','
          Nsttl=Nsttl+1
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+21))=' irregular regression'
         Nsttl=Nsttl+21
        END IF
        IF(Khol.eq.2)THEN
         IF(Subhdr)THEN
          Subttl(Nsttl+1:Nsttl+1)=','
          Nsttl=Nsttl+1
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+12))=' X-11 Easter'
         Nsttl=Nsttl+12
        END IF
        IF(Kswv.gt.0)THEN
         IF(Subhdr)THEN
          Subttl(Nsttl+1:Nsttl+1)=','
          Nsttl=Nsttl+1
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+18))=' prior trading day'
         Nsttl=Nsttl+18
        END IF
        Subttl(Nsttl+1:Nsttl+9)=' factors)'
        Nsttl=Nsttl+9
       END IF
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.4.and.(Ktabl.eq.16.or.Ktabl.eq.18))THEN
       ltd=Adjtd.eq.1.or.(Ixreg.gt.0.and.Axrghl)
       lhol=Finhol.and.(Khol.eq.2.or.(Ixreg.gt.0.and.Axrghl).or.
     &      Adjhol.eq.1)
       IF(ltd.or.lhol)THEN
        IF(Ktabl.eq.16)THEN
         Nsttl=25
         Subttl(1:Nsttl)='      (includes seasonal,'
        ELSE
         Nsttl=15
         Subttl(1:Nsttl)='      (includes'
        END IF
        IF(ltd)THEN
         IF(Kswv.gt.0)THEN
          Subttl((Nsttl+1):(Nsttl+21))=' combined trading day'
          Nsttl=Nsttl+21
         ELSE
          Subttl((Nsttl+1):(Nsttl+12))=' trading day'
          Nsttl=Nsttl+12
         END IF
         Subhdr=T
        ELSE IF(Kswv.gt.0)THEN
         Subttl((Nsttl+1):(Nsttl+18))=' prior trading day'
         Nsttl=Nsttl+18
         Subhdr=T
        END IF
        IF(lhol)THEN
         IF(Subhdr)THEN
          Subttl((Nsttl+1):(Nsttl+1))=','
          Nsttl=Nsttl+1
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+8))=' holiday'
         Nsttl=Nsttl+8
        END IF
        Subttl(Nsttl+1:Nsttl+9)=' factors)'
        Nsttl=Nsttl+9
       END IF
c-----------------------------------------------------------------------
      ELSE IF(((Kpart.eq.2.or.Kpart.eq.3).and.Ktabl.eq.15).AND.
     &       ((Khol.eq.1.OR.(Kpart.eq.0.and.Ktabl.eq.1)).OR.
     &       Ixreg.eq.2))THEN
       Nsttl=26
       Subttl(1:Nsttl)='  First pass - Estimating '
       IF(Ixreg.eq.2.AND.(Khol.eq.1.OR.(Kpart.eq.0.and.Ktabl.eq.1)))THEN
        Subttl((Nsttl+1):(Nsttl+44))=
     &     'irregular regression and X-11 Easter effects'
        Nsttl=Nsttl+44
       ELSE IF(Ixreg.eq.2)THEN
        Subttl((Nsttl+1):(Nsttl+28))='irregular regression effects'
        Nsttl=Nsttl+28
       ELSE
        Subttl((Nsttl+1):(Nsttl+19))='X-11 Easter effects'
        Nsttl=Nsttl+19
       END IF
       Subhdr=T
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.1.and.Ktabl.eq.2.and.Priadj.gt.1)THEN
       Nsttl=5
       Subttl(1:Nsttl)='     '
       IF(Priadj.eq.2)THEN
        Subttl((Nsttl+1):(Nsttl+16))='Length of month '
        Nsttl=Nsttl+16
       ELSE IF(Priadj.eq.3)THEN
        Subttl((Nsttl+1):(Nsttl+18))='Length of quarter '
        Nsttl=Nsttl+18
       ELSE IF(Priadj.eq.4)THEN
        Subttl((Nsttl+1):(Nsttl+10))='Leap year '
        Nsttl=Nsttl+10
       END IF
       Subhdr=T
       IF(Picktd)THEN
        Subttl((Nsttl+1):(Nsttl+30))='(from trading day regression) '
        Nsttl=Nsttl+30
       END IF
       IF(Nprtyp.gt.0)THEN
        Subttl((Nsttl+1):(Nsttl+11))=' and prior '
        Nsttl=Nsttl+11
       END IF
       Subttl((Nsttl+1):(Nsttl+12))='adjustments.'
       Nsttl=Nsttl+12
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.1.and.Ktabl.eq.6.and.Priadj.lt.0)THEN
       Nsttl=5
       Subttl(1:Nsttl)='     '
       IF(Priadj.eq.-2)THEN
        Subttl((Nsttl+1):(Nsttl+16))='Length of month '
        Nsttl=Nsttl+16
       ELSE IF(Priadj.eq.-3)THEN
        Subttl((Nsttl+1):(Nsttl+18))='Length of quarter '
        Nsttl=Nsttl+18
       ELSE IF(Priadj.eq.-4)THEN
        Subttl((Nsttl+1):(Nsttl+10))='Leap year '
        Nsttl=Nsttl+10
       END IF
       Subhdr=T
       Subttl((Nsttl+1):(Nsttl+41))=
     &    'prior adjustments included from Table A2.'
       Nsttl=Nsttl+41
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.1.and.Ktabl.eq.3.and.Itype.eq.1)THEN
       lxreg=Ixreg.gt.0.AND.(Axrghl.or.Axrgtd)
       lpri=Priadj.gt.1.OR.Nprtyp.gt.0
       IF(lpri.or.lxreg.or.Khol.eq.2)THEN
        Nsttl=6
        Subttl(1:Nsttl)='     ('
        IF(lpri)THEN
         Subhdr=T
         Subttl((Nsttl+1):(Nsttl+5))='Prior'
         Nsttl=Nsttl+5
        END IF
        IF(lxreg)THEN
         IF(Subhdr)THEN
          Subttl(Nsttl+1:Nsttl+22)=', irregular regression'
          Nsttl=Nsttl+22
         ELSE
          Subttl((Nsttl+1):(Nsttl+20))='Irregular regression'
          Nsttl=Nsttl+20
          Subhdr=T
         END IF
        END IF
        IF(Khol.eq.2)THEN
         IF(Subhdr)THEN
          Subttl(Nsttl+1:Nsttl+2)=', '
          Nsttl=Nsttl+1
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+12))=' X-11 Easter'
         Nsttl=Nsttl+12
        END IF
        Subttl((Nsttl+1):(Nsttl+27))=' adjustments applied to A1)'
        Nsttl=Nsttl+27
       END IF
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.1.and.Ktabl.eq.3.and.Itype.eq.2)THEN
       Nsttl=50
       Subttl(1:Nsttl)=
     &    '       (Permanent prior adjustments applied to A1)'
       Subhdr=T
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.1.and.Ktabl.eq.18)THEN
       Nsttl=16
       Subttl(1:Nsttl)='      (regARIMA '
       IF(Adjtd.eq.1)THEN
        IF(Adjhol.eq.1)THEN
         Subttl((Nsttl+1):(Nsttl+23))='trading day and holiday'
         Nsttl=Nsttl+23
        ELSE
         Subttl((Nsttl+1):(Nsttl+11))='trading day'
         Nsttl=Nsttl+11
        END IF
       ELSE
        Subttl((Nsttl+1):(Nsttl+7))='holiday'
        Nsttl=Nsttl+7
       END IF
       Subttl((Nsttl+1):(Nsttl+23))=' factors applied to A1)'
       Nsttl=Nsttl+23
       Subhdr=T
c     ------------------------------------------------------------------
      ELSE IF((Kpart.eq.5.and.Ktabl.eq.6.and.Itype.ge.2).and.
     &        Iyrt.gt.0)THEN
       Nsttl=32
       Subttl(1:Nsttl)='       with forced yearly totals'
       Subhdr=T
      ELSE IF(Kpart.eq.5.and.Ktabl.eq.1)THEN
       Nsttl=44
       Subttl(1:Nsttl)='       (A1 adjusted by C20 whenever C17 = 0)'
       Subhdr=T
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.5.and.Ktabl.eq.2)THEN
       Nsttl=54
       Subttl(1:Nsttl)='     (D11 with D12 trend substituted whenever C1
     &7 = 0)'
       Subhdr=T
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.5.and.Ktabl.eq.3)THEN
       Nsttl=48
       IF(Muladd.eq.1)THEN
        Subttl(1:Nsttl)=
     &    '     (D13 with 0.0 substituted whenever C17 = 0)'
       ELSE
        Subttl(1:Nsttl)=
     &    '     (D13 with 1.0 substituted whenever C17 = 0)'
       END IF
       Subhdr=T
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.5.and.Ktabl.eq.11)THEN
       Nsttl=62
       Subttl(1:Nsttl)='      (E2 with D12+(A1-E1) value substituted whe
     &never C17 = 0)'
       Subhdr=T
c     ------------------------------------------------------------------
c      ELSE IF((Kpart.eq.4.and.Ktabl.eq.12.AND.((.not.Finls).and.
c     &         Adjls.eq.1)).or.
       ELSE IF (Kpart.eq.1.and.Ktabl.eq.12)THEN
       Nsttl=7
       Subttl(1:Nsttl)='      ('
       IF(nnls.gt.0)THEN
        Subttl((Nsttl+1):(Nsttl+2))='LS'
        Nsttl=Nsttl+2
        Subhdr=T
       END IF
       IF(Nramp.gt.0)THEN
        IF(Subhdr)THEN
         Subttl((Nsttl+1):(Nsttl+1))=','
         Nsttl=Nsttl+1
        ELSE
         Subhdr=T
        END IF
        Subttl((Nsttl+1):(Nsttl+5))=' ramp'
        Nsttl=Nsttl+5
       END IF
       Subttl((Nsttl+1):(Nsttl+19))=' outliers included)'
       Nsttl=Nsttl+19
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.1.and.Ktabl.eq.16)THEN
       Nsttl=15
       Subttl(1:Nsttl)='      (includes'
       IF(Khol.eq.2)THEN
        Subttl((Nsttl+1):(Nsttl+12))=' X-11 Easter'
        Nsttl=Nsttl+12
        Subhdr=T
       END IF
       IF(Adjhol.gt.0)THEN
        IF(Subhdr)THEN
         Subttl((Nsttl+1):(Nsttl+1))=','
         Nsttl=Nsttl+1
        ELSE
         Subhdr=T
        END IF
        Subttl((Nsttl+1):(Nsttl+9))=' regARIMA'
        Nsttl=Nsttl+9
       END IF
       IF(Ixreg.gt.2.and.Axrghl)THEN
        IF(Subhdr)THEN
         Subttl((Nsttl+1):(Nsttl+1))=','
         Nsttl=Nsttl+1
        ELSE
         Subhdr=T
        END IF
        Subttl((Nsttl+1):(Nsttl+21))=' irregular regression'
        Nsttl=Nsttl+21
       END IF
       Subttl((Nsttl+1):(Nsttl+9))=' holiday)'
       Nsttl=Nsttl+9
c     ------------------------------------------------------------------
      ELSE IF(Kpart.eq.1.and.Ktabl.eq.5)THEN
       Nsttl=14
       Subttl(1:Nsttl)='     (includes'
       IF(Nlp.gt.0)THEN
        Subttl((Nsttl+1):(Nsttl+21))=' leap year regressor)'
        Nsttl=Nsttl+21
        Subhdr=T
       ELSE IF(Nln.gt.0)THEN
        Subhdr=T
        IF(Ny.eq.12)THEN
         Subttl((Nsttl+1):(Nsttl+27))=' length of month regressor)'
         Nsttl=Nsttl+27
        ELSE
         Subttl((Nsttl+1):(Nsttl+29))=' length of quarter regressor)'
         Nsttl=Nsttl+29
        END IF
       ELSE IF(Nsln.gt.0)THEN
        Subhdr=T
        Subttl((Nsttl+1):(Nsttl+33))='stock length of month regressor)'
        Nsttl=Nsttl+33
       ELSE IF(Picktd)THEN
        Subhdr=T
        Subttl((Nsttl+1):(Nsttl+25))=' leap year preadjustment)'
        Nsttl=Nsttl+25
       ELSE IF(Priadj.gt.0) THEN
        Subhdr=T
        IF(Priadj.eq.2)THEN
         Subttl((Nsttl+1):(Nsttl+31))=' length of month preadjustment)'
         Nsttl=Nsttl+31
        ELSE IF(Priadj.eq.3)THEN
         Subttl((Nsttl+1):(Nsttl+33))=
     &          ' length of quarter preadjustment)'
         Nsttl=Nsttl+33
        ELSE IF(Priadj.eq.4)THEN
         Subttl((Nsttl+1):(Nsttl+25))=' leap year preadjustment)'
         Nsttl=Nsttl+25
        END IF
       END IF
c     ------------------------------------------------------------------
      ELSE IF((Kpart.eq.2.or.Kpart.eq.3).and.Ktabl.eq.22)THEN
       Subhdr=T
       IF(Itype.eq.1)THEN
        Subttl(1:31)='      (trading day and holiday)'
        Nsttl=31
       ELSE
        Subttl(1:62)='      (prior and irregular regression trading day
     &and holiday)'
        Nsttl=62
       END IF
c     ------------------------------------------------------------------
      ELSE IF((Kpart.eq.1.and.Ktabl.eq.8.and.Itype.eq.1).AND.
     &       ((Adjls.eq.1.or.Finls).OR.(Adjao.eq.1.or.Finao).OR.
     &        (Adjtc.eq.1.or.Fintc)))THEN
       Nsttl=6
       Subttl(1:Nsttl)='     ('
       IF(Adjao.eq.1.or.Finao)THEN
        Subttl((Nsttl+1):(Nsttl+2))='AO'
        Nsttl=Nsttl+2
        Subhdr=T
       END IF
       IF(Adjls.eq.1.or.Finls)THEN
        IF(nnls.gt.0)THEN
         IF(Subhdr)THEN
          Subttl((Nsttl+1):(Nsttl+2))=', '
          Nsttl=Nsttl+2
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+2))='LS'
         Nsttl=Nsttl+2
        END IF
        IF(Nramp.gt.0)THEN
         IF(Subhdr)THEN
          Subttl((Nsttl+1):(Nsttl+2))=', '
          Nsttl=Nsttl+2
         ELSE
          Subhdr=T
         END IF
         Subttl((Nsttl+1):(Nsttl+4))='ramp'
         Nsttl=Nsttl+4
        END IF
       END IF
       IF(Adjtc.eq.1.or.Fintc)THEN
        IF(Subhdr)THEN
         Subttl((Nsttl+1):(Nsttl+2))=', '
         Nsttl=Nsttl+2
        ELSE
         Subhdr=T
        END IF
        Subttl((Nsttl+1):(Nsttl+2))='TC'
        Nsttl=Nsttl+2
       END IF
       Subttl((Nsttl+1):(Nsttl+19))=' outliers included)'
       Nsttl=Nsttl+19
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
