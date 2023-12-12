C     Last change:  BCM  19 Oct 1998   11:51 am
      SUBROUTINE adjsrs(Nspobs,Sp,Begspn,Fctdrp,Nfcst,Nbcst,Ok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     adjsrs.f, Release 1, Subroutine Version 1.4, Modified 02 Nov 1994.
c-----------------------------------------------------------------------
c     Get the time series of prior adjustment factors, y, including the
c  number of observations, nadj and start date, start.
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER PLOM,PLOQ
      PARAMETER(PLOM=2,PLOQ=3,T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER sname*3
      DOUBLE PRECISION base
      LOGICAL Ok,lom,begrgm
      INTEGER Begspn,Fctdrp,i,Nbcst,Nfcst,Nspobs,Sp
      DIMENSION Begspn(2),begrgm(PLEN)
c     ------------------------------------------------------------------
      LOGICAL chkcvr
      EXTERNAL chkcvr
c     ------------------------------------------------------------------
      IF(Adjmod.eq.2)THEN
       base=0D0
      ELSE
       base=1D0
      END IF
      CALL addate(Begspn,Sp,-Nbcst,Begadj)
      Nadj=Nspobs+Nbcst+max(Sp,Nfcst-Fctdrp)
c-----------------------------------------------------------------------
c     Generate length of period factors.  Note lom and loq are the
c same the factor is determined by the seasonal period.
c-----------------------------------------------------------------------
      IF(Priadj.gt.1)THEN
       IF(Priadj.eq.PLOM.or.Priadj.eq.PLOQ)THEN
        lom=T
       ELSE
        lom=F
       END IF
c-----------------------------------------------------------------------
c     The 7th trading day factors
c-----------------------------------------------------------------------
       IF(Lrgmtd.and.Picktd.and.(MOD(Tdzero,2).ne.0).and.
     &   (.not.Fulltd))THEN
        CALL gtrgpt(Begadj,Tddate,Tdzero,begrgm,Nadj)
       ELSE
        CALL setlg(T,PLEN,begrgm)
       END IF
       CALL td7var(Begadj,Sp,Nadj,1,1,lom,F,T,Adj,begrgm)
c-----------------------------------------------------------------------
       Kfmt=1
       IF(lom)THEN
        sname='LOM'
        IF(Sp.eq.4)sname='LOQ'
       ELSE
        sname='LPY'
       END IF
       IF(Nuspad.eq.0.or.Npser.eq.0)THEN
        Prmser(1:3)=sname
        Npser=3
       ELSE IF(Npser.lt.61)THEN
        Prmser(1:Npser+4)=Prmser(1:Npser)//'+'//sname
        Npser=Npser+4
       END IF
c-----------------------------------------------------------------------
c     Default is no adjustment
c-----------------------------------------------------------------------
      ELSE
       CALL setdp(base,Nadj,Adj)
      END IF
c-----------------------------------------------------------------------
c     May want to change so this will only need to cover the span and
c not the series and the forecasts then the other user defined
c adjustments will default to 1d0.  Note, if there are adjustments
c for part of the forecasts but not all and they are not 1 already
c this should be an error because the proper unadjusted forecasts
c will not be printed.
c-----------------------------------------------------------------------
      IF(Nprtyp.gt.0)THEN
       DO i=1,Nprtyp
        IF(Prtype(i).eq.1)THEN
         CALL addadj(Nspobs,Begspn,Sp,Begadj,Bgutad,Nustad,Frstat,
     &               Usrtad,Adj,Nadj,base,'temporary',Percnt(i),Ok)
        ELSE IF(Prtype(i).eq.2)THEN
         CALL addadj(Nspobs,Begspn,Sp,Begadj,Bgupad,Nuspad,Frstap,
     &               Usrpad,Adj,Nadj,base,'permanent',Percnt(i),Ok)
        END IF
        IF(Lfatal)RETURN
        IF(Ok)THEN
         IF(Kfmt.eq.0)Kfmt=1
        ELSE
         IF(Kfmt.gt.0)Kfmt=0
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
      CALL dfdate(Begspn,Begadj,Sp,Adj1st)
      Adj1st=Adj1st+1
      IF(Frstap.eq.0.and.Priadj.gt.1)Frstap=1
c     -----------------------------------------------------------------
      RETURN
c     -----------------------------------------------------------------
      END
