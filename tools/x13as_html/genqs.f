      SUBROUTINE genqs(Lmodel,Lseats,Lx11,X11agr,Psuadd,Muladd,Kfulsm,
     &                 Iagr,Ny,Tblind,Lsvlg,Lorig)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'adxser.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
c      INCLUDE 'spctbl.i'
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE,ONEHND,ZERO
      PARAMETER(F=.false.,T=.true.,ONE=1D0,ONEHND=100D0,ZERO=0D0)
c-----------------------------------------------------------------------
      CHARACTER begstr*(10),chdr*(30)
      LOGICAL Lmodel,Lseats,Lx11,X11agr,goirr,gosa,lqs,lqss,Psuadd,
     &        Lsvlg,Lorig,lplog
      INTEGER i,j,Muladd,Kfulsm,Iagr,Ny,ipos,nchr1,Tblind
      DOUBLE PRECISION qsori,qsirr,qssadj,qsori2,qsirr2,qssadj2,srs,
     &                 xmean,qsoriS,qsirrS,qssadjS,qsoriS2,qsirrS2,
     &                 qssadjS2
      DIMENSION srs(PLEN)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Stmcd(PLEN),Stime(PLEN),Stex(PLEN)
      COMMON /mq5a  / Stmcd,Stime
      COMMON /mq10  / Stex
c-----------------------------------------------------------------------
      LOGICAL dpeq
      DOUBLE PRECISION calcqs,chisq
      EXTERNAL dpeq,calcqs,chisq
c-----------------------------------------------------------------------
      goirr=F
      gosa=F
      chdr=' '
      lplog=F
c-----------------------------------------------------------------------
      CALL dfdate(Bgspec,Begbk2,Ny,ipos)
c-----------------------------------------------------------------------
c   Generate QS stat for the original series
c-----------------------------------------------------------------------
      QSori=DNOTST
      QSoriS=DNOTST
      IF(Lorig)THEN
       CALL copy(Series,PLEN,1,srs)
c-----------------------------------------------------------------------
c    take log of series if necessary (12-2-2014)
c-----------------------------------------------------------------------
       IF(Llogqs)THEN
        IF(Lx11)THEN
         IF(Muladd.ne.1)THEN
          DO i=Pos1ob,Posfob
           srs(i)=log(srs(i))
          END DO
          IF(.not.lplog)lplog=T
         END IF
        ELSE
         IF(dpeq(Lam,ZERO))THEN
          DO i=Pos1ob,Posfob
           srs(i)=log(srs(i))
          END DO
          IF(.not.lplog)lplog=T
         END IF
        END IF       
       END IF       
c-----------------------------------------------------------------------
       CALL qsDiff(srs,Pos1ob,Posfob,Lmodel,Nnsedf,Nseadf,Ny,QSori)
       IF((ipos+1).gt.Pos1ob)
     &    CALL qsDiff(srs,ipos+1,Posfob,Lmodel,Nnsedf,Nseadf,Ny,QSoriS)
      END IF
c-----------------------------------------------------------------------
c   Generate QS stat for the original series adjusted for
c   extreme values and outliers
c-----------------------------------------------------------------------
      QSori2=DNOTST
      QSoriS2=DNOTST
      IF(Lorig)THEN
       CALL copy(Stcsi,PLEN,1,srs)
       IF(Lx11)THEN
        IF(Psuadd)THEN
         DO i=Pos1ob,Posfob
          IF(Kfulsm.eq.2)THEN
           srs(i)=Stc(i)*Sti(i)
          ELSE
           srs(i)=Stc(i)*(Sts(i)+(Sti(i)-ONE))
          END IF
         END DO
        ELSE
         CALL addmul(srs,srs,Stex,Pos1bk,Posffc)
        END IF
       END IF
c-----------------------------------------------------------------------
c    take log of series if necessary (12-2-2014)
c-----------------------------------------------------------------------
       IF(Llogqs)THEN
        IF(Lx11)THEN
         IF(Muladd.ne.1)THEN
          DO i=Pos1ob,Posfob
           srs(i)=log(srs(i))
          END DO
         END IF
        ELSE
         IF(dpeq(Lam,ZERO))THEN
          DO i=Pos1ob,Posfob
           srs(i)=log(srs(i))
          END DO
         END IF
        END IF       
       END IF       
c-----------------------------------------------------------------------
       CALL qsDiff(srs,Pos1ob,Posfob,Lmodel,Nnsedf,Nseadf,Ny,QSori2)
       IF((ipos+1).gt.Pos1ob)
     &    CALL qsDiff(srs,ipos+1,Posfob,Lmodel,Nnsedf,Nseadf,Ny,QSoriS2)
      END IF
c-----------------------------------------------------------------------
c   Generate QS stat for the seasonally adjusted series
c-----------------------------------------------------------------------
      QSsadj=DNOTST
      QSsadjS=DNOTST
      IF((Lx11.and.Kfulsm.eq.0).or.Lseats)THEN
       gosa=T
       IF(Lseats)gosa=Hvstsa
      END IF
      IF(gosa)THEN
       IF(Lseats)THEN
        CALL copy(Seatsa,PLEN,1,srs)
       ELSE
        CALL copy(Stci,PLEN,1,srs)
       END IF
c-----------------------------------------------------------------------
c    take log of series if necessary (12-2-2014)
c-----------------------------------------------------------------------
       IF(Llogqs)THEN
        IF(Lx11)THEN
         IF(Muladd.ne.1)THEN
          DO i=Pos1ob,Posfob
           srs(i)=log(srs(i))
          END DO
          IF(.not.lplog)lplog=T
         END IF
        ELSE
         IF(dpeq(Lam,ZERO))THEN
          DO i=Pos1ob,Posfob
           srs(i)=log(srs(i))
          END DO
          IF(.not.lplog)lplog=T
         END IF
        END IF       
       END IF       
c-----------------------------------------------------------------------
       CALL qsDiff(srs,Pos1ob,Posfob,Lmodel,Nnsedf,Nseadf,Ny,QSSadj)
       IF((ipos+1).gt.Pos1ob)
     &    CALL qsDiff(srs,ipos+1,Posfob,Lmodel,Nnsedf,Nseadf,Ny,QSSadjS)
      END IF
c-----------------------------------------------------------------------
c   Generate QS stat for the seasonally adjusted series adjusted for
c   extreme values and outliers
c-----------------------------------------------------------------------
      QSsadj2=DNOTST
      QSsadjS2=DNOTST
      IF(gosa)THEN
       IF(Iagr.eq.4)THEN
        IF(X11agr)THEN
         CALL copy(Stcime,PLEN,1,srs)
         IF(Adjls.eq.1)CALL divsub(srs,srs,Facls,Pos1ob,Posfob)
        ELSE
         CALL copy(Stci,PLEN,1,srs)
         IF(Adjls.eq.1)CALL divsub(srs,srs,Facls,Pos1ob,Posfob)
        END IF
       ELSE IF(Lx11)THEN
        CALL copy(Stcime,PLEN,1,srs)
        IF(Adjls.eq.1)CALL divsub(srs,srs,Facls,Pos1ob,Posfob)
       ELSE
        CALL copy(Stocsa,PLEN,1,srs)
       END IF
c-----------------------------------------------------------------------
c    take log of series if necessary (12-2-2014)
c-----------------------------------------------------------------------
       IF(Llogqs)THEN
        IF(Lx11)THEN
         IF(Muladd.ne.1)THEN
          DO i=Pos1ob,Posfob
           srs(i)=log(srs(i))
          END DO
         END IF
        ELSE
         IF(dpeq(Lam,ZERO))THEN
          DO i=Pos1ob,Posfob
           srs(i)=log(srs(i))
          END DO
         END IF
        END IF       
       END IF       
c-----------------------------------------------------------------------
       CALL qsDiff(srs,Pos1ob,Posfob,Lmodel,Nnsedf,Nseadf,Ny,QSSadj2)
       IF((ipos+1).gt.Pos1ob)
     &   CALL qsDiff(srs,ipos+1,Posfob,Lmodel,Nnsedf,Nseadf,Ny,QSSadjS2)
      END IF
c-----------------------------------------------------------------------
c   Generate QS stat for the irregular component
c-----------------------------------------------------------------------
      QSirr=DNOTST
      QSirrS=DNOTST
      IF((Lx11.and.Kfulsm.eq.0).or.Lseats)THEN
       goirr=T
       IF(Lseats)goirr=Hvstir
       IF(Iagr.eq.4)goirr=goirr.and.X11agr
      END IF
      IF(goirr)THEN
       DO i=Pos1ob,Posfob
        IF(Lx11)THEN
         srs(i)=Sti(i)
        ELSE
         srs(i)=Seatir(i)
        END IF
        IF(Muladd.ne.1)srs(i)=srs(i)-ONE
       END DO
       QSirr = calcqs(srs,Pos1ob-1,Posfob,Ny)
       IF((ipos+1).gt.Pos1ob)QSirrS = calcqs(srs,ipos,Posfob,Ny)
      END IF
c-----------------------------------------------------------------------
c   Generate QS stat for the irregular component adjusted for
c   extreme values and outliers
c-----------------------------------------------------------------------
      QSirr2=DNOTST
      QSirrS2=DNOTST
      IF(goirr)THEN
       DO i=Pos1ob,Posfob
        IF(Lx11)THEN
         srs(i)=Stime(i)
        ELSE
         srs(i)=Stocir(i)/ONEHND
        END IF
        IF(Muladd.ne.1)srs(i)=srs(i)-ONE
       END DO
       QSirr2 = calcqs(srs,Pos1ob-1,Posfob,Ny)
       IF((ipos+1).gt.Pos1ob)QSirrS2 = calcqs(srs,ipos,Posfob,Ny)
      END IF
c-----------------------------------------------------------------------
c   Print out q stats
c-----------------------------------------------------------------------
      lqs=.not.(dpeq(QSori,DNOTST).and.dpeq(QSrsd,DNOTST).and.
     &          dpeq(QSsadj,DNOTST).and.dpeq(QSirr,DNOTST).and.
     &          dpeq(QSsadj2,DNOTST).and.dpeq(QSirr2,DNOTST).and.
     &          dpeq(QSori2,DNOTST))
      lqss=.not.(dpeq(QSoriS,DNOTST).and.
     &          dpeq(QSrsd2,DNOTST).and.dpeq(QSsadjS,DNOTST).and.
     &          dpeq(QSirrS,DNOTST).and.dpeq(QSsadjS2,DNOTST).and.
     &          dpeq(QSirrS2,DNOTST).and.dpeq(QSoriS2,DNOTST))
      IF(Prttab(Tblind).and.(lqs.or.lqss))THEN
       CALL genSkip(Tblind)
       IF(Iagr.eq.4)THEN
        CALL writTagOneLine(Mt1,'h3','@',
     &                      'QS Statistics for seasonality: '//
     &                      '(indirect adjustment)')
       ELSE
        CALL writTagOneLine(Mt1,'h3','@',
     &                      'QS Statistics for seasonality')
       END IF
       IF(lqs)THEN
        chdr(1:13)='(Full series)'
        Inqs=Inqs+1
        WRITE(Mt1,1010)Inqs
        CALL OutQS(Mt1,Iagr,QSori,QSori2,QSrsd,QSsadj,QSsadj2,QSirr,
     &             QSirr2,chdr,13,Iagr.lt.4,lplog,F)
       END IF
       IF(lqss)THEN
        CALL wrtdat(Bgspec,Sp,begstr,nchr1)
        chdr(1:(nchr1+18))='(Series start in '//begstr(1:nchr1)//')'
        Inqs=Inqs+1
        WRITE(Mt1,1010)Inqs
        CALL OutQS(Mt1,Iagr,QSoriS,QSoriS2,QSrsd2,QSsadjS,QSsadjS2,
     &             QSirrS,QSirrS2,chdr,nchr1+18,Iagr.lt.4,lplog,F)
       END IF
      END IF
c-----------------------------------------------------------------------
c   save q stats to log file
c-----------------------------------------------------------------------
      IF(Savtab(Tblind).and.lqs)THEN
       IF(Iagr.lt.4)THEN
        IF(lplog)THEN
         WRITE(Nform,1040)'qslog','yes'
        ELSE
         WRITE(Nform,1040)'qslog','no'
        END IF
       END IF       
       IF(.not.dpeq(QSori,DNOTST).and.Iagr.lt.4)
     &    WRITE(Nform,1030)'qsori',QSori,chisq(QSori,2)
       IF(.not.dpeq(QSori2,DNOTST).and.Iagr.lt.4)
     &    WRITE(Nform,1030)'qsorievadj',QSori2,chisq(QSori2,2)
       IF(.not.dpeq(QSrsd,DNOTST).and.Iagr.lt.4)
     &    WRITE(Nform,1030)'qsrsd',QSrsd,chisq(QSrsd,2)
       IF(.not.dpeq(QSsadj,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1030)'qsindsadj',QSsadj,chisq(QSsadj,2)
        ELSE
         WRITE(Nform,1030)'qssadj',QSsadj,chisq(QSsadj,2)
        END IF
       END IF
       IF(.not.dpeq(QSsadj2,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1030)'qsindsadjevadj',QSsadj2,chisq(QSsadj2,2)
        ELSE
         WRITE(Nform,1030)'qssadjevadj',QSsadj2,chisq(QSsadj2,2)
        END IF
       END IF
       IF(.not.dpeq(QSirr,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1030)'qsindirr',QSirr,chisq(QSirr,2)
        ELSE
         WRITE(Nform,1030)'qsirr',QSirr,chisq(QSirr,2)
        END IF
       END IF
       IF(.not.dpeq(QSirr2,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1030)'qsindirrevadj',QSirr2,chisq(QSirr2,2)
        ELSE
         WRITE(Nform,1030)'qsirrevadj',QSirr2,chisq(QSirr2,2)
        END IF
       END IF
      END IF
      IF(Savtab(Tblind).and.lqss)THEN
       IF(.not.dpeq(QSoriS,DNOTST).and.Iagr.lt.4)
     &    WRITE(Nform,1030)'qssori',QSoriS,chisq(QSoriS,2)
       IF(.not.dpeq(QSoriS2,DNOTST).and.Iagr.lt.4)
     &    WRITE(Nform,1030)'qssorievadj',QSoriS2,chisq(QSoriS2,2)
       IF(.not.dpeq(QSrsd2,DNOTST).and.Iagr.lt.4)
     &    WRITE(Nform,1030)'qssrsd',QSrsd2,chisq(QSrsd2,2)
       IF(.not.dpeq(QSsadjS,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1030)'qssindsadj',QSsadjS,chisq(QSsadjS,2)
        ELSE
         WRITE(Nform,1030)'qsssadj',QSsadjS,chisq(QSsadjS,2)
        END IF
       END IF
       IF(.not.dpeq(QSsadjS2,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1030)'qssindsadjevadj',QSsadjS2,chisq(QSsadjS2,2)
        ELSE
         WRITE(Nform,1030)'qsssadjevadj',QSsadjS2,chisq(QSsadjS2,2)
        END IF
       END IF
       IF(.not.dpeq(QSirrS,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1030)'qssindirr',QSirrS,chisq(QSirrS,2)
        ELSE
         WRITE(Nform,1030)'qssirr',QSirrS,chisq(QSirrS,2)
        END IF
       END IF
       IF(.not.dpeq(QSirrS2,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1030)'qssindirrevadj',QSirrS2,chisq(QSirrS2,2)
        ELSE
         WRITE(Nform,1030)'qssirrevadj',QSirrS2,chisq(QSirrS2,2)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c   save q stats to .udg file
c-----------------------------------------------------------------------
      IF(Lsvlg.and.(lqs.or.lqss))THEN
       IF(Lorig)THEN
        CALL writTagOneLine(Ng,'h3','@','QS Statistics for seasonality')
       ELSE
        CALL writTagOneLine(Ng,'h3','@',
     &            'QS Statistics for seasonality (Indirect Adjustment)')
       END IF
       IF(lqs)THEN
        chdr(1:13)='(Full series)'
        Inlgfl=Inlgfl+1
        WRITE(Ng,1020)Inlgfl
        CALL OutQS(Ng,Iagr,QSori,QSori2,QSrsd,QSsadj,QSsadj2,QSirr,
     &             QSirr2,chdr,13,Lorig,lplog,F)
       END IF
       IF(lqss)THEN
        CALL wrtdat(Bgspec,Sp,begstr,nchr1)
        chdr(1:(nchr1+18))='(Series start in '//begstr(1:nchr1)//')'
        Inlgfl=Inlgfl+1
        WRITE(Ng,1020)Inlgfl
        CALL OutQS(Ng,Iagr,QSoriS,QSoriS2,QSrsd2,QSsadjS,QSsadjS2,
     &             QSirrS,QSirrS2,chdr,nchr1+18,Lorig,lplog,F)
       END IF
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT('<div id="qs',i3.3,'">')
 1020 FORMAT('<div id="lgqs',i6.6,'">')
 1030 FORMAT(a,':',f16.5,1x,f10.5)
 1040 FORMAT(a,': ',a)
c-----------------------------------------------------------------------
      RETURN
      END
