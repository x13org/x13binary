      SUBROUTINE gennpsa(Lmodel,Lseats,Lx11,X11agr,Muladd,Kfulsm,
     &                   Iagr,Ny,Tblind,Lsvlg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'adxser.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ZERO
      PARAMETER(F=.false.,T=.true.,ZERO=0D0)
c-----------------------------------------------------------------------
      INTEGER ipos,Muladd,Kfulsm,Iagr,nchr,nchr1,NPsadj,NPsadjS,NPsadj2,
     &        NPsadjS2,Ny,Tblind
      DOUBLE PRECISION srs
      LOGICAL Lmodel,Lseats,Lx11,Lvslg,X11agr,gosa,lplog,Lsvlg,lnp,lnps
      CHARACTER begstr*(10),chdr*(30),str*(3)
      DIMENSION srs(PLEN)
c-----------------------------------------------------------------------
      logical dpeq
      integer npsa
      external dpeq,npsa
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='noyes')
c-----------------------------------------------------------------------
      DATA ysnptr/1,3,6/
c-----------------------------------------------------------------------
      str=' '
      chdr=' '
      gosa=F
c-----------------------------------------------------------------------
      lplog=F
      IF(Llogqs)THEN
       IF(Lx11)THEN
        IF(Muladd.ne.1)lplog=T
       ELSE
        IF(dpeq(Lam,ZERO))lplog=T
       END IF
      END IF
c-----------------------------------------------------------------------
      CALL dfdate(Bgspec,Begbk2,Ny,ipos)
c-----------------------------------------------------------------------
c   Generate NP stat for the seasonally adjusted series
c-----------------------------------------------------------------------
      NPsadj=NOTSET
      NPsadjS=NOTSET
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
       NPSadj=NPsa(srs,Pos1ob,Posfob,Lmodel,Nnsedf,Nseadf,Ny,lplog)
       IF((ipos+1).gt.Pos1ob)
     &    NPSadjS=NPsa(srs,ipos+1,Posfob,Lmodel,Nnsedf,Nseadf,Ny,lplog)
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
c   Generate NP stat for the seasonally adjusted series adjusted for
c   extreme values and outliers
c-----------------------------------------------------------------------
      NPsadj2=NOTSET
      NPsadjS2=NOTSET
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
       NPSadj2=npsa(srs,Pos1ob,Posfob,Lmodel,Nnsedf,Nseadf,Ny,Llogqs)
       IF((ipos+1).gt.Pos1ob)
     &   NPSadjS2=npsa(srs,ipos+1,Posfob,Lmodel,Nnsedf,Nseadf,Ny,Llogqs)
      END IF
c-----------------------------------------------------------------------
c   Print out NP stats
c-----------------------------------------------------------------------
      lnp=.not.((Npsadj.eq.NOTSET).and.(Npsadj2.eq.NOTSET))
      lnps=.not.((NpsadjS.eq.DNOTST).and.(NpsadjS2.eq.DNOTST))
      IF(Prttab(Tblind).and.(lnp.or.lnps))THEN
       CALL genSkip(Tblind)
       IF(Iagr.eq.4)THEN
        CALL writTagOneLine(Mt1,'h3','@',
     &                      'NP Statistics for residual seasonality: '//
     &                      '(indirect adjustment)')
       ELSE
        CALL writTagOneLine(Mt1,'h3','@',
     &                     'NP Statistics for residual seasonality')
       END IF
       IF(lnp)THEN
        chdr(1:13)='(Full series)'
        Innp=Innp+1
        WRITE(Mt1,1010)Innp
        CALL OutNP(Mt1,Iagr,Npsadj,Npsadj2,chdr,13,lplog)
       END IF
       IF(lnps)THEN
        CALL wrtdat(Bgspec,Sp,begstr,nchr1)
        chdr(1:(nchr1+18))='(Series start in '//begstr(1:nchr1)//')'
        Innp=Innp+1
        WRITE(Mt1,1010)Innp
        CALL OutNP(Mt1,Iagr,NpsadjS,NpsadjS2,chdr,nchr1+18,lplog)
       END IF
      END IF
c-----------------------------------------------------------------------
c   save q stats to udg file
c-----------------------------------------------------------------------
      IF(Savtab(Tblind).and.lnp)THEN
       IF(Iagr.lt.4)THEN
        IF(lplog)THEN
         WRITE(Nform,1040)'nplog','yes'
        ELSE
         WRITE(Nform,1040)'nplog','no'
        END IF
       END IF       
       IF(.not.(NPsadj.eq.NOTSET))THEN
        CALL getstr(YSNDIC,ysnptr,PYSN,NPsadj+1,str,nchr)
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1040)'npindsadj',str(1:nchr)
        ELSE
         WRITE(Nform,1040)'npsadj',str(1:nchr)
        END IF
       END IF
       IF(.not.(NPsadj2.eq.NOTSET))THEN
        CALL getstr(YSNDIC,ysnptr,PYSN,NPsadj2+1,str,nchr)
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1040)'npindsadjevadj',str(1:nchr)
        ELSE
         WRITE(Nform,1040)'npsadjevadj',str(1:nchr)
        END IF
       END IF
      END IF
      IF(Savtab(Tblind).and.lnps)THEN
       IF(.not.(NPsadjS.eq.NOTSET))THEN
        CALL getstr(YSNDIC,ysnptr,PYSN,NPsadjS+1,str,nchr)
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1040)'npsindsadj',str(1:nchr)
        ELSE
         WRITE(Nform,1040)'npssadj',str(1:nchr)
        END IF
       END IF
       IF(.not.(NPsadjS2.eq.NOTSET))THEN
        CALL getstr(YSNDIC,ysnptr,PYSN,NPsadjS2+1,str,nchr)
        IF(Iagr.eq.4)THEN
         WRITE(Nform,1040)'npsindsadjevadj',str(1:nchr)
        ELSE
         WRITE(Nform,1040)'npssadjevadj',str(1:nchr)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c   save q stats to .log file
c-----------------------------------------------------------------------
      IF(Lsvlg.and.(lnp.or.lnps))THEN
       CALL writTagOneLine(Ng,'h3','@',
     &                     'NP Statistics for residual seasonality')
       IF(lnp)THEN
        chdr(1:13)='(Full series)'
        Inlgfl=Inlgfl+1
        WRITE(Ng,1020)Inlgfl
        CALL OutNP(Ng,Iagr,Npsadj,Npsadj2,chdr,13,lplog)
       END IF
       IF(lnps)THEN
        CALL wrtdat(Bgspec,Sp,begstr,nchr1)
        chdr(1:(nchr1+18))='(Series start in '//begstr(1:nchr1)//')'
        Inlgfl=Inlgfl+1
        WRITE(Ng,1020)Inlgfl
        CALL OutNP(Ng,Iagr,NpsadjS,NpsadjS2,chdr,nchr1+18,lplog)
       END IF
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT('<div id="npsa',i3.3,'">')
 1020 FORMAT('<div id="lgnpsa',i6.6,'">')
 1040 FORMAT(a,': ',a)
c-----------------------------------------------------------------------
      RETURN
      END
