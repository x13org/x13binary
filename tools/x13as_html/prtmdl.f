C     Last change:  SRD  19 Nov 99    7:01 am 
      SUBROUTINE prtmdl(Lestim,Lprtes,Prtse,Lsaves,Lgraf,Ldiag,Lprtcm,
     &                  Lsavcm,Prtch2,Tlsrun,Prtvar,Prttls,Lpritr)
c-----------------------------------------------------------------------
c     prtmdl.f, Release 1, Subroutine Version 1.13, Modified 16 Feb 1995.
c-----------------------------------------------------------------------
c     Prints out the regression estimates, standard errors, t-values,
c and estimates of ARMA parameters for each component
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c begcol  i  Local index for the begining column in b of the current
c             group of regression effects
c endcol  i  Local index for the last column in b of the current
c             group of regression effects
c i       i  Local do loop index
c igrp    i  Local do loop index for the current group of regression
c             variables, suchas trading day
c ndf     i  Local number of degrees of freedom, nefobs-nb
c nefobs  i  Number of effective observations, nw, the length of the
c             differenced series is used if exact AR and MA, nwp, the
c             length of the AR filtered data if conditional used or only
c             exact MA.
c nelt    i  Local number of elements in the packed form of
c             chol([X:y]'[X:y])
c rmse    d  Local root mean square error a'a/(nefobs-nb).  Note, a'a
c             is the ncth diagonal element of the cholesky
c             decomposition of the filtered [X:y]'[X:y] matrix
c seb     d  Local standard error of the current regression estimate,
c             b(i).  Seb=sqrt(X'X[i,i])*rmse
c tmp     d  Local temporary scalar
c tval    d  Local t-value=b(i)/seb
c xpxinv  d  Local pb(pb+1)/2, ncxy(ncxy+1)/2 used vector to hold the
c             packed form of the inverse of X'X
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
      INTEGER LAGS,MODEL,OPRS,PDRV,TWOHUN
      DOUBLE PRECISION TWO,ZERO,TWOPT5
      PARAMETER(LAGS=3,MODEL=1,OPRS=2,PDRV=4,TWO=2D0,TWOPT5=2.5D0,
     &          ZERO=0D0,TWOHUN=200)
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'mdldg.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'title.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'cogreg.prm'
      INCLUDE 'htmlout.prm'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdlsvl.i'
c-----------------------------------------------------------------------
      INTEGER PTBLWD
      PARAMETER(PTBLWD=PGRPCR+6)
c-----------------------------------------------------------------------
      CHARACTER blnk*(11),colstr*(PCOLCR),grpstr*(PGRPCR),ordend*(2),
     &          str*(PGRPCR),cfix*(7),tmpttl*(PGRPCR),begstr*(10),
     &          endstr*(10),starz*(2),drvttl*((PCOLCR+PGRPCR+1)*PDRV),
     &          drvstr*(PCOLCR+PGRPCR+1),marker*(11),fixdrv*(7),
     &          tmpstr*(PGRPCR),thisId*6,thisHdr*20,thisVal*16,
     &          thisLag*6,ctype*(31),cfix2*(7)
      LOGICAL fcnok,ldrvfc,ldrvf1,Lestim,lfrtgr,linhol,linotl,lishol,
     &        lisotl,lnewgr,lprchi,Lprtcm,Lprtes,lprthd,lprtrs,lprtse,
     &        lprund,lprvar,Lsavcm,Lsaves,Lgraf,Prtch2,Prtse,Prtvar,
     &        Prttls,Lpritr,Ldiag,lprrgm,lsvchi
      INTEGER baselt,begcol,begfac,endcol,fh1,i,icol,iestpm,igrp,info,j,
     &        ipos,jcol,nblnk,nchr,numg,nefobs,nelt,ngrpcr,ncolcr,ntmp,
     &        beglag,begopr,endlag,endopr,iflt,ilag,iopr,ntmpcr,spchr,
     &        nbeg,nend,tbwdth,Tlsrun,fh2,nb2,nfix,regidx,df,drvptr,
     &        ndrvtl,ndrv,imark,msg,imsg,tmsg,fh0,nGroup
      DOUBLE PRECISION chi2vl,dpmpar,pv,rmse,seb,sumb,sumvar,tmp,tval,
     &                 xpxinv,searma,bdrv,sedrv,tvdrv,dnefob,seinov
c      DIMENSION ordend(0:9),xpxinv(PB*(PB+1)/2),tmp(2),regidx(PB)
      DIMENSION ordend(0:9),xpxinv(PXPX),tmp(2),regidx(PB),bdrv(PDRV),
     &          sedrv(PDRV),drvptr(0:PDRV),msg(4),fixdrv(0:PDRV),
     &          tvdrv(PDRV)
c  Bob Fay moved EXTERNAL statement up
      EXTERNAL dpmpar
      SAVE fh1
      INTEGER Nobs,Nrusrx,Bgusrx,Mxiter,Mxnlit,Mxcklg,Begtst,Endtst,
     &        Fctdrp,Begsrs,Frstsy,Begmdl,Endmdl,Nomnfy,Lsrun,Dflist,
     &        Niddf,Nidsdf,Mxidlg
      DIMENSION Bgusrx(2),Begtst(2),Endtst(2),Begsrs(2),Begmdl(2),
     &          Endmdl(2),Dflist(PDFLG,2)
      COMMON /armaid/ Dflist,Niddf,Nidsdf,Mxidlg
      COMMON /armain/ Nobs,Nrusrx,Bgusrx,Mxiter,Mxnlit,Mxcklg,Begtst,
     &                Endtst,Fctdrp,Begsrs,Frstsy,Begmdl,Endmdl,Nomnfy,
     &                Lsrun
c-----------------------------------------------------------------------
      DATA ordend/'th','st','nd','rd','th','th','th','th','th','th'/
      DATA blnk/'           '/
c-----------------------------------------------------------------------
      INCLUDE 'cogreg.var'
c-----------------------------------------------------------------------
c     Open the save file to print the estimates if necessary.
c-----------------------------------------------------------------------
      fh0=0
      IF(.not.Lnoprt)fh0=Mt1
      nb2=0
      ndrvtl=0
      ndrv=0
      seinov=ZERO
      cfix=' &nbsp;'
      cfix2='       '
      IF(Ldiag)THEN
       CALL intlst(PDRV,drvptr,ndrvtl)
       ndrv=ndrvtl+1
      END IF
      IF(Lsaves.and.Irev.le.1.and.Issap.le.1)THEN
       CALL opnfil(T,F,LESTES,fh1,fcnok)
       IF(.not.fcnok)THEN
        CALL abend
        RETURN
       END IF
      END IF
      IF(Lgraf)THEN
       CALL opnfil(T,Lgraf,LESTES,fh2,fcnok)
       IF(.not.fcnok)THEN
        CALL abend
        RETURN
       END IF
      END IF
      tmsg=0
      CALL setint(0,4,msg)
      IF(Lprtes)Inpmdl=Inpmdl+1
c-----------------------------------------------------------------------
c      initialize xpxinv to zero
c      BCM February 2007
c-----------------------------------------------------------------------
      CALL setdp(ZERO,PXPX,xpxinv)
c-----------------------------------------------------------------------
c     Print out the convergence error messages and determine what to
c print depending on whether or not the model converged.  If the model
c does converge, report the number of iterations and print the estimates
c and standard errors.
c-----------------------------------------------------------------------
      nefobs=Nspobs-Nintvl
c      CALL prterr(nefobs,Lestim)
c      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Report convergence
c-----------------------------------------------------------------------
*      IF((.not.(Lautom.or.Lautox)).and.(.not.Lhiddn).and.Ldiag)THEN
      IF((.not.Lhiddn).and.Ldiag)THEN
       WRITE(Nform,1281)'steplength: ',Stepln
       IF(Convrg)THEN
        WRITE(Nform,1282)'yes'
       ELSE
        WRITE(Nform,1282)'no'
       END IF
      END IF
c-----------------------------------------------------------------------
*      IF((.NOT.(Lautom.or.Lautox)).and.Convrg.and.(.not.Lhiddn).and.
*     &    Lestim.and.Nestpm.gt.0)THEN
      IF(Convrg.and.(.not.Lhiddn).and.Lestim.and.Nestpm.gt.0)THEN
       IF(Lpritr.or.Lprtes)THEN
        IF(Lpritr)CALL mkPOneLine(Mt1,'@','&nbsp;')
        WRITE(Mt1,110)Nliter,Nfev
       END IF
c-----------------------------------------------------------------------
c      print out warning message if estimation converges and maximum
c      iterations < 200
c-----------------------------------------------------------------------
       IF(Nliter.gt.TWOHUN)THEN
        CALL nWritln('Maximization of the <abbr title="ARIMA">AR(I)MA'//
     &               '</abbr> model likelihood has required more',
     &               fh0,Mt2,T,F)
        CALL writln(' than 200 iterations.  This could indicate that'//
     &              ' the model is inadequate for the data.',
     &              fh0,Mt2,F,T)
       END IF
       IF(Ldiag)THEN
        WRITE(Nform,1000)'niter: ',Nliter
        WRITE(Nform,1000)'nfev: ',Nfev         
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print estimates only or SE and other tests.  If the model has not
c converged the standard errors, t-statistics, chi^2 tests, and
c MLE variance will not be printed out.
c-----------------------------------------------------------------------
      lprchi=Prtch2
      lprvar=Prtvar
      lprtse=Prtse
      lsvchi=Ldiag
c-----------------------------------------------------------------------
*      lprtrs=Prtse
      lprtrs=.not.(Niddf.gt.0.or.Nidsdf.gt.0)
      IF(Convrg.and.Var.gt.2D0*dpmpar(1))THEN
       tbwdth=PTBLWD
      ELSE
       lprtrs=F
       tbwdth=37
       lprchi=F
       lsvchi=F
       Tlsrun=0
      END IF
c-----------------------------------------------------------------------
c     Find the number of columns in [X:y] and the number of regression
c variables.
c-----------------------------------------------------------------------
      IF(Ldiag)WRITE(Nform,1000)'nreg: ',Nb
      IF(Ngrp.gt.0)THEN
c     ------------------------------------------------------------------
c     Generate number of unfixed regressors
c     ------------------------------------------------------------------
       nb2=Nb
       IF(Iregfx.ge.2)THEN
        DO j=1,Nb
         IF(Regfx(j))nb2=nb2-1
        END DO
       END IF
c-----------------------------------------------------------------------
c     Get the root mean square error and X'X inverse.
c-----------------------------------------------------------------------
       IF(nb2.gt.0)THEN
c        nelt=Ncxy*(Ncxy+1)/2
        nelt=(nb2+1)*(nb2+2)/2
c-----------------------------------------------------------------------
        IF(Var.gt.2D0*dpmpar(1))THEN
         rmse=sqrt(Var)
         CALL copy(Chlxpx,nelt,1,xpxinv)
         CALL dppdi(xpxinv,nb2,tmp,1)
c         CALL dppdi(xpxinv,Nb,tmp,1)
c-----------------------------------------------------------------------
        ELSE
         rmse=ZERO
        END IF
       ELSE
        rmse=ZERO
       END IF
c-----------------------------------------------------------------------
c     Print out the regression estimates, standard errors, and t-values
c for each regression group.
c-----------------------------------------------------------------------
       IF(Lprtes)THEN
        CALL genSkip(1074)
        CALL writTagOneLine(Mt1,'h3','@','Regression Model')
        WRITE(Mt1,100)Inpmdl,'.reg'
        CALL mkTableTag(Mt1,'w70','Regression Model')
        CALL mkCaption(Mt1,'Regression Model')
*        WRITE(Mt1,1020)('-',i=1,tbwdth)
* 1020   FORMAT(' ',120(a))
c-----------------------------------------------------------------------
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        IF(lprtrs.and.nb2.gt.0)THEN
         Inpe=Inpe+1
         WRITE(thisId,1010)'pe',Inpe
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                       'Parameter'//Cbr//'Estimate')
         Inse=Inse+1
         WRITE(thisId,1010)'se',Inse
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                       'Standard'//Cbr//'Error')
         Intv=Intv+1
         WRITE(thisId,1010)'tv',Intv
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','t-value')
         nGroup=4
c-----------------------------------------------------------------------
        ELSE
         Inpe=Inpe+1
         WRITE(thisId,1010)'pe',Inpe
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                       'Parameter'//Cbr//'Value')
         IF(Iregfx.eq.2)THEN
          Inse=Inse+1
          WRITE(thisId,1010)'se',Inse
          CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                        'Standard'//Cbr//'Error')
         ELSE
          CALL mkTableCell(Mt1,'head','&nbsp;')
         END IF
         nGroup=3
        END IF
c-----------------------------------------------------------------------
        CALL writTag(Mt1,'</tr>')
       END IF
c-----------------------------------------------------------------------
       IF(Lsaves)WRITE(fh1,1050)TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                          TABCHR,TABCHR,TABCHR
       IF(Lgraf)WRITE(fh2,1050)TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                         TABCHR,TABCHR,TABCHR
c-----------------------------------------------------------------------
c     Foreach regression variable or group of variables find their
c starting and ending columns and initialize variables indicate
c whether
c-----------------------------------------------------------------------
       ldrvfc=F
       ldrvf1=F
       lfrtgr=T
       linhol=F
       linotl=F
       nfix=0
c-----------------------------------------------------------------------
       DO igrp=1,Ngrp
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
        lnewgr=T
        lishol=Rgvrtp(begcol).eq.PRGTTH.or.Rgvrtp(begcol).eq.PRGTLD.or.
     &       ((Rgvrtp(begcol).eq.PRGTEC.or.Rgvrtp(begcol).eq.PRGTEA.or.
     &         Rgvrtp(begcol).eq.PRGTES).and.(begcol-endcol).eq.0)
        lisotl=Rgvrtp(begcol).eq.PRGTAO.or.Rgvrtp(begcol).eq.PRGTLS.or.
     &         Rgvrtp(begcol).eq.PRGTRP.or.Rgvrtp(begcol).eq.PRGTTC.or.
     &         Rgvrtp(begcol).eq.PRGTSO.or.Rgvrtp(begcol).eq.PRGTTL.or.
     &         Rgvrtp(begcol).eq.PRGTQI.or.Rgvrtp(begcol).eq.PRGTQD.or.
     &         Rgvrtp(begcol).eq.PRSQAO.or.Rgvrtp(begcol).eq.PRSQLS
c-----------------------------------------------------------------------
c     Get the title of the regression group and indicate whether the
c group/effect is and outlier or holiday effect.
c-----------------------------------------------------------------------
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,grpstr,ngrpcr)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     For each regression variable in the group calculate the standard
c error and t-value if the variance in nonzero
c-----------------------------------------------------------------------
        DO icol=begcol,endcol
         IF(Regfx(icol))THEN
          seb=ZERO
          nfix=nfix+1
          regidx(icol)=NOTSET
         ELSE
          regidx(icol)=icol-nfix
          seb=sqrt(xpxinv(regidx(icol)*(regidx(icol)+1)/2))*rmse
         END IF
c-----------------------------------------------------------------------
c     compute t value, or set to zero is se is zero
c-----------------------------------------------------------------------
         IF(seb.gt.ZERO)THEN
          tval=B(icol)/seb
         ELSE
          tval=ZERO
         END IF
         Treg(icol)=tval
c-----------------------------------------------------------------------
c     Get the title of the effect
c-----------------------------------------------------------------------
         CALL getstr(Colttl,Colptr,Ncoltl,icol,colstr,ncolcr)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Set up the formatting.  New groups of effects skip a line before
c the title unless it is the first group which is under the title or
c is an outlier effect following another outlier or a holiday effect
c following another holiday effect.  Effects within a group are indented
c but groups of single effects are not.
c-----------------------------------------------------------------------
         IF(Lprtes)THEN
*          IF(.not.lfrtgr.and.lnewgr)THEN
*           IF(.not.((lishol.and.linhol).or.(lisotl.and.linotl)))
*     &        WRITE(Mt1,'()')
*          END IF
c-----------------------------------------------------------------------
          IF(lnewgr)THEN
           linhol=lishol
           linotl=lisotl
c-----------------------------------------------------------------------
           IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
            Ingr=Ingr+1
            WRITE(thisId,1010)'gr',Ingr
            CALL writTag(Mt1,'<tr>')
            CALL mkHeaderCellId(Mt1,0,nGroup,thisId,'left','@',
     &                          grpstr(1:ngrpcr))
            CALL writTag(Mt1,'</tr>')
           END IF
          END IF
c-----------------------------------------------------------------------
c     Now that the group title has been printed it is nolonger a new
c or first group.
c-----------------------------------------------------------------------
          lnewgr=F
          lfrtgr=F
c-----------------------------------------------------------------------
c     If the regressor is a change of regime regressor, ensure that the
c proper label is printed next to the regressor name.
c-----------------------------------------------------------------------
          marker=blnk
          imark=Rgvrtp(icol)
          IF((imark.ge.PRRTSE.and.imark.le.PRRTSL).or.
     &       (imark.ge.PRATSE.and.imark.le.PRATSL).or.
     &       (imark.ge.PRR1TD.and.imark.le.PRA1TD).or.
     &       (imark.ge.PRR1ST.and.imark.le.PRA1ST))THEN
           IF(index(grpstr(1:ngrpcr),'change for after').gt.0)THEN
            marker(10:11)='@@'
            imsg=4
           ELSE IF(index(grpstr(1:ngrpcr),'change for before').gt.0)THEN
            marker(2:11)='&amp;&amp;'
            imsg=2
           ELSE IF(index(grpstr(1:ngrpcr),'starting').gt.0)THEN
            marker(11:11)='@'
            imsg=3
           ELSE
            marker(7:11)='&amp;'
            imsg=1
           END IF
c-----------------------------------------------------------------------
c     set up indicator variable for descriptive message following
c     regressor printout
c-----------------------------------------------------------------------
           IF(imark.ge.PRR1TD)THEN
            tmsg=3
           ELSE IF(imark.ge.PRRTSE.and.imark.le.PRRTSL)THEN
            tmsg=imark-PRRTSE+1
           ELSE
            tmsg=imark-PRATSE+1
           END IF
           IF(msg(imsg).gt.0.and.msg(imsg).ne.tmsg)THEN
            msg(imsg)=9
           ELSE IF(msg(imsg).eq.0)THEN
            msg(imsg)=tmsg
           END IF
          ELSE
           imark=0
          END IF
c-----------------------------------------------------------------------
c     Print the regression estimates and possibly the standard errors
c and t-values.
c-----------------------------------------------------------------------
          cfix=' &nbsp;'
          cfix2='       '
c-----------------------------------------------------------------------
c    Create header tag for regression variable - check to see if there
c    are Markers before the regressor
c-----------------------------------------------------------------------
          CALL writTag(Mt1,'<tr>')
          Invl=Invl+1
          WRITE(thisId,1010)'vl',Invl
          IF(imark.eq.0)THEN
           IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
            CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                          colstr(1:ncolcr))
           ELSE
            CALL mkHeaderCellId(Mt1,0,0,thisId,'left','@',
     &                          colstr(1:ncolcr))
           END IF
          ELSE
           Infoot=Infoot+1
           Vfoot(Infoot)=imsg+2
           WRITE(Mt1,1060)thisId,marker,Infoot,marker,Infoot,
     &                    colstr(1:ncolcr)
          END IF
c-----------------------------------------------------------------------
          IF((.not.Regfx(icol)).and.lprtrs)THEN
           IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
*           WRITE(Mt1,1070)B(icol),seb,tval
            WRITE(thisHdr,1020)Ingr,Invl,'pe',Inpe
            WRITE(thisVal,1030)B(icol)
            CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
            WRITE(thisHdr,1020)Ingr,Invl,'se',Inse
            WRITE(thisVal,1040)seb
            CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
            WRITE(thisHdr,1020)Ingr,Invl,'tv',Intv
            WRITE(thisVal,1051)tval
            CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
           ELSE
            WRITE(thisHdr,1021)Invl,'pe',Inpe
            WRITE(thisVal,1030)B(icol)
            CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
            WRITE(thisHdr,1021)Invl,'se',Inse
            WRITE(thisVal,1040)seb
            CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
            WRITE(thisHdr,1021)Invl,'tv',Intv
            WRITE(thisVal,1051)tval
            CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
           END IF
c-----------------------------------------------------------------------
          ELSE IF(Regfx(icol).and.lprtrs)THEN
*           WRITE(Mt1,1071)marker(1:nblnk),colstr(1:ncolcr),B(icol),
*     &                    '         (fixed)'
           IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
            WRITE(thisHdr,1020)Ingr,Invl,'pe',Inpe
            WRITE(thisVal,1030)B(icol)
            CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
           ELSE
            WRITE(thisHdr,1021)Invl,'pe',Inpe
            WRITE(thisVal,1030)B(icol)
            CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
           END IF
           CALL mkTableCellSpan(Mt1,'col',2,'center','(fixed)')
           cfix='(fixed)'
           cfix2=cfix
          ELSE
           IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
            WRITE(thisHdr,1020)Ingr,Invl,'pe',Inpe
            WRITE(thisVal,1030)B(icol)
            CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
           ELSE
            WRITE(thisHdr,1021)Invl,'pe',Inpe
            WRITE(thisVal,1030)B(icol)
            CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
           END IF
          END IF
          CALL writTag(Mt1,'</tr>')
         END IF
c-----------------------------------------------------------------------
         IF(Lsaves)THEN
          IF((.not.Regfx(icol)).and.lprtrs)THEN
           WRITE(fh1,1080)grpstr(1:ngrpcr),TABCHR,colstr(1:ncolcr),
     &                    TABCHR,B(icol),TABCHR,seb,TABCHR,cfix2
          ELSE 
           WRITE(fh1,1080)grpstr(1:ngrpcr),TABCHR,colstr(1:ncolcr),
     &                    TABCHR,B(icol),TABCHR,ZERO,TABCHR,cfix2
          END IF
         END IF
c-----------------------------------------------------------------------
         IF(Lgraf)THEN
          IF((.not.Regfx(icol)).and.lprtrs)THEN
           WRITE(fh2,1080)grpstr(1:ngrpcr),TABCHR,colstr(1:ncolcr),
     &                    TABCHR,B(icol),TABCHR,seb,TABCHR,cfix2
          ELSE 
           WRITE(fh2,1080)grpstr(1:ngrpcr),TABCHR,colstr(1:ncolcr),
     &                    TABCHR,B(icol),TABCHR,ZERO,TABCHR,cfix2
          END IF
         END IF
c-----------------------------------------------------------------------
         IF(Ldiag)THEN
          CALL reglbl(grpstr,ngrpcr,tmpstr,ntmp,Rgvrtp(icol))
          IF((.not.Regfx(icol)).and.lprtrs)THEN
           WRITE(Nform,2080)tmpstr(1:ntmp),'$',colstr(1:ncolcr),': ',
     &                      B(icol),' ',seb,' ',tval,' ',cfix2
          ELSE
           WRITE(Nform,2080)tmpstr(1:ntmp),'$',colstr(1:ncolcr),': ',
     &                      B(icol),' ',ZERO,' ',ZERO,' ',cfix2
          END IF
         END IF
        END DO
c-----------------------------------------------------------------------
c     For Seasonal, Trading day, and Stock Trading Day
c-----------------------------------------------------------------------
        IF((Lprtes.and.lprtrs).or.Ldiag)THEN
         ncolcr=0
         CALL setchr(' ',PCOLCR,colstr)
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
c-----------------------------------------------------------------------
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
c-----------------------------------------------------------------------
         ELSE IF(grpstr(1:min(8,ngrpcr)).eq.'Seasonal')THEN
          IF(Sp.eq.12)THEN
           ncolcr=3
           colstr(1:ncolcr)='Dec'
          IF(((.not.Lseff).and.index(grpstr(1:ngrpcr),'(before').gt.0)
     &       .or.index(grpstr(1:ngrpcr),'(change for before').gt.0)THEN
            ncolcr=5
            colstr(1:ncolcr)='Dec I'
           ELSE IF(index(grpstr(1:ngrpcr),'(starting').gt.0
     &         .or.index(grpstr(1:ngrpcr),'(change for after').gt.0)THEN
            ncolcr=6
            colstr(1:ncolcr)='Dec II'
           END IF
c-----------------------------------------------------------------------
          ELSE
           ipos=1
           CALL itoc(Sp,colstr,ipos)
           IF(Lfatal)RETURN
           IF(mod(Sp,100).ge.11.and.mod(Sp,100).le.13)THEN
            colstr(ipos:ipos+1)='th'
           ELSE
            colstr(ipos:ipos+1)=ordend(mod(Sp,10))
           END IF
           ncolcr=ipos+1
           IF(index(grpstr(1:ngrpcr),'(before').gt.0.or.
     &        index(grpstr(1:ngrpcr),'(change for before').gt.0)THEN
            colstr(ncolcr+1:ncolcr+2)=' I'
            ncolcr=ncolcr+2
           ELSE IF(index(grpstr(1:ngrpcr),'(starting').gt.0.or.
     &             index(grpstr(1:ngrpcr),'(change for after').gt.0)THEN
            colstr(ncolcr+1:ncolcr+3)=' II'
            ncolcr=ncolcr+3
           END IF
          END IF
         END IF
c-----------------------------------------------------------------------
         IF(ncolcr.gt.0)THEN
          IF(begcol.eq.endcol)THEN
           ldrvf1=T
           starz='**'
          ELSE
           ldrvfc=T
           starz=' *'
          END IF
          seb=ZERO
          cfix=' &nbsp;'
          cfix2='       '
          IF(Var.gt.ZERO)THEN
c-----------------------------------------------------------------------
c     Sum the coefficient estimates b(begcol) + ... + b(endcol).  Also
c compute the variance of this sum and the corresponding t-statistic
c (tstat).
c-----------------------------------------------------------------------
           sumb=-B(begcol)
           IF(regidx(begcol).eq.NOTSET)THEN
            baselt=NOTSET
            sumvar=0D0
           ELSE
            baselt=regidx(begcol)*(regidx(begcol)+1)/2
            sumvar=xpxinv(baselt)
           END IF
c-----------------------------------------------------------------------
           IF(begcol.eq.endcol)THEN
            sumb=sumb*TWOPT5
            IF(baselt.ne.NOTSET)seb=(sqrt(sumvar)*rmse)*TWOPT5
           ELSE
            DO icol=begcol+1,endcol
             sumb=sumb-B(icol)
             IF(regidx(icol).ne.NOTSET)THEN
              baselt=(regidx(icol)-1)*regidx(icol)/2
              sumvar=sumvar+xpxinv(baselt+regidx(icol))
c-----------------------------------------------------------------------
              DO jcol=begcol,icol-1
               IF(regidx(jcol).ne.NOTSET)
     &            sumvar=sumvar+TWO*xpxinv(baselt+regidx(jcol))
              END DO
             END IF
            END DO
            IF(baselt.ne.NOTSET)seb=sqrt(sumvar)*rmse
           END IF
c-----------------------------------------------------------------------
           IF(Lprtes)THEN
            CALL writTag(Mt1,'<tr>')
            Invl=Invl+1
            WRITE(thisId,1010)'vl',Invl
            Infoot=Infoot+1
            IF(ldrvfc)Vfoot(Infoot)=PSTAR1F
            IF(ldrvf1)Vfoot(Infoot)=PSTAR2F
            WRITE(Mt1,1060)thisId,starz,Infoot,starz,Infoot,
     &                        colstr(1:ncolcr)//' (derived)'
           END IF
           IF(baselt.ne.NOTSET)THEN
            tval=sumb/seb
            IF(Lprtes)THEN
             IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
              WRITE(thisHdr,1020)Ingr,Invl,'pe',Inpe
              WRITE(thisVal,1030)sumb
              CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
              WRITE(thisHdr,1020)Ingr,Invl,'se',Inse
              WRITE(thisVal,1040)seb
              CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
              WRITE(thisHdr,1020)Ingr,Invl,'tv',Intv
              WRITE(thisVal,1051)tval
              CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
             ELSE
              WRITE(thisHdr,1021)Invl,'pe',Inpe
              WRITE(thisVal,1030)sumb
              CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
              WRITE(thisHdr,1021)Invl,'se',Inse
              WRITE(thisVal,1040)seb
              CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
              WRITE(thisHdr,1021)Invl,'tv',Intv
              WRITE(thisVal,1051)tval
              CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
             END IF
            END IF
c-----------------------------------------------------------------------
           ELSE
            IF(Lprtes)THEN
             IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
              WRITE(thisHdr,1020)Ingr,Invl,'pe',Inpe
              WRITE(thisVal,1030)sumb
              CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
             ELSE
              WRITE(thisHdr,1021)Invl,'pe',Inpe
              WRITE(thisVal,1030)sumb
              CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
             END IF
             CALL mkTableCellSpan(Mt1,'col',2,'center','(fixed)')
            END IF
            cfix='(fixed)'
            cfix2=cfix
            seb=ZERO
            tval=ZERO
           END IF
          ELSE
           sumb=-B(begcol)
           IF(begcol.eq.endcol)THEN
            sumb=sumb*TWOPT5
           ELSE
            DO icol=begcol+1,endcol
             sumb=sumb-B(icol)
            END DO
           END IF
c-----------------------------------------------------------------------
           seb=ZERO
           tval=ZERO
           IF(Lprtes)THEN
            IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
             WRITE(thisHdr,1020)Ingr,Invl,'pe',Inpe
             WRITE(thisVal,1030)sumb
             CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
            ELSE
             WRITE(thisHdr,1021)Invl,'pe',Inpe
             WRITE(thisVal,1030)sumb
             CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
            END IF
           END IF
          END IF
          IF(Lprtes)CALL writTag(Mt1,'</tr>')
          IF(Ldiag)THEN
           CALL insstr(grpstr(1:ngrpcr)//'$'//colstr(1:ncolcr),ndrv,
     &                 PDRV,drvttl,drvptr,ndrvtl)
           IF(Lfatal)RETURN
           bdrv(ndrvtl)=sumb
           sedrv(ndrvtl)=seb
           fixdrv(ndrvtl)=cfix2
           tvdrv(ndrvtl)=tval
           ndrv=ndrv+1
          END IF
         END IF
        END IF
       END DO
       IF(Ldiag.and.ndrvtl.gt.0)THEN
        WRITE(Nform,1081)ndrvtl
        DO icol=1,ndrvtl
         CALL getstr(drvttl,drvptr,Ndrvtl,icol,drvstr,nchr)
         IF(Lfatal)RETURN
         WRITE(Nform,1082)drvstr(1:nchr),': ',bdrv(icol),' ',
     &                    sedrv(icol),' ',tvdrv(icol),' ',fixdrv(icol)
        END DO
       END IF
c-----------------------------------------------------------------------
c     Print the tail line and the change of regime regressor message
c and/or the derived factor message if there were any
c-----------------------------------------------------------------------
       IF(Lprtes)THEN
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
*        WRITE(Mt1,1020)('-',i=1,tbwdth)
        IF(tmsg.gt.0.or.((ldrvfc.or.ldrvf1).and.lprtrs))THEN
         CALL mkTableTag(Mt1,'w70','@')
         CALL mkCaption(Mt1,'Key for Regression Table')
        END IF
        IF(tmsg.gt.0)THEN
         lprrgm=F
         DO imsg=1,4
          IF(msg(imsg).gt.0)THEN
           CALL writTag(Mt1,'<tr>')
           CALL getstr(COGDIC,cogptr,PCOG,msg(imsg),grpstr,ngrpcr)
           IF(imsg.eq.1)THEN
            CALL mkTableCell(Mt1,'head','&amp;')
            CALL writtd('The I values estimate the '//
     &                  grpstr(1:ngrpcr)//' coefficients',Mt1,T,F)
            CALL writtd(' for the span of data before the change date.',
     &                  Mt1,F,T)
           ELSE IF(imsg.eq.2)THEN
            CALL mkTableCell(Mt1,'head','&amp;&amp;')
            CALL writtd('The I values estimate how much the early '//
     &                  grpstr(1:ngrpcr),Mt1,T,F)
            CALL writtd(' coefficients differ from those estimated '//
     &                  'for the span of data',Mt1,F,F)
            CALL writtd(' starting at the change date.',Mt1,F,T)
           ELSE IF(imsg.eq.3)THEN
            CALL mkTableCell(Mt1,'head','@')
            CALL writtd('The II values estimate the '//
     &                  grpstr(1:ngrpcr)//' coefficients',Mt1,T,F)
            CALL writtd(' for the span of data starting at the '//
     &                  'change date.',Mt1,F,T)
           ELSE 
            CALL mkTableCell(Mt1,'head','@@')
            CALL writtd('The II values estimate how much the early '//
     &                  grpstr(1:ngrpcr),Mt1,T,F)
            CALL writtd(' coefficients differ from those estimated '//
     &                  'for the span of data',Mt1,F,F)
            CALL writtd(' before the change date.',Mt1,F,T)
           END IF
           CALL writTag(Mt1,'</tr>')
          END IF
         END DO
        END IF
        IF(ldrvfc.and.lprtrs)THEN
         CALL writTag(Mt1,'<tr>')
         CALL mkTableCell(Mt1,'head','*')
         CALL writtd('For full trading-day and stable seasonal '//
     &       'effects, the derived',Mt1,T,F)
         CALL writtd(' parameter estimate is obtained indirectly '//
     &       'as minus the sum',Mt1,F,F)
         CALL writtd(' of the directly estimated parameters that '//
     &       'define the effect.',Mt1,F,T)
         CALL writTag(Mt1,'</tr>')
        END IF
        IF(ldrvf1.and.lprtrs)THEN
         CALL writTag(Mt1,'<tr>')
         CALL mkTableCell(Mt1,'head','**')
         CALL writtd('For the one coefficient trading-day effect, '//
     &       'the derived',Mt1,T,F)
         CALL writtd(' parameter estimate is obtained indirectly '//
     &       'as minus -2.5 times',Mt1,F,F)
         CALL writtd(' the directly estimated parameter that '//
     &       'defines the effect.',Mt1,F,T)
         CALL writTag(Mt1,'</tr>')
        END IF
        IF(tmsg.gt.0.or.((ldrvfc.or.ldrvf1).and.lprtrs))THEN
         CALL writTag(Mt1,'</table>')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END IF
       END IF
c-----------------------------------------------------------------------
c     Compute and print out the chi^2 tests for the seasonal effects,
c and trading day but not Automatically Identified Outliers.
c-----------------------------------------------------------------------
       IF(((Lprtes.and.lprchi).or.lsvchi).and.Iregfx.lt.3)THEN
        lprthd=F
        lprund=F
        IF(lprchi)lprthd=T
c-----------------------------------------------------------------------
        DO igrp=1,Ngrp
         begcol=Grp(igrp-1)
         CALL eltlen(igrp,Grp,Ngrp,numg)
         IF(Lfatal)RETURN
         IF((Rgvrtp(begcol).ne.PRGTAA.and.Rgvrtp(begcol).ne.PRGTAL.and.
     &       Rgvrtp(begcol).ne.PRGTAT.and.Rgvrtp(begcol).ne.PRGTUD.and.
     &       Rgvrtp(begcol).ne.PRGULM.and.Rgvrtp(begcol).ne.PRGULQ.and.
     &       Rgvrtp(begcol).ne.PRGULY).and.numg.gt.1)THEN
          IF(lprchi)lprund=T
          endcol=Grp(igrp)-1
          CALL getstr(Grpttl,Grpptr,Ngrp,igrp,str,nchr)
          IF(Lfatal)RETURN
          info=0
          baselt=regidx(begcol)
          df=endcol-begcol+1
          IF(Iregfx.eq.2)THEN
           IF(baselt.eq.NOTSET)df=df-1
           DO icol=begcol+1,endcol
            IF(regidx(icol).eq.NOTSET)THEN
             df=df-1
            ELSE
             baselt=regidx(icol)
            END IF
           END DO
          END IF
          CALL setchr(' ',31,ctype)
          ctype(1:10)='Regressors'
          IF(baselt.ne.NOTSET)
     &       CALL chitst(xpxinv,begcol,endcol,chi2vl,pv,regidx,T,info)
          CALL savchi(lsvchi,F,lprthd,tbwdth,baselt,str,nchr,info,df,
     &                chi2vl,pv,ctype,10,'chi$')
          IF(lprchi)THEN
           CALL prtchi(Mt1,lprthd,tbwdth,baselt,str,nchr,info,df,chi2vl,
     &                 pv,ctype,10,F)
           IF(lprthd)lprthd=F
          END IF
         END IF
        END DO
c-----------------------------------------------------------------------
        IF(lsvchi.or.lprchi)
     &     CALL cmpchi(xpxinv,regidx,lsvchi,F,lprchi,lprthd,tbwdth,F)
c-----------------------------------------------------------------------
c     Print the tail line
c-----------------------------------------------------------------------
        IF(lprchi.and.(.not.lprthd))THEN
         CALL writTag(Mt1,'</table></div>')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END IF
c-----------------------------------------------------------------------
c     print seasonal f-tests, if seasonal regressors are present
c     (BCM July 2007)
c-----------------------------------------------------------------------
        IF(lsvchi.or.lprchi.or.Svltab(LSLSFT))
     &     CALL sftest(xpxinv,regidx,lprchi,lsvchi,Svltab(LSLSFT),F)
c-----------------------------------------------------------------------
c     print trading day f-tests, if trading day regressors are present
c     (BCM July 2011)
c-----------------------------------------------------------------------
        IF(lsvchi.or.lprchi.or.Svltab(LSLTFT))
     &     CALL tdftest(xpxinv,regidx,lprchi,lsvchi,Svltab(LSLTFT),F)
       END IF
c-----------------------------------------------------------------------
c     Save the covariance matrix and print the correlation matrix
c of the regression variables.  If not printing out the regression
c standard errors don't print out related statistics.
c-----------------------------------------------------------------------
       IF(lprtrs)THEN
        IF(Lsavcm.and.Iregfx.lt.3)CALL svrgcm(nefobs,xpxinv,regidx)
        IF((.not.Lfatal).and.Lprtcm.and.Iregfx.lt.3)THEN
         CALL genSkip(LESTCM)
         CALL cormtx(xpxinv,regidx)
        END IF
c-----------------------------------------------------------------------
c     Print the temporary level-shift tests if requested.
c-----------------------------------------------------------------------
        IF((.not.Lfatal).and.Tlsrun.gt.1.and.(Prttls.or.Ldiag).and.
     &     Iregfx.lt.3)CALL templs(Lsrun,rmse,xpxinv,Prttls,Ldiag)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print out the ARMA parameters.  If the ARMA part of the model
c is fixed then only print out the estimates
c-----------------------------------------------------------------------
      begopr=Mdl(DIFF-1)
      beglag=Opr(begopr-1)
      endopr=Mdl(MA)-1
      IF(Ldiag)THEN
       WRITE(Nform,1000)'nonseasonaldiff: ',Nnsedf
       WRITE(Nform,1000)'seasonaldiff: ',Nseadf
       WRITE(Nform,1000)'nmodel: ',Opr(endopr)-Opr(Mdl(AR-1)-1)
      END IF
c-----------------------------------------------------------------------
      IF(lprtse.and.Convrg.and.Var.gt.2D0*dpmpar(1))THEN
       lprtse=T
       tbwdth=53
      ELSE
       lprtse=F
       tbwdth=37
      END IF
c-----------------------------------------------------------------------
      IF(Lprtes.and.(lprvar.or.endopr.gt.0))THEN
       endlag=Opr(endopr)-1
       CALL isfixd(MODEL,Arimaf,beglag,endlag,cfix)
       IF(Nb.gt.0)CALL mkPOneLine(Mt1,'@','&nbsp;')
       IF(Lestim.and.cfix.eq.'(fixed)')lprtse=F
c-----------------------------------------------------------------------
       IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL genSkip(1075)
       CALL writTagOneLine(Mt1,'h3','@','ARIMA Model')
       CALL mkPOneLine(Mt1,'@',
     &                 Mdlttl(1:Nmdlcr)//':  '//Mdldsn(1:Nmddcr))
       IF(.not.Lprtdf)THEN
        CALL mkPClass(Mt1,'indent')
        IF(Nnsedf.gt.0)WRITE(Mt1,1140)'Nonseasonal differences',Nnsedf
        IF(Nseadf.gt.0)THEN
         IF(Nnsedf.gt.0)CALL writTag(Mt1,Cbr)
         WRITE(Mt1,1140)'Seasonal differences',Nseadf
        END IF
        CALL writTag(Mt1,'</p>')
 1140   FORMAT('   ',a,':',t28,i2)
       END IF
c-----------------------------------------------------------------------
       WRITE(Mt1,100)Inpmdl,'.arma'
       CALL mkTableTag(Mt1,'w70','ARIMA Model')
       CALL mkCaption(Mt1,'ARIMA Model')
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
c-----------------------------------------------------------------------
       IF(lprtse)THEN
        Inpe=Inpe+1
        WRITE(thisId,1010)'pe',Inpe
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Estimate')
        Inse=Inse+1
        WRITE(thisId,1010)'se',Inse
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Standard Error')
        nGroup=3
c-----------------------------------------------------------------------
       ELSE IF(Lestim.and.cfix.eq.'(fixed)')THEN
        Inpe=Inpe+1
        WRITE(thisId,1010)'pe',Inpe
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Value (fixed)')
        nGroup=2
c     ------------------------------------------------------------------
       ELSE
        Inpe=Inpe+1
        WRITE(thisId,1010)'pe',Inpe
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Value')
        nGroup=2
       END IF
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
      IF(endopr.gt.0)THEN
       iestpm=0
c-----------------------------------------------------------------------
       IF(Lsaves.or.Lgraf)THEN
        IF(lprtse)THEN
         IF(Lsaves)WRITE(fh1,1180)TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                            TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                            TABCHR,TABCHR
         IF(Lgraf)WRITE(fh2,1180)TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                           TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                           TABCHR,TABCHR
 1180    FORMAT('$arima:',/,'$arima$estimates:',/,'operator',a,'factor',
     &          a,'period',a,'lag',a,'estimate',a,'standard error',a,
     &          'fixed',/,'--------',a,'------',a,'------',a,'---',a,
     &          '--------',a,'--------------',a,'-----')
c-----------------------------------------------------------------------
        ELSE
         IF(Lsaves)WRITE(fh1,1190)TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                            TABCHR,TABCHR,TABCHR,TABCHR,TABCHR
         IF(Lgraf)WRITE(fh2,1190)TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                           TABCHR,TABCHR,TABCHR,TABCHR,TABCHR
 1190    FORMAT('$arima:',/,'$arima$estimates:',/,'operator',a,'factor',
     &          a,'period',a,'lag',a,'estimate',a,'fixed',/,'--------',
     &          a,'------',a,'------',a,'---',a,'--------',a,'-----')
        END IF
       END IF
c-----------------------------------------------------------------------
       lfrtgr=T
c-----------------------------------------------------------------------
       IF(Lprtdf)THEN
        begfac=DIFF
       ELSE
        begfac=AR
       END IF
c     ------------------------------------------------------------------
       DO iflt=begfac,MA
        begopr=Mdl(iflt-1)
        endopr=Mdl(iflt)-1
c-----------------------------------------------------------------------
        DO iopr=begopr,endopr
         beglag=Opr(iopr-1)
         endlag=Opr(iopr)-1
c-----------------------------------------------------------------------
         CALL isfixd(OPRS,Arimaf,beglag,endlag,cfix)
         CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
         IF(Lprtes)THEN
          Ingr=Ingr+1
          WRITE(thisId,1010)'gr',Ingr
          CALL writTag(Mt1,'<tr>')
          IF(lprtse)THEN
           CALL mkHeaderCellId(Mt1,0,nGroup-1,thisId,'left','@',
     &                         tmpttl(1:ntmpcr))
          ELSE
           CALL mkHeaderCellId(Mt1,0,0,thisId,'left','@',
     &                         tmpttl(1:ntmpcr))
          END IF
          CALL mkTableCell(Mt1,'head',cfix)
c-----------------------------------------------------------------------
          CALL writTag(Mt1,'</tr>')
         END IF
c-----------------------------------------------------------------------
         DO ilag=beglag,endlag
          CALL isfixd(LAGS,Arimaf,ilag,ilag,cfix)
          IF(.not.Arimaf(ilag))iestpm=iestpm+1
c-----------------------------------------------------------------------
          IF(Lprtes)THEN
           CALL writTag(Mt1,'<tr>')
           Invl=Invl+1
           WRITE(thisId,1010)'vl',Invl
           WRITE(thisLag,1220)Arimal(ilag)
 1220      FORMAT('Lag',i3)
           CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',thisLag)
           WRITE(thisHdr,1020)Ingr,Invl,'pe',Inpe
           WRITE(thisVal,1040)Arimap(ilag)
           CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
           IF(.not.(Arimaf(ilag).or..not.lprtse))THEN
*            WRITE(Mt1,1220)Arimal(ilag)
            WRITE(thisHdr,1020)Ingr,Invl,'se',Inse
            WRITE(thisVal,1040)sqrt(Var*Armacm(iestpm,iestpm))
            CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
           ELSE IF(lprtse)THEN
            WRITE(thisHdr,1020)Ingr,Invl,'se',Inse
            IF(nGroup.gt.3)THEN
             CALL mkTableCellHeaderSpan(Mt1,'col',nGroup-2,thisHdr,
     &                                  'right',cfix)
            ELSE 
             CALL mkTableCellHeader(Mt1,thisHdr,'right',cfix)
            END IF
c-----------------------------------------------------------------------
           ELSE
            WRITE(thisHdr,1020)Ingr,Invl,'se',Inse
            IF(nGroup.ge.3)
     &         CALL mkTableCellHeader(Mt1,thisHdr,'right',cfix)
c-----------------------------------------------------------------------
           END IF
           CALL writTag(Mt1,'</tr>')
          END IF
c-----------------------------------------------------------------------
          DO spchr=ntmpcr,1,-1
           IF(tmpttl(spchr:spchr).eq.' ')GO TO 10
          END DO
          spchr=1
   10     IF(Lsaves.or.Lgraf.or.Ldiag)THEN
c-----------------------------------------------------------------------
           IF(.not.lprtse)THEN
            IF(Lsaves)WRITE(fh1,1250)tmpttl(spchr+1:ntmpcr),TABCHR,
     &                               tmpttl(1:spchr-1),TABCHR,
     &                               Oprfac(iopr),TABCHR,Arimal(ilag),
     &                               TABCHR,Arimap(ilag),TABCHR,cfix2
            IF(Lgraf)WRITE(fh2,1250)tmpttl(spchr+1:ntmpcr),TABCHR,
     &                              tmpttl(1:spchr-1),TABCHR,
     &                              Oprfac(iopr),TABCHR,Arimal(ilag),
     &                              TABCHR,Arimap(ilag),TABCHR,cfix2
            IF(Ldiag)WRITE(Nform,1261)tmpttl(spchr+1:ntmpcr),'$',
     &                              tmpttl(1:spchr-1),'$',Oprfac(iopr),
     &                              '$',Arimal(ilag),': ',Arimap(ilag),
     &                              ' ',ZERO,' ',ZERO,' ',cfix2
           ELSE IF(Arimaf(ilag))THEN
            IF(Lsaves)
     &         WRITE(fh1,1260)tmpttl(spchr+1:ntmpcr),TABCHR,
     &                        tmpttl(1:spchr-1),TABCHR,Oprfac(iopr),
     &                        TABCHR,Arimal(ilag),TABCHR,Arimap(ilag),
     &                        TABCHR,ZERO,TABCHR,cfix2
            IF(Lgraf)
     &         WRITE(fh2,1260)tmpttl(spchr+1:ntmpcr),TABCHR,
     &                        tmpttl(1:spchr-1),TABCHR,Oprfac(iopr),
     &                        TABCHR,Arimal(ilag),TABCHR,Arimap(ilag),
     &                        TABCHR,ZERO,TABCHR,cfix2
            IF(Ldiag)
     &         WRITE(Nform,1261)tmpttl(spchr+1:ntmpcr),'$',
     &                           tmpttl(1:spchr-1),'$',Oprfac(iopr),'$',
     &                           Arimal(ilag),': ',Arimap(ilag),' ',
     &                           ZERO,' ',ZERO,' ',cfix2
 1250       FORMAT(a,a,a,a,i2.2,a,i2.2,a,sp,e21.14,a,a)
 1260       FORMAT(a,a,a,a,i2.2,a,i2.2,a,sp,e21.14,a,e21.14,a,a)
 1261       FORMAT(a,a,a,a,i2.2,a,i2.2,a,sp,3(e21.14,a),a)
c-----------------------------------------------------------------------
           ELSE
            searma=sqrt(Var*Armacm(iestpm,iestpm))            
            IF(Lsaves)
     &         WRITE(fh1,1260)tmpttl(spchr+1:ntmpcr),TABCHR,
     &                        tmpttl(1:spchr-1),TABCHR,Oprfac(iopr),
     &                        TABCHR,Arimal(ilag),TABCHR,Arimap(ilag),
     &                        TABCHR,searma,TABCHR,cfix2
            IF(Lgraf)
     &         WRITE(fh2,1260)tmpttl(spchr+1:ntmpcr),TABCHR,
     &                        tmpttl(1:spchr-1),TABCHR,Oprfac(iopr),
     &                        TABCHR,Arimal(ilag),TABCHR,Arimap(ilag),
     &                        TABCHR,searma,TABCHR,cfix2
            IF(Ldiag)
     &         WRITE(Nform,1261)tmpttl(spchr+1:ntmpcr),'$',
     &                           tmpttl(1:spchr-1),'$',Oprfac(iopr),'$',
     &                           Arimal(ilag),': ',Arimap(ilag),' ',
     &                           searma,' ',Arimap(ilag)/searma,' ',
     &                           cfix2
           END IF
          END IF
         END DO
        END DO
       END DO
      END IF
      IF(Lprtes.and.(lprvar.or.endopr.gt.0))THEN
       CALL writTag(Mt1,'</table></div>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
c  Compute the standard error of the innovation variance, if printed or
c  saved (BCM March 2004)
c-----------------------------------------------------------------------
      IF((Lprtes.and.lprvar).or.Lgraf.or.(Ldiag.and.Convrg))THEN
       nefobs=Nspobs-Nintvl
       dnefob=dble(nefobs)
       seinov=sqrt(TWO/dnefob)*Var
      END IF
c-----------------------------------------------------------------------
      IF(Lprtes)THEN
       IF(lprvar)THEN
        WRITE(Mt1,100)Inpmdl,'.var'
        CALL mkTableTag(Mt1,'w60',
     &                  'Innovation Variance of the regARIMA model')       
        CALL mkCaption(Mt1,'Model Innovation Variance')
        WRITE(Mt1,1270)' Variance ',Var
        WRITE(Mt1,1270)' Standard Error of Variance ',seinov
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lsaves)THEN
       WRITE(fh1,1280)TABCHR,Var,TABCHR,seinov
       IF(Irev.eq.0.and.Issap.eq.0)THEN
        CALL fclose(fh1)
       ELSE
        CALL wrtdat(Begmdl,Sp,begstr,nbeg)
        IF(.not.Lfatal)CALL wrtdat(Endmdl,Sp,endstr,nend)
        IF(Lfatal)RETURN
        WRITE(fh1,1283)begstr(1:nbeg),endstr(1:nend)
        IF(((.not.Rvtran).and.Irev.gt.0).or.
     &     ((.not.Sstran).and.Issap.gt.0))WRITE(fh1,'(1x,a)')'-----'
       END IF                  
      END IF
      IF(Lgraf)THEN
       WRITE(fh2,1280)TABCHR,Var,TABCHR,seinov
       CALL fclose(fh2)
      END IF
      IF(Ldiag)THEN
       IF(Convrg)THEN
        WRITE(Nform,1281)'variance$mle: ',Var
        WRITE(Nform,1281)'variance$se: ',seinov
       ELSE
        WRITE(Nform,1281)'variance$mle: ',ZERO
        WRITE(Nform,1281)'variance$se: ',ZERO
       END IF
      END IF
c-----------------------------------------------------------------------
  100 FORMAT('<div id="mdl',i3.3,a,'">')
  110 FORMAT(/,'<p>Estimation converged in ',i5,' <abbr ',
     &         'title="autoregressive moving average">ARMA</abbr> ',
     &         'iterations, ',i5,' function evaluations.</p>')
 1000 FORMAT(a,i3)
 1010 FORMAT(a2,i4.4)
 1020 FORMAT('gr',i4.4,' vl',i4.4,' ',a2,i4.4)
 1021 FORMAT('vl',i4.4,' ',a2,i4.4)
 1030 FORMAT(f16.4)
 1040 FORMAT(f16.5)
 1050 FORMAT('$regression:',/,'$regression$estimates:',/,'group',a,
     &       'variable',a,'estimate',a,'standard error',a,
     &       'fixed',/,'-----',a,'--------',a,'-----------',a,
     &       '--------------',a,'-----')
 1051 FORMAT(f16.2)
 1060 FORMAT('<th id="',a,'">',a,/,'<a href="#footnote',i4.4,
     &       '" class="longdesc">','Link to definition of ',a,'</a>',/
     &       '<a name="foot',i4.4,'"></a>',a,'</th>')
 1070 FORMAT(a,a,t25,f14.4,:f16.5,:f13.2)
 1071 FORMAT(a,a,t25,f14.4,a16)
 1080 FORMAT(sp,a,a,a,a,e22.15,a,e22.15,a,a)
 1081 FORMAT('nregderived: ',i3)
 1082 FORMAT(sp,a,3(a,e22.15),a,a)
 1270 FORMAT('<tr><th scope="row">',a,'</th><td>',e33.5,'</td></tr>')
 1280 FORMAT(sp,'$variance:',//,'mle',a,e21.14,/,'se',a,e21.14)
 1281 FORMAT(a,e21.14)
 1282 FORMAT('converged: ',a)
 1283 FORMAT('$modelspan: ',a,' to ',a)
 2080 FORMAT(sp,a,a,a,3(a,e22.15),a,a)
c-----------------------------------------------------------------------
      RETURN
      END
