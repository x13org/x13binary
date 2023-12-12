C     Last change:  BCM  10 Dec 1998   10:33 am
      SUBROUTINE prtxrg(Lestim,Lprtes,Lsaves,Lprtcm,Lsavcm,Itbles,Fh,
     &                  Ldiag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Prints out the X-11 regression estimates, standard errors and
c     t-values
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
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
      DOUBLE PRECISION TWO,TWOPT5,ZERO
      PARAMETER(TWO=2D0,TWOPT5=2.5D0,ZERO=0D0)
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'cogreg.prm'
      INCLUDE 'xrgtbl.i'
c-----------------------------------------------------------------------
      INCLUDE 'htmlout.prm'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      INTEGER PTBLWD,PDRV
      PARAMETER(PTBLWD=PGRPCR+6,PDRV=4)
c-----------------------------------------------------------------------
      CHARACTER blnk*(5),colstr*(PCOLCR),grpstr*(PGRPCR),str*(PGRPCR),
     &          starz*(2),begstr*(10),endstr*(10),marker*(5),cfix*(7),
     &          fixdrv*(7),drvttl*((PCOLCR+PGRPCR+1)*PDRV),ctype*(31),
     &          drvstr*(PCOLCR+PGRPCR+1),thisId*6,thisHdr*20,thisVal*16,
     &          thisLag*6
      LOGICAL ldrvfc,ldrvf1,fcnok,Lestim,lfrtgr,linhol,linotl,lishol,
     &        lisotl,lnewgr,lprchi,Lprtes,lprthd,lprtrs,lprund,lprvar,
     &        Lsaves,Lprtcm,Lsavcm,Ldiag,lprrgm
      INTEGER baselt,begcol,endcol,Fh,i,icol,igrp,info,jcol,Itbles,
     &        nblnk,nchr,ncol,nefobs,nelt,ngrpcr,ncolcr,tbwdth,regidx,
     &        nfix,df,nb2,j,nbeg,nend,drvptr,ndrvtl,ndrv,imark,msg,
     &        imsg,tmsg,nGroup
      DOUBLE PRECISION chi2vl,dpmpar,pv,rmse,seb,sumb,sumvar,tmp,tval,
     &                 xpxinv,bdrv,sedrv,tvdrv
c      DIMENSION xpxinv(PB*(PB+1)/2),tmp(2),regidx(PB)
      DIMENSION xpxinv(PXPX),tmp(2),regidx(PB),bdrv(PDRV),sedrv(PDRV),
     &          drvptr(0:PDRV),msg(4),tvdrv(PDRV),fixdrv(0:PDRV)
c-----------------------------------------------------------------------
c  Bob Fay moved EXTERNAL statement up
c-----------------------------------------------------------------------
      EXTERNAL dpmpar
c-----------------------------------------------------------------------
      DATA blnk/'     '/
c-----------------------------------------------------------------------
      INCLUDE 'cogreg.var'
c-----------------------------------------------------------------------
c     Open the save file to print the estimates if necessary.
c-----------------------------------------------------------------------
      nb2=0
      ndrvtl=0
      ndrv=0
      cfix=' '
      IF(Ldiag)THEN
       CALL intlst(PDRV,drvptr,ndrvtl)
       ndrv=ndrvtl+1
      END IF
      IF(Lsaves.and.(Irev.le.1.and.Issap.le.1))THEN
       CALL opnfil(T,F,Itbles,Fh,fcnok)
       IF(.not.fcnok)THEN
        CALL abend
        RETURN
       END IF
      END IF
      tmsg=0
      CALL setint(0,4,msg)
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
      IF(Convrg)THEN
       IF(.not.Lhiddn.and.Lestim.and.Nestpm.gt.0)THEN
        IF(Lprtes)WRITE(Mt1,120)Nliter,Nfev
       END IF
      END IF
  120 FORMAT('<p>Estimation converged in',i5,' iterations,',i5,
     &       ' function evaluations.</p>')
c-----------------------------------------------------------------------
c     Print estimates only or SE and other tests.  If the model has not
c converged the standard errors, t-statistics, chi^2 tests, and
c MLE variance will not be printed out.
c-----------------------------------------------------------------------
      lprchi=Lprtes
      lprvar=Lprtes
c-----------------------------------------------------------------------
      lprtrs=T
      IF(Convrg.and.Var.gt.2D0*dpmpar(1))THEN
       tbwdth=PTBLWD
      ELSE
       lprtrs=F
       tbwdth=37
       lprchi=F
      END IF
      IF(Ldiag)WRITE(Nform,1000)'nxreg: ',Nb
 1000 FORMAT(a,i3)
c-----------------------------------------------------------------------
c     Find the number of columns in [X:y] and the number of regression
c variables.
c-----------------------------------------------------------------------
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
        CALL genSkip(1077)
        CALL writTagOneLine(Mt1,'h3','@','Irregular Regression Model')
        CALL mkTableTag(Mt1,'w70','Irregular Regression Model')
        CALL mkCaption(Mt1,'Irregular Regression Model')
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
         nGroup=2
        END IF
c-----------------------------------------------------------------------
        CALL writTag(Mt1,'</tr>')
       END IF
c-----------------------------------------------------------------------
       IF(Lsaves)WRITE(Fh,1050)TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,TABCHR
 1050  FORMAT('$regression:',/,'$regression$estimates:',/,'group',a,
     &        'variable',a,'estimate',a,'standard error',/,'-----',a,
     &        '--------',a,'-----------',a,'--------------')
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
     &       ((Rgvrtp(begcol).eq.PRGTEC.or.Rgvrtp(begcol).eq.PRGTEA)
     &         .and.(begcol-endcol).eq.0)
        lisotl=Rgvrtp(begcol).eq.PRGTAO
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
          IF(.not.lfrtgr.and.lnewgr)THEN
           IF(.not.((lishol.and.linhol).or.(lisotl.and.linotl)))
     &        WRITE(Mt1,'()')
          END IF
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
          IF((imark.ge.PRRTSE.and.imark.le.PRRTSL).or.(imark.ge.PRATSE
     &        .and.imark.le.PRATSL).or.(imark.ge.PRR1TD.and.
     &        imark.lt.PRGTUH))THEN
           IF(index(grpstr(1:ngrpcr),'change for after').gt.0)THEN
            marker(2:3)='@@'
            imsg=4
           ELSE IF(index(grpstr(1:ngrpcr),'change for before').gt.0)THEN
            marker(2:3)='&&'
            imsg=2
           ELSE IF(index(grpstr(1:ngrpcr),'starting').gt.0)THEN
            marker(3:3)='@'
            imsg=3
           ELSE
            marker(3:3)='&'
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
          END IF
c-----------------------------------------------------------------------
c     Print the regression estimates and possibly the standard errors
c and t-values.
c-----------------------------------------------------------------------
          cfix=' '
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
           CALL mkTableCellSpan(Mt1,'@',2,'center','(fixed)')
           cfix='(fixed)'
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
         IF(Lsaves)WRITE(Fh,1080)grpstr(1:ngrpcr),TABCHR,
     &                           colstr(1:ncolcr),TABCHR,B(icol),TABCHR,
     &                           seb,TABCHR,cfix
         IF(Ldiag)WRITE(Nform,2080)grpstr(1:ngrpcr),'$',
     &                             colstr(1:ncolcr),': ',B(icol),' ',
     &                             seb,' ',tval,' ',cfix
 1080    FORMAT(sp,a,a,a,a,e22.15,a,e22.15,a,a)
 2080    FORMAT(sp,a,a,a,3(a,e22.15),a,a)
        END DO
c-----------------------------------------------------------------------
c     For Trading day, and Stock Trading Day
c-----------------------------------------------------------------------
        IF(Lprtes.and.lprtrs)THEN
         ncolcr=0
         CALL setchr(' ',PCOLCR,colstr)
         IF((grpstr(1:min(11,ngrpcr)).eq.'Trading Day'.and.
     &      begcol.lt.endcol).or.
     &      grpstr(1:min(17,ngrpcr)).eq.'Stock Trading Day')THEN
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
         ELSE IF(grpstr(1:min(11,ngrpcr)).eq.'Trading Day'.and.
     &      begcol.eq.endcol)THEN
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
c-----------------------------------------------------------------------
         IF(ncolcr.gt.0)THEN
          IF(begcol.eq.endcol)THEN
           ldrvf1=T
           starz='**'
          ELSE
           ldrvfc=T
           starz=' *'
          END IF
          cfix= ' '
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
     &                     colstr(1:ncolcr)//' (derived)'
           END IF
           IF(baselt.ne.NOTSET)THEN
            tval=sumb/seb
            IF(Lprtes)THEN
             IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
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
            END IF
c-----------------------------------------------------------------------
           ELSE
            IF(Lprtes)THEN
             IF(grpstr(1:ngrpcr).ne.colstr(1:ncolcr))THEN
              WRITE(thisHdr,1020)Ingr,Invl,'pe',Inpe
              WRITE(thisVal,1030)B(icol)
              CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
             ELSE
              WRITE(thisHdr,1021)Invl,'pe',Inpe
              WRITE(thisVal,1030)B(icol)
              CALL mkTableCellHeader(Mt1,thisHdr(1:13),'right',thisVal)
             END IF
             CALL mkTableCellSpan(Mt1,'@',2,'center','(fixed)')
            END IF
            cfix='(fixed)'
            seb=ZERO
            tval=ZERO
           END IF
c-----------------------------------------------------------------------
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
             WRITE(thisVal,1030)B(icol)
             CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
            ELSE
             WRITE(thisHdr,1021)Invl,'pe',Inpe
             WRITE(thisVal,1030)B(icol)
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
           fixdrv(ndrvtl)=cfix
           tvdrv(ndrvtl)=tval
           ndrv=ndrv+1
          END IF
         END IF
        END IF
       END DO
       IF(Ldiag.and.ndrvtl.gt.0)THEN
        WRITE(Nform,1081)ndrvtl
 1081   FORMAT('nxregderived: ',i3)
        DO icol=1,ndrvtl
         CALL getstr(drvttl,drvptr,Ndrvtl,icol,drvstr,nchr)
         IF(Lfatal)RETURN
         WRITE(Nform,1082)drvstr(1:nchr),': ',bdrv(icol),' ',
     &                    sedrv(icol),' ',tvdrv(icol),' ',fixdrv(icol)
 1082    FORMAT(sp,a,3(a,e22.15),a,a)
        END DO
       END IF
c-----------------------------------------------------------------------
c     Print the tail line and the derived factor message if there were
c any
c-----------------------------------------------------------------------
       IF(Lprtes)THEN
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
        CALL mkTableTag(Mt1,'w70','@')
        CALL mkCaption(Mt1,'Key for Regression Table')
        IF(tmsg.gt.0)THEN
         lprrgm=F
         DO imsg=1,4
          IF(msg(imsg).gt.0)THEN
           CALL writTag(Mt1,'<tr>')
           CALL getstr(COGDIC,cogptr,PCOG,msg(imsg),grpstr,ngrpcr)
           IF(imsg.eq.1)THEN
            CALL mkTableCell(Mt1,'center','&amp;')
            CALL writtd('The I values estimate the '//grpstr(1:ngrpcr)//
     &                  ' coefficients',Mt1,T,F)
            CALL writtd(' for the span of data before the change date.',
     &                  Mt1,T,F)
           ELSE IF(imsg.eq.2)THEN
            CALL mkTableCell(Mt1,'center','&amp;&amp;')
            CALL writtd('The I values estimate how much the early '//
     &                  grpstr(1:ngrpcr)//' ',Mt1,T,F)
            CALL writtd(' coefficients differ from those estimated '//
     &                  'for the span of data',Mt1,F,F)
            CALL writtd(' starting at the change date.',Mt1,F,T)
           ELSE IF(imsg.eq.3)THEN
            CALL mkTableCell(Mt1,'center','@')
            CALL writtd('The II values estimate the '//
     &                  grpstr(1:ngrpcr)//' coefficients',Mt1,T,F)
            CALL writtd(' for the span of data starting at the '//
     &                  'change date.',Mt1,F,T)
           ELSE 
            CALL mkTableCell(Mt1,'center','@@')
            CALL writtd('The II values estimate how much the early '//
     &                  grpstr(1:ngrpcr),Mt1,T,F)
            CALL writtd(' coefficients differ from those estimated '//
     &                  'for the span of data',Mt1,F,F)
            CALL writtd(' before the change date.</td>',Mt1,F,T)
           END IF
           CALL writTag(Mt1,'</tr>')
          END IF
         END DO
        END IF
        IF(ldrvfc.and.lprtrs)THEN
         CALL writTag(Mt1,'<tr>')
         CALL mkTableCell(Mt1,'center','*')
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
         CALL mkTableCell(Mt1,'center','**')
         CALL writTag(Mt1,'<td>')
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
c-----------------------------------------------------------------------
c     Compute and print out the chi^2 tests for the seasonal effects,
c and trading day but not Automatically Identified Outliers.
c-----------------------------------------------------------------------
        IF(Iregfx.lt.3.and.(lprchi.or.Ldiag))THEN
         lprthd=T
         lprund=F
         IF(lprchi)lprthd=T
c-----------------------------------------------------------------------
         DO igrp=1,Ngrp
          CALL eltlen(igrp,Grp,Ngrp,ncol)
          IF(Lfatal)RETURN
          begcol=Grp(igrp-1)
          IF(ncol.gt.1.and.Rgvrtp(begcol).ne.PRGTAA)THEN
           lprund=T
           CALL getstr(Grpttl,Grpptr,Ngrp,igrp,str,nchr)
           IF(Lfatal)RETURN
           endcol=Grp(igrp)-1
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
     &        CALL chitst(xpxinv,begcol,endcol,chi2vl,pv,regidx,T,info)
           CALL savchi(Ldiag,F,lprthd,tbwdth,baselt,str,nchr,info,df,
     &                 chi2vl,pv,ctype,10,'chi$')
           IF(lprchi)THEN
            CALL prtchi(Mt1,lprthd,tbwdth,baselt,str,nchr,info,df,
     &                  chi2vl,pv,ctype,10,F)
            IF(lprthd)lprthd=F
           END IF
          END IF
         END DO
c-----------------------------------------------------------------------
         IF(Ldiag.or.lprchi)
     &      CALL cmpchi(xpxinv,regidx,Ldiag,F,lprchi,lprthd,tbwdth,T)
c-----------------------------------------------------------------------
c     Print the tail line
c-----------------------------------------------------------------------
         IF(lprchi.and.(.not.lprthd))THEN
          CALL writTag(Mt1,'</table></div>')
          CALL mkPOneLine(Mt1,'@','&nbsp;')
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Save the covariance matrix and print the correlation matrix
c of the regression variables.  If not printing out the regression
c standard errors don't print out related statistics.
c-----------------------------------------------------------------------
       IF(lprtrs)THEN
        IF(Lsavcm)CALL svrgcm(nefobs,xpxinv,regidx)
        IF(.not.Lfatal.and.Lprtcm.and.Iregfx.lt.3)THEN
         CALL genSkip(LXRXCM)
         CALL cormtx(xpxinv,regidx)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lprtes)THEN
       IF(lprvar)THEN
        CALL mkPClass(Mt1,'center')
        WRITE(Mt1,1270)' Variance ','&nbsp;&nbsp;&nbsp;',Var
 1270   FORMAT(a,a,e33.5)
        CALL writTag(Mt1,'</p>')
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lsaves)THEN
       WRITE(Fh,1140)TABCHR,Var
 1140  FORMAT(sp,'$variance:',/,'ols',a,e21.14)
       IF(Irev.eq.0.and.Issap.eq.0)THEN 
        CALL fclose(Fh)
       ELSE
        CALL wrtdat(Begxrg,Sp,begstr,nbeg)
        IF(.not.Lfatal)CALL wrtdat(Endxrg,Sp,endstr,nend)
        IF(Lfatal)RETURN
        WRITE(Fh,1282)begstr(1:nbeg),endstr(1:nend)
 1282   FORMAT('$x11regression$span: ',a,' to ',a)
        WRITE(Fh,'(1x,a)')'-----'
       END IF
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT(a2,i4.4)
 1020 FORMAT('gr',i4.4,' vl',i4.4,' ',a2,i4.4)
 1021 FORMAT('vl',i4.4,' ',a2,i4.4)
 1030 FORMAT(f16.4)
 1040 FORMAT(f16.5)
 1051 FORMAT(f16.2)
 1060 FORMAT('<th id="',a,'">',a,/,'<a href="#footnote',i4.4,
     &       '" class="longdesc">','Link to definition of ',a,'</a>',/
     &       '<a name="foot',i4.4,'"></a>',a,'</th>')
c-----------------------------------------------------------------------
      RETURN
      END
