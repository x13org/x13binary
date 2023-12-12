C     Last change:  BCM  29 Jan 1999    9:56 am
      SUBROUTINE prlkhd(Y,Adj,Adjmod,Fcntyp,Lam,Lsavst,Lprtst,Lprtfm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c adj     d  Pobs (nobs used) vector of preadjment factors
c AIC     d  Local scalar for the AIC = -2*(log(L)-np)
c aic     d  Local scalar for the F-corrected AIC
c             = -2*(log(L)-np/(1-(np+1)/nefobs))
c Bic     d  Local scalar for the BIC
c dnefob  d  Local double precision version of nefobs
c dotln   c  Local 60 character dotted line under the model title
c Aicc   d  Local scalar for AIC corrected for different orders of
c             differencing
c hnquin  d  Local scalar for the Hannan-Quinn
c i       i  Do loop index
c ilag    i  Local index for the current lag
c jacadj  d  Jocobian of the transformation
c lam     d  Box-Cox transformation parameter
c nlag    i  Local number of lags in the all the components of the
c             structural model
c np      i  Local number of estimated parameters including the signal
c             variance
c one     d  Local PARAMETER for a double precision 1
c pi      d  Local PARAMETER for pi
c two     d  Local PARAMETER for a double precision 2
c y       d  Pobs (nobs used) vector of original untransformed
c             undifferenced series
c zero    d  Local PARAMETER for a double precision 0
c-----------------------------------------------------------------------
c     Data typing and initialization
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER NDOTS
      DOUBLE PRECISION ONE,TWO,ZERO
      PARAMETER(ONE=1D0,NDOTS=66,TWO=2.0D0,ZERO=0D0,F=.false.,T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'lkhd.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      INCLUDE 'mdltbl.i'
c     ------------------------------------------------------------------
c      INCLUDE 'svllog.prm'
c      INCLUDE 'svllog.cmn'
c      INCLUDE 'mdlsvl.i'
c     ------------------------------------------------------------------
      CHARACTER star*1
      LOGICAL lclaic,Lprtfm,locok,Lsavst,Lprtst,lprt
      INTEGER endlag,Fcntyp,fh,i,igrp,ilag,nefobs,np,Adjmod,dsp,fh0
      DOUBLE PRECISION Adj,dnefob,Lam,Y,jacadj,jaci,yi
      DIMENSION Adj(*),Y(*)
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL dpeq
      EXTERNAL dpeq,strinx
c-----------------------------------------------------------------------  
      IF(Fixmdl.eq.3)THEN
       igrp=strinx(F,Grpttl,Grpptr,1,Ngrptl,
     &             'Automatically Identified Outliers')
       IF(igrp.gt.0)THEN
        CALL nWritln('Likelihood statistics are not printed out '//
     &               'when the regARIMA model',Mt1,0,T,F)
        CALL writln(' is fixed and automatic outlier '//
     &              'identification is performed.',Mt1,0,F,T)
        RETURN
       END IF
      END IF
      nefobs=Nspobs-Nintvl
      dnefob=dble(nefobs)
      CALL dfdate(Begspn,Begbk2,Sp,dsp)
c-----------------------------------------------------------------------
c     Calculate the AIC.  First find the number of estimated parameters,
c including the regression and ARIMA parameters, and the variance.
c-----------------------------------------------------------------------
      np=Ncxy
      IF(Nb.gt.0)THEN
       DO ilag=1,Nb
        IF(Regfx(ilag))np=np-1
       END DO
      END IF
      lclaic=(Mdl(AR)-Mdl(DIFF).eq.0.or.Lar).and.
     &       (Mdl(MA)-Mdl(AR).eq.0.or.Lma)
c     ------------------------------------------------------------------
      IF(Nopr.gt.0)THEN
       endlag=Opr(Nopr)-1
       DO ilag=1,endlag
        IF(.not.Arimaf(ilag))np=np+1
       END DO
      END IF
c     ------------------------------------------------------------------
      lprt=.not.Lhiddn.and.Lprtst
      IF(lprt)THEN
       Inplkh=Inplkh+1
       WRITE(Mt1,1000)Inplkh
       CALL mkTableTag(Mt1,'w60','Likelihood Statistics')       
       CALL mkCaption(Mt1,'Likelihood Statistics')
c     ------------------------------------------------------------------
       WRITE(Mt1,1030)'Number of observations (nobs)',Nspobs
       WRITE(Mt1,1030)'Effective number of observations (nefobs)',nefobs
       WRITE(Mt1,1030)'Number of parameters estimated (np)',np
      END IF
c-----------------------------------------------------------------------
c     Open a file to save the likelihood statistics
c-----------------------------------------------------------------------
      locok=.true.
      IF(Lsavst)THEN
       CALL opnfil(.true.,.false.,LESTST,fh,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
       WRITE(fh,*)
       WRITE(fh,1040)'nobs',Nspobs
       WRITE(fh,1040)'nefobs',nefobs
       WRITE(fh,1040)'d',Nintvl
       WRITE(fh,1040)'np',np
       WRITE(fh,1050)'lndetcov',Lndtcv
      END IF
c-----------------------------------------------------------------------
c     Calculate the jacobian of the transformation and print out the
c AIC's if valid to do so.
c-----------------------------------------------------------------------
      IF(Var.le.ZERO)THEN
       CALL writTag(Mt1,'</table></div>')
       fh0=0
       IF(lprt)fh0=Mt1
       CALL nWritln('Can''t calculate a log likelihood for a '//
     &              'model with no variance',fh0,Mt2,T,T)
       IF(Lsavst)WRITE(fh,1050)'var',ZERO
c     ------------------------------------------------------------------
      ELSE
       IF(Lsavst)WRITE(fh,1050)'var',Var
       jacadj=ZERO
       IF(Adjmod.lt.2)THEN
c     ------------------------------------------------------------------
*        DO i=1,Nspobs
        DO i=Nintvl+1,Nspobs
         IF((.not.dpeq(Y(i),0D0)).AND.(.not.dpeq(Adj(i),0D0)))THEN
c-----------------------------------------------------------------------
c Sometimes this equality statement doesn't work but the only way around
c  it is to make a FuzzyEquals(doubleA,doubleB,MachinePrecision) where
c it would test if abs(A-B)<MachinePrecision.  This would slow the
c computations down a lot.  The machine and language should do this
c automatically.
c-----------------------------------------------------------------------
c     If there is a prior adjustment get the log(jacobian) of the
c adjustment.
c-----------------------------------------------------------------------
c          IF(dpeq(Adj(i),ONE))THEN
c           jaci=ONE
c          ELSE
          jaci=Adj(i)
          IF(Khol.gt.0)jaci=jaci*X11hol(i+dsp)
          IF(Ixreg.gt.0.and.(Axrgtd.or.Axrghl))THEN
           IF(Muladd.eq.1)THEN
            jaci=jaci+Faccal(i+dsp)
           ELSE
            jaci=jaci*Faccal(i+dsp)
           END IF
          END IF
c          END IF
c-----------------------------------------------------------------------
c     If there is a transformation take the log(jacobian) of the
c adjusted series using the chain rule, f'(y/a)a^{-1}
c-----------------------------------------------------------------------
          IF(Fcntyp.ne.4)THEN
           yi=Y(i)/jaci
c-----------------------------------------------------------------------
c     Logit
c-----------------------------------------------------------------------
           IF(Fcntyp.eq.3)THEN
*            jaci=ONE/yi/(ONE-yi)/jaci
*            jaci=(jaci*Y(i)-Y(i)**2)/jaci
            jaci=jaci/(jaci*Y(i)-Y(i)**2)
c-----------------------------------------------------------------------
c     Boxcox, which includes the log and square root
c-----------------------------------------------------------------------
           ELSE IF(.not.dpeq(Lam,ONE))THEN
            jaci=(abs(yi)**(Lam-ONE))/jaci
           END IF
          END IF
c     ------------------------------------------------------------------
c     Sum logs for the log jacobian
c-----------------------------------------------------------------------
          jacadj=jacadj+log(jaci)
         END IF
        END DO
       ELSE IF((.not.dpeq(Lam,ONE)).or.Fcntyp.eq.3)THEN
        DO i=Nintvl+1,Nspobs
         jaci=ONE
         yi=Y(i)
c-----------------------------------------------------------------------
c     Logit
c-----------------------------------------------------------------------
         IF(Fcntyp.eq.3)THEN
*          jaci=ONE/yi/(ONE-yi)/jaci
*          jaci=(jaci*Y(i)-Y(i)**2)/jaci
          jaci=jaci/(jaci*Y(i)-Y(i)**2)
c-----------------------------------------------------------------------
c     Boxcox, which includes the log and square root
c-----------------------------------------------------------------------
         ELSE IF(.not.dpeq(Lam,ONE))THEN
          jaci=abs(yi)**(Lam-ONE)/jaci
         END IF
c     ------------------------------------------------------------------
c     Sum logs for the log jacobian
c-----------------------------------------------------------------------
         jacadj=jacadj+log(jaci)
        END DO
       END IF
c     ------------------------------------------------------------------
       IF(dpeq(jacadj,ZERO))THEN
        IF(lprt)THEN
         WRITE(Mt1,1070)' Log likelihood (L)',Lnlkhd
        END IF
        IF(Lsavst)WRITE(fh,1050)'lnlkhd',Lnlkhd
        Olkhd=Lnlkhd
c     ------------------------------------------------------------------
       ELSE
        IF(lprt)THEN
         WRITE(Mt1,1070)' Log likelihood',Lnlkhd
         WRITE(Mt1,1070)' Transformation Adjustment',jacadj
         WRITE(Mt1,1070)' Adjusted Log likelihood (L)',Lnlkhd+jacadj
        END IF
c     ------------------------------------------------------------------
        IF(Lsavst)THEN
         WRITE(fh,1050)'trnadj',jacadj
         WRITE(fh,1050)'lnlkhd',Lnlkhd
        END IF
        Olkhd=Lnlkhd+jacadj
       END IF
c-----------------------------------------------------------------------
c      AIC=-two*(lnlkhd-dble(np))
c-----------------------------------------------------------------------
       IF(.not.lclaic)THEN
        fh0=0
        IF(Issap.lt.2.and.Irev.lt.4)THEN
         IF(lprt)THEN
          CALL writTag(Mt1,'</table></div>')
          fh0=Mt1
         END IF
         CALL nWritln('<abbr title="Akaike information criterion">AIC'//
     &                '</abbr> and related statistics are printed '//
     &                'only for exact maximum likelihood estimation.',
     &                Mt2,fh0,T,T)
        END IF
        IF(Irev.eq.4)RETURN
       ELSE IF(Convrg)THEN
        IF(Axrgtd.or.Axrghl.or.Axhol)THEN
         star='*'
        ELSE
         star=' '
        END IF
        Aic=-TWO*(Lnlkhd+jacadj-dble(np))
        IF(lprt)WRITE(Mt1,1100)
     &     ' <abbr title="Akaike information criterion">AIC</abbr>',
     &     Aic,star
        IF(Lsavst)WRITE(fh,1050)'aic',Aic
c     ------------------------------------------------------------------
        IF(nefobs.gt.np+1)THEN
         Aicc=-TWO*(Lnlkhd+jacadj-dnefob*dble(np)/(dnefob-dble(np+1)))
         IF(lprt)WRITE(Mt1,1100)
     &      ' <abbr title="corrected Akaike information criterion">'//
     &      'AICC</abbr> (F-corrected-<abbr title="Akaike '//
     &      'information criterion">AIC</abbr>)',Aicc,star
         IF(Irev.eq.4)RETURN
         IF(Lsavst)WRITE(fh,1050)'Aicc',Aicc
        END IF
c     ------------------------------------------------------------------
        Hnquin=-TWO*(Lnlkhd+jacadj-log(log(dnefob))*dble(np))
        IF(lprt)WRITE(Mt1,1100)' Hannan Quinn ',Hnquin,star
        IF(Lsavst)WRITE(fh,1050)'hnquin',Hnquin
c     ------------------------------------------------------------------
        Bic=-TWO*(Lnlkhd+jacadj)+dble(np)*log(dnefob)
        IF(lprt)WRITE(Mt1,1100)
     &     ' <abbr title="Bayesian information criterion">BIC</abbr> ',
     &     Bic,star
        IF(Lsavst)WRITE(fh,1050)'bic',Bic
c     Compute BIC2 for automatic model identification procedure
c     computed as in TRAMO - BCM 2/2000
        Bic2=(-TWO*Lnlkhd+dble(np)*log(dnefob))/dnefob
c     ------------------------------------------------------------------
c     compute EIC as in Khandakar and Hyndman (2007) if EICk is
c     specified by the user - BCM 5/2008
c     ------------------------------------------------------------------
        IF(Eick.gt.ZERO)THEN
         Eic=-TWO*(Lnlkhd+jacadj)+dble(np)*Eick
         IF(lprt)WRITE(Mt1,1131)Eick,Eic,star
         IF(Lsavst)THEN
          WRITE(fh,1050)'eic',Eic
          WRITE(fh,1050)'eick',Eick
         END IF
        ELSE
         Eic=DNOTST
        END IF
c     ------------------------------------------------------------------
        IF(lprt)THEN
         CALL writTag(Mt1,'</table></div>')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END IF
c     ------------------------------------------------------------------
        IF(lprt.and.(Axrgtd.or.Axrghl.or.Axhol))THEN
         IF(Axhol.and.Axrgtd)THEN
          WRITE(Mt1,1170)
         ELSE IF(Axhol)THEN
          WRITE(Mt1,1180)
         ELSE
          WRITE(Mt1,1190)
         END IF
        END IF
c     ------------------------------------------------------------------
       ELSE
        Aic=DNOTST
        Aicc=DNOTST
        Bic=DNOTST
        Hnquin=DNOTST
        Lnlkhd=DNOTST
        Eic=DNOTST
        IF(Irev.eq.4)RETURN
       END IF
c     ------------------------------------------------------------------
       IF(Lprtfm)THEN
        CALL writTag(Mt1,Charhr)
c     ------------------------------------------------------------------
        IF(Lextar)THEN
         WRITE(Mt1,1140)Cbr,' ',Cbr,Cbr,Cbr,Cbr,Cbr,Cbr
        ELSE
         WRITE(Mt1,1140)Cbr,' <abbr title="autoregression">AR</abbr> '//
     &                  'and ',Cbr,Cbr,Cbr,Cbr,Cbr,Cbr
        END IF
c     ------------------------------------------------------------------
        IF(Convrg.and.lclaic)THEN
         IF(.not.dpeq(jacadj,ZERO))WRITE(Mt1,1150)Cbr
         WRITE(Mt1,1160)Cbr,Cbr,Cbr,'</p>'
        END IF
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Lsavst.and.locok)CALL fclose(fh)
c     ------------------------------------------------------------------
 1000 FORMAT('<div id="lkhd',i3.3,'">')
 1030 FORMAT('<tr><th scope="row">',a,'</th><td>',i6,'</td></tr>')
 1040 FORMAT(a,' ',i16)
 1050 FORMAT(a,' ',e29.15)
 1070 FORMAT('<tr><th scope="row">',a,'</th><td>',f40.4,'</td></tr>')
 1100 FORMAT('<tr><th scope="row">',a,'</th><td>',f40.4,1x,a,
     &       '</td></tr>')
 1131 FORMAT('<tr><th scope="row"> <abbr title="'//
     &       'empiricle information criterion">EIC</abbr> (k=',f6.2,
     &       ')</th><td>',f40.4,1x,a,'</td></tr>')
 1140 FORMAT('<p>nobs   = number of observations',a,/,
     &       ' nefobs = nobs  - total order of',a,'differencing ',
     &       'operators',a,/,
     &       ' np     = number of estimated regression and ARIMA model'
     &       ,/,'            parameters including the variance',a,/,
     &       ' V      = Covariance matrix of z''s',a,/,
     &       ' z      = Vector of data - regression values',a,/,
     &       ' c      = Vector of prior adjustment factors',a,/,
     &       ' Log likelihood =',/,
     &       '          -[nefobs*log(2*pi)+log|V|+z''(V-inverse)z]/2',a)
 1150 FORMAT(
     &    ' Transformation Adjustment = sum(ln(|(ci*(ci*yi)^(lam-1))|))'
     &    ,/' where the sum is over the last nefobs observations',a)
 1160 FORMAT(' <abbr title="Akaike information criterion">AIC</abbr>'//
     &       ' = -2*L+2*np',a,/,
     &       ' <abbr title="corrected Akaike information criterion">',
     &       'AICC</abbr> = -2*L+2*np*[nefobs/(nefobs-np-1)]',a,/,
     &       ' Hannan Quinn = -2*L+2*np*log[log(nefobs)]',a,/,
     &       ' <abbr title="Bayesian information criterion">BIC</abbr>',
     &       ' = -2*L+np*log(nefobs)',a)
 1170 FORMAT(' <p> * <strong>NOTE:</strong> ',
     &       'These statistics do not contain a penalty for ',
     &       'parameters',/,9x,
     &       'estimated by the x11regression and x11 specs to ',
     &       'produce the',/,9x,
     &       'prior adjustment factors because these estimates are not',
     &       /,9x,
     &       'maximum likelihood estimates.  Therefore they cannot be',
     &       /,9x,
     &       'compared to the statistics from models in which ',
     &       'regression',/,9x,
     &       'variables in a regARIMA model are used to estimate the',
     &       'same',/,9x,'effects.</p>')
 1180 FORMAT(' <p> * <strong>NOTE:</strong> ',
     &       'These statistics do not contain a penalty ',
     &       'for parameters',/,9x,
     &       'estimated by X-11 to produce the prior adjustment ',
     &       'factors,',/,9x,
     &       'because the X-11 estimates are not maximum likelihood',
     &       /,9x,
     &       'estimates.  Therefore they cannot be compared to the',
     &       /,9x,
     &       'statistics from models in which regression variables ',
     &       'in a',/,9x,
     &       'regARIMA model are used to estimate the same effects.',
     &       '</p>')
 1190 FORMAT(' <p> * <strong>NOTE:</strong> ',
     &       'These statistics do not contain a penalty ',
     &       'for parameters',/,9x,
     &       'estimated by x11regression to produce the prior ',
     &       'adjustment',/,9x,
     &       'factors because the x11regression estimates are not ',
     &       'maximum',/,9x,
     &       'likelihood estimates.  Therefore they cannot be ',
     &       ' compared to ',/,9x,
     &       'the statistics from models in which regression ',
     &       'variables in',/,9x,
     &       'a regARIMA model are used to estimate the same effects.',
     &       '</p>')
      RETURN
      END
