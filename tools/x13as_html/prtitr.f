      SUBROUTINE prtitr(A,Na,Parms,Nparms,Itrlbl,Iter,Nfev)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     prtitr.f, Release 1, Subroutine Version 1.8, Modified 16 Feb 1995.
c-----------------------------------------------------------------------
c     Prints out the iteration, either the nonlinear or the
c overall iteration, the deviance, |G'G|**(1/nsrs)*a'a, and the
c parameter estimates.  Note that a'a must already have the determinate
c factored in.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c a      d    Input either an array of exact likelihood values
c              a*|G'G|**(.5/nsrs) or its sum of squares
c dev    d    Local scalar for the deviance, the log likelihood without
c              constants, log(detcov)+sum(e(t)^2/v(t),t=1,nefobs)
c i      i    Local do loop index for write
c iter   i    Input number of iterations either overall or nonlinear
c              iterations
c itrlbl c    Input label to identify whether these are regression
c              parameters or ARMA nonlinear parameters
c na     i    Input number of a's to sum.  If na > 1 then the routine
c              is called from lmdif and the non linear parameters are
c              to be printed.  If na = 1 then the regression parmeters
c              are to be printed and the dev has already been summed.
c nfev   i    Input number of function evaluations
c nparms i    Input number of parmeters to print
c parms  d    Input parameters
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'series.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      DOUBLE PRECISION ONE,PI,TWO,ZERO
      PARAMETER(T=.true.,F=.false.,ZERO=0D0,ONE=1D0,TWO=2D0,
     &          PI=3.14159265358979d0)
c     ------------------------------------------------------------------
c     Changed by BCM Feb 1996 to ensure iteration information is printed
c     in multiple runs.
c     ------------------------------------------------------------------
      LOGICAL Frstcl,Scndcl
      COMMON /lgiter / Frstcl,Scndcl
c     ------------------------------------------------------------------
      CHARACTER Itrlbl*(4)
      INTEGER i,Iter,Na,Nfev,Nparms,ovrlit
      DOUBLE PRECISION A(*),dev,lnlkhd,Parms(Nparms)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      IF(Scndcl)THEN
       IF(Nb.eq.0)THEN
        CALL genSkip(1072)
        CALL writTagOneLine(Mt1,'h3','@',
     &               '<abbr title="autoregressive moving average">'//
     &               'ARMA</abbr> Iterations')
c     ------------------------------------------------------------------
       ELSE
        CALL genSkip(1073)
        CALL writTagOneLine(Mt1,'h3','@','regARIMA Iterations')
        WRITE(Mt1,1020)Cbr
 1020   FORMAT(/,'<p><strong><abbr title="iterative generalized ',
     &           'least squares">IGLS</abbr>:</strong>',
     &           ' Estimate regression parameters given last values ',
     &           'of <abbr title="autoregressive moving average">',
     &           'ARMA</abbr> parameters.',a,
     &         /,'  <strong><abbr title="autoregressive moving ',
     &           'average">ARMA</abbr>:</strong>',
     &           'Estimate <abbr title="autoregressive moving ',
     &           'average">ARMA</abbr> parameters using residuals ',
     &           'from last IGLS regression.</p>',
     &         /,'<p><strong>NOTE:</strong> <abbr ',
     &           'title="autoregressive moving average">ARMA</abbr> ',
     &           'iteration counts are cumulative over <abbr title="',
     &           'iterative generalized least squares">IGLS</abbr> ',
     &           'iterations.')
       END IF
c       Frstcl=F
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
c     Calculate the log likelihood from the deviance
c-----------------------------------------------------------------------
      IF(Na.gt.0)THEN
       IF(Na.gt.1)THEN
        CALL yprmy(A,Na,dev)
       ELSE
        dev=A(1)
       END IF
       IF(dpeq(dev,ZERO))THEN
        lnlkhd=ZERO
       ELSE
        lnlkhd=-Dnefob/TWO*(log(TWO*PI*dev/Dnefob)+ONE)
       END IF
c     ------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
c     If printing out the parameters because of an error don't print the
c labels.  Print out the initial values on the first call and print 
c the iteration headers without a trailing blank line on the second
c call.  Only print the initial log likelihood on pure ARMA models.
c-----------------------------------------------------------------------
      IF(Iter.ne.NOTSET)THEN
       IF(Frstcl)THEN
        Scndcl=T
c     ------------------------------------------------------------------
        IF(Nb.eq.0)THEN
         WRITE(Mt1,1030)' <abbr title="autoregressive moving '//
     &                  'average">ARMA</abbr> parameters'
 1030    FORMAT(/,'<p> Initial values for the',a)
         WRITE(Mt1,1040)lnlkhd
 1040    FORMAT('  Log Likelihood',1p,e23.9,'</p>')
        ELSE
         WRITE(Mt1,1030)' '
        END IF
c     ------------------------------------------------------------------
       ELSE
        IF(.not.Scndcl)THEN
         WRITE(Mt1,'(1x)')
        ELSE IF(.not.Frstcl)THEN
         Scndcl=F
        END IF
c     ------------------------------------------------------------------
        CALL mkTableTag(Mt1,'w60','@')
        IF(Nb.eq.0)THEN
         WRITE(Mt1,1050)'Iteration',Iter
 1050    FORMAT('<tr><th class="iter">',a,'</th><td class="left">',i10,
     &          '</td></tr>')
c     ------------------------------------------------------------------
        ELSE IF(Itrlbl.eq.'IGLS')THEN
         WRITE(Mt1,1050)'<abbr title="iterative generalized least '//
     &                  'squares">IGLS</abbr> Iteration',Iter
        ELSE IF(Itrlbl.eq.'ARMA')THEN
         WRITE(Mt1,1050)'<abbr title="autoregressive moving average">'//
     &                  'ARMA</abbr> Iteration',Iter
        ELSE
         WRITE(Mt1,1050)Itrlbl//' Iteration',Iter
        END IF
c     ------------------------------------------------------------------
        WRITE(Mt1,1050)'  Function evaluations',Nfev
c     ------------------------------------------------------------------
        WRITE(Mt1,1070)lnlkhd
 1070   FORMAT('<tr><th class="iter">Log Likelihood</th>',
     &         '<td class="left">',1p,e23.9,'</td></tr>')
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Frstcl)THEN
       IF(Itrlbl.eq.'IGLS')THEN
        WRITE(Mt1,1080)'Regression',(Parms(i),i=1,Nparms)
 1080   FORMAT('  ',a,' parameters',5x,3g23.9,/,(t22,3g23.9))
        ovrlit=Iter
c     ------------------------------------------------------------------
       ELSE
        WRITE(Mt1,1080)'<abbr title="autoregressive moving average">'//
     &                 'ARMA</abbr>',(Parms(i),i=1,Nparms)
       END IF
       CALL writTag(Mt1,'</p>')
      ELSE
c     ------------------------------------------------------------------
       IF(Itrlbl.eq.'IGLS')THEN
        WRITE(Mt1,1090)'Regression parameters',(Parms(i),i=1,Nparms)
 1090   FORMAT('<tr><th class="iter">',a,'</th><td class="left">',
     &         3g23.9,/,(t22,3g23.9))
        ovrlit=Iter
       ELSE
        WRITE(Mt1,1090)'<abbr title="autoregressive moving average">'//
     &                 'ARMA</abbr> parameters',(Parms(i),i=1,Nparms)
       END IF
       CALL writTag(Mt1,'</td></tr>')
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
      IF(Savtab(LESTIT))THEN
       CALL savitr(F,ovrlit,Iter,lnlkhd,Parms,Nparms)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
      Frstcl=F
c     ------------------------------------------------------------------
      RETURN
      END
