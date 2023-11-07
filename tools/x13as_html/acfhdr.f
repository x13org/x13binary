      SUBROUTINE acfhdr(Mt1,Ndf,Nsdf,Iflag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     print title to acf and pacf tables and plots
c iflag   i   indicator for PACF and ACF, i = 1,6 PACF, i = 2,4 ACF,
c               i=3,5 ACF of squared residuals
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     ------------------------------------------------------------------
      CHARACTER ctargt*(50)
      DOUBLE PRECISION Lam
      INTEGER Fcntyp,Mt1,Ndf,Nsdf,Iflag,ntargt
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      COMMON /armalm/ Lam,Fcntyp
c-----------------------------------------------------------------------
      IF(Ndf.eq.NOTSET)THEN
       IF(Iflag.eq.3.or.Iflag.eq.5)THEN
        ctargt(1:17)='Squared Residuals'
        ntargt=17
       ELSE
        ctargt(1:9)='Residuals'
        ntargt=9
       END IF
      ELSE
       IF(Nb.gt.0)THEN
        ctargt(1:20)='Regression Residuals'
        ntargt=20
       ELSE
        ctargt(1:6)='Series'
        ntargt=6
        IF(dpeq(Lam,0D0).or.Kfmt.gt.0)THEN
         ctargt(7:8)=' ('
         ntargt=ntargt+2
         IF(dpeq(Lam,0D0))THEN
          ctargt(9:19)='Transformed'
          ntargt=ntargt+11
          IF(Kfmt.gt.0)THEN
           ctargt(20:21)=', '
           ntargt=ntargt+2
          END IF
         END IF
         IF(Kfmt.gt.0)THEN
          ctargt(ntargt+1:ntargt+11)='Preadjusted'
          ntargt=ntargt+11
         END IF
         ctargt(ntargt+1:ntargt+1)=')'
         ntargt=ntargt+1
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Write out header for the plot
c-----------------------------------------------------------------------
      IF(Iflag.eq.1.or.Iflag.eq.6)THEN
       IF(Iflag.eq.6)THEN
        CALL makeSkipLink(Mt1,Idxtab,'ASCII plot of '//
     &                   'Sample Partial Autocorrelation of the '//
     &                    ctargt(1:ntargt),T)
       END IF
       CALL writTagOneLine(Mt1,'h3','@',
     &    'Sample Partial Autocorrelations of the '//ctargt(1:ntargt))
      ELSE
       IF(Iflag.le.3)THEN
        IF(Iqtype.eq.0)THEN
         CALL writTagOneLine(Mt1,'h3','@',
     &      '  Sample Autocorrelations of the '//ctargt(1:ntargt)//
     &      ' with the Ljung-Box diagnostic.')
        ELSE
         CALL writTagOneLine(Mt1,'h3','@',
     &      '  Sample Autocorrelations of the '//ctargt(1:ntargt)//
     &      ' with the Box-Pierce diagnostic.')
        END IF
       ELSE
        CALL makeSkipLink(Mt1,Idxtab,'ASCII plot of '//
     &                    'Sample Autocorrelation of the '//
     &                    ctargt(1:ntargt),T)
        CALL writTagOneLine(Mt1,'h3','@',
     &     'Sample Autocorrelations of the '//ctargt(1:ntargt))
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Ndf.ne.NOTSET)THEN
       IF(Ndf.eq.0)THEN
        IF(Nsdf.eq.0)THEN
         WRITE(Mt1,1030)
c     ------------------------------------------------------------------
        ELSE
         WRITE(Mt1,1040)Nsdf
        END IF
c     ------------------------------------------------------------------
       ELSE IF(Nsdf.eq.0)THEN
        WRITE(Mt1,1050)Ndf
c     ------------------------------------------------------------------
       ELSE
        WRITE(Mt1,1060)Ndf,Nsdf
       END IF
      END IF
c     ------------------------------------------------------------------
 1030 FORMAT(' <p> Differencing:  none </p>')
 1040 FORMAT(' <p> Differencing:  Seasonal Order=',i1,'</p>')
 1050 FORMAT(' <p> Differencing:  Nonseasonal Order=',i1,'</p>')
 1060 FORMAT(' <p> Differencing:  Nonseasonal Order=',i1,
     &       ', Seasonal Order=',i1,'</p>')
c     ------------------------------------------------------------------
      RETURN
      END
      