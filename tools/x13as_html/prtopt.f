C     Last change:  BCM   5 May 1998    4:00 pm
      SUBROUTINE prtopt(Lestim,Mxiter,Mxnlit)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     prtopt.f, Release 1, Subroutine Version 1.5, Modified 16 Feb 1995.
c-----------------------------------------------------------------------
c     Prints the nonlinear estimation options
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER ceval*(10)
      LOGICAL Lestim
      INTEGER Mxiter,Mxnlit,neval
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      CALL mkPClass(Mt1,'indent')
      IF(Lextar.and.Lextma)THEN
       WRITE(Mt1,1010)'Exact <abbr title="autoregressive moving '//
     &                'average">ARMA</abbr>',' likelihood '
      ELSE IF(Lextma)THEN
       WRITE(Mt1,1010)'Exact <abbr title="moving average">MA</abbr>,'//
     &                ' conditional <abbr title="autoregression">AR'//
     &                '</abbr>',' likelihood '
      ELSE
       WRITE(Mt1,1010)'Conditional',' likelihood '
      END IF
c-----------------------------------------------------------------------
      ceval='evaluation'
      IF(Lestim)THEN
       ceval='estimation'
       WRITE(Mt1,1010)ceval,Cbr
      ELSE IF(Iregfx.gt.0)THEN
       WRITE(Mt1,1010)ceval,Cbr
      ELSE IF(Ncxy.gt.1)THEN
       WRITE(Mt1,1010)ceval//' with <abbr title="generalized least '//
     &                'squares">GLS</abbr> regression estimates',Cbr
      ELSE
       WRITE(Mt1,1010)ceval,Cbr
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT(a,a)
c-----------------------------------------------------------------------
      IF(Lestim)THEN
       IF(Ncxy.gt.1)THEN
        WRITE(Mt1,1020)Mxiter,Cbr
 1020   FORMAT('  Max total <abbr title="autoregressive moving ',
     &         'average">ARMA</abbr> iterations     ',i8,a)
        IF(Mxnlit.gt.0)WRITE(Mt1,1030)Mxnlit,Cbr
 1030   FORMAT('  Max <abbr title="autoregressive moving ',
     &         'average">ARMA</abbr> iterations''s within an ',
     &         '<abbr title="iterative generalized least squares">IGLS',
     &         '</abbr> iteration   ',i8,a)
        IF((.not.dpeq(Nltol,Tol)).OR.
     &     (.not.dpeq(Nltol0,100D0*Nltol)))THEN
         WRITE(Mt1,1040)Tol,Cbr
 1040    FORMAT('  Convergence tolerance  ',1p,g9.2,a)
         WRITE(Mt1,1050)Nltol,'</p>'
 1050    FORMAT('  <abbr title="autoregressive moving ',
     &          'average">ARMA</abbr> convergence tolerance  ',
     &          1p,g9.2,a)
        ELSE
         WRITE(Mt1,1040)Tol,Cbr//'</p>'
        END IF
c-----------------------------------------------------------------------
       ELSE
        IF(Mxnlit.gt.0)WRITE(Mt1,1020)Mxiter,Cbr
        WRITE(Mt1,1040)Tol,'</p>'
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
