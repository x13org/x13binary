C     Last change:  BCM  13 May 1998    9:04 am
      SUBROUTINE prtamd(Cmodel,Mape,Blchi,Qchi,Dgfchi,Mdlnum,Lfcst,
     &                  Ovrdff,Ovrsdf,Fctok,Argok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Print out model information for automatic model identification
c     procedure
c-----------------------------------------------------------------------
      DOUBLE PRECISION CHILIM
      PARAMETER(CHILIM=0.005D0)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'fxreg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER Autofl*(PFILMD),Cmodel*(*)
      DOUBLE PRECISION Fctlim,Bcklim,Qlim,Ovrdif,Mape,Blchi,Qchi
      LOGICAL Lfcst,Lautox,Ovrdff,Ovrsdf,Pck1st,Outfer,Id1st,Fctok,Argok
      INTEGER Mdlnum,Dgfchi
      DIMENSION Mape(4)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      COMMON /armamx/ Fctlim,Bcklim,Qlim,Ovrdif,Lautox,Pck1st,Id1st,
     &                Outfer,Autofl
c-----------------------------------------------------------------------
      IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
      WRITE(Mt1,1010)Mdlnum,Cmodel
 1010 FORMAT(/,'<p> Model ',i3,': ',a,'</p>')
      IF(Ngrp.gt.0)
     &   CALL desreg('Regression Model',Ngrp,Grpttl,Grpptr,Ngrptl)
      IF(Ngrpfx.gt.0)
     &   CALL desreg('Regression Model (fixed)',Ngrpfx,Gfxttl,Gfxptr,
     &               Ngfxtl)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print out message when error occurs in producing backcast error,
c     return from subroutine.  BCM May 2007
c-----------------------------------------------------------------------
      IF(.not.Argok)THEN
       CALL mkPOneLine(Mt1,'@','  Estimation error in computing '//
     &                 'average backcast error for this model.')
       RETURN
      END IF
c-----------------------------------------------------------------------
c     print out average forecast error
c-----------------------------------------------------------------------
      IF(Fctok)CALL prafce(Mt1,Mape,Outfer,Lfcst)
      IF(Lfcst)THEN
       IF(.not.dpeq(Blchi,DNOTST))THEN
        IF(Blchi.gt.CHILIM)then
         WRITE(Mt1,1040)Blchi,Qchi,Dgfchi
        ELSE
         WRITE(Mt1,1041)Blchi,Qchi,Dgfchi
        END IF
       END IF
 1040  FORMAT('<p>Chi Square Probability:   ',f6.2,' %  (Q = ',f12.4,
     &        ', ',i4,' DF)</p>',/)
 1041  FORMAT('<p>Chi Square Probability:   ',e17.10,' %  (Q = ',f12.4,
     &        ', ',i4,' DF)</p>',/)
c-----------------------------------------------------------------------
c     Print out model information.  First, load estimated model
c     parameters into temporary variables, then print out model
c     coefficents for each type.
c-----------------------------------------------------------------------
       CALL setpt(Mt1,AR,'Nonseasonal AR')
       IF(.not.Lfatal)CALL setpt(Mt1,MA,'Nonseasonal MA')
       IF(.not.Lfatal)CALL setpt(Mt1,AR,'Seasonal AR')
       IF(.not.Lfatal)CALL setpt(Mt1,MA,'Seasonal MA')
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check to see if model has been rejected, and print out message
c     explaining rejection.
c-----------------------------------------------------------------------
       IF((.not.Fctok).or.(Mape(4).gt.Fctlim).or.((Blchi.lt.Qlim).or.
     &    dpeq(Blchi,DNOTST)).or.Ovrdff)THEN
c     &    Ovrdff.or.Ovrsdf)THEN
        IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
        WRITE(Mt1,1060)Mdlnum
 1060   FORMAT(/,'<p>MODEL ',i3,' REJECTED: </p>')
        IF(Fctok)THEN
         IF(Mape(4).gt.Fctlim)WRITE(Mt1,1070)Fctlim
 1070    FORMAT('<p>Average forecast error > ',f6.2,'%</p>')
        ELSE
         CALL mkPOneLine(Mt1,'@','Insufficient data to compute the '//
     &                         'average forecast error for this model.')
        END IF
        IF(dpeq(Blchi,DNOTST))THEN
         CALL mkPOneLine(Mt1,'@','Insufficient data to compute the '//
     &               'Ljung-Box chi-square probability for this model.')
        ELSE
         IF(Blchi.le.Qlim)THEN
          IF(Qlim.gt.CHILIM)THEN
           WRITE(Mt1,1080)Qlim
          ELSE
           WRITE(Mt1,1081)Qlim
          END IF
         END IF
 1080    FORMAT('<p>Ljung-Box Q chi-square probability  < ',f6.2,
     &          ' %</p>')
 1081    FORMAT('<p>Ljung-Box Q chi-square probability  < ',e17.10,
     &          ' %</p>')
        END IF
        IF(Ovrdff)WRITE(Mt1,1090)'E','nonseasonal','.'
 1090   FORMAT('<p>',a,'vidence of ',a,' overdifferencing',a,'</p>')
       END IF
      ELSE IF(Mape(4).gt.Bcklim)THEN
       IF(.not.Lcmpaq)WRITE(Mt1,'()')
       WRITE(Mt1,1100)Mdlnum,Cbr,Bcklim
 1100  FORMAT(/,'<p class="center"> MODEL ',i3,' REJECTED: ',a,/,
     &        '   Average backcast error > ',f6.2,'%</p>')
      END IF
      IF(Ovrsdf)WRITE(Mt1,1090)'<strong>WARNING:</strong> E','seasonal',
     &                         ' (see message below).'
c-----------------------------------------------------------------------
      RETURN
      END
