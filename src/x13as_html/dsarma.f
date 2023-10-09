      SUBROUTINE dsarma(Lcmpaq)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Constructs a description of an ARIMA model
c     ( 0 1 1)12
c or
c     ( 0 2*11 [ 1 3])( 0 0 1)12
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL Lcmpaq
c-----------------------------------------------------------------------
c     Print the ARIMA part
c-----------------------------------------------------------------------
      IF(Lcmpaq)THEN
       IF(Nmdl.gt.0)THEN
        WRITE(Mt1,1010)Mdlttl(1:Nmdlcr),Mdldsn(1:Nmddcr)
       ELSE
        WRITE(Mt1,1010)Mdlttl(1:Nmdlcr),'(0 0 0)'
       END IF
      ELSE
       IF(Nmdl.gt.0)THEN
        WRITE(Mt1,1010)Mdlttl(1:Nmdlcr),
     &           Cbr//'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'//Mdldsn(1:Nmddcr)
       ELSE
        WRITE(Mt1,1010)Mdlttl(1:Nmdlcr),
     &                 Cbr//'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(0 0 0)'
       END IF
      END IF
c     ------------------------------------------------------------------
 1010 FORMAT(' <p><strong>',a,':</strong> ',a,'</p>')
c     ------------------------------------------------------------------
      RETURN
      END
