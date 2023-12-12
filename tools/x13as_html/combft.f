C     Last change:  BCM  29 Jan 98    1:14 pm
**==combft.f    processed by SPAG 4.03F  at 15:12 on  1 Aug 1994
      SUBROUTINE combft(Lprt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINUE PRODUCES THE COMBINED TEST FOR THE PRESENCE OF
C --- IDENTIFIABLE SEASONALITY.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssft.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tests.cmn'
c-----------------------------------------------------------------------
*      CHARACTER xb*50
      DOUBLE PRECISION test
*      INTEGER sp1
      LOGICAL Lprt
c-----------------------------------------------------------------------
      Iqfail=1
      Test1=9D0
      IF(Fstabl*9D0.ge.7D0)Test1=7D0/Fstabl
      IF(Fstabl.gt.0D0)Test2=(3D0*Fmove)/Fstabl
      IF(Test2.gt.9D0.or.Fstabl.le.0D0)Test2=9D0
      IF(.not.Lhiddn.and.Lprt)THEN
       IF(.not.Lcmpaq)WRITE(Mt1,'()')
       CALL writTagOneLine(Mt1,'h3','center',
     &     'COMBINED TEST FOR THE PRESENCE OF IDENTIFIABLE SEASONALITY')
      END IF
      IF(P1.lt.0.1D0)THEN
       IF(P2.le.5D0)THEN
        test=(Test1+Test2)/2D0
        IF(test.ge.1D0)GO TO 10
       END IF
       IF(Test1.lt.1D0)THEN
        IF(P5.le.0.1D0)THEN
         IF(Test2.lt.1D0)THEN
          IF(.not.Lhiddn.and.Lprt)THEN
           IF(.not.Lcmpaq)WRITE(Mt1,'()')
           CALL mkPOneLine(Mt1,'center',
     &                     'IDENTIFIABLE SEASONALITY PRESENT')
          END IF
          IF(Issap.eq.2)Issqf(Icol)=0
          RETURN
         END IF
        END IF
       END IF
       IF(.not.Lhiddn.and.Lprt)THEN
        IF(.not.Lcmpaq)WRITE(Mt1,'()')
        CALL mkPOneLine(Mt1,'center',
     &                  'IDENTIFIABLE SEASONALITY PROBABLY NOT PRESENT')
       END IF
       IF(Issap.eq.2)Issqf(Icol)=1
       RETURN
      END IF
   10 IF(.not.Lhiddn.and.Lprt)THEN
       IF(.not.Lcmpaq)WRITE(Mt1,'()')
       CALL mkPOneLine(Mt1,'center',
     &                 'IDENTIFIABLE SEASONALITY NOT PRESENT')
      END IF
      Iqfail=2
      IF(Issap.eq.2)Issqf(Icol)=2
c-----------------------------------------------------------------------
      RETURN
      END
