C     Last change:  SRD  19 Nov 99    6:04 am
**==prprad.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE prprad(Adjttl,Nadjtl,Nustad,Nuspad,Priadj,Reglom)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C     Prints the Box-Cox transformation formula
c-----------------------------------------------------------------------
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      CHARACTER Adjttl*(79),ttl*(100)
      LOGICAL lusrad
      INTEGER nttl,Nadjtl,Nustad,Nuspad,Priadj,Reglom
c     ------------------------------------------------------------------
      nttl=2
      call setchr(' ',100,ttl)
      lusrad=Nustad.gt.0.or.Nuspad.gt.0
c     ------------------------------------------------------------------
      IF(lusrad)THEN
       IF(Nadjtl.eq.0)THEN
        ttl((nttl+1):(nttl+12))='User-defined'
        nttl=nttl+12
c     ------------------------------------------------------------------
       ELSE
        ttl((nttl+1):(nttl+Nadjtl))=Adjttl(1:Nadjtl)
        nttl=nttl+Nadjtl
       END IF
      END IF
c-----------------------------------------------------------------------
c     Predefined prior adjustment argument (2=lom, 3=loq, and 4=lpyear)
c-----------------------------------------------------------------------
      IF(Priadj.gt.1)THEN
       IF(lusrad)THEN
        ttl((nttl+1):(nttl+3))=' * '
        nttl=nttl+3
       END IF
c     ------------------------------------------------------------------
       IF(Priadj.eq.2)THEN
        ttl((nttl+1):(nttl+15))='Length-of-Month'
        nttl=nttl+15
c     ------------------------------------------------------------------
       ELSE IF(Priadj.eq.3)THEN
        ttl((nttl+1):(nttl+17))='Length-of-Quarter'
        nttl=nttl+17
       ELSE IF(Priadj.eq.4)THEN
        ttl((nttl+1):(nttl+9))='Leap Year'
        nttl=nttl+9
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(nttl.gt.2)THEN
       CALL mkPOneLine(Mt1,'@',
     &            '<strong>Combined Prior Adjustment Factors</strong>'//
     &                 Cbr//'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'//
     &                 ttl(1:nttl))
       IF(Reglom.eq.2)THEN
        CALL mkPOneLine(Mt1,'@','Trading Day variables adjusted too')
       ELSE IF(Reglom.eq.3)THEN
        CALL mkPOneLine(Mt1,'@','All regression variables adjusted too')
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END

