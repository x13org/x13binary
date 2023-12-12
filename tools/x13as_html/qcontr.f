C     Last change:  BCM  22 Jul 1998    9:40 am
**==qcontr.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE qcontr(Mq)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE PRINTS THE QUALITY CONTROL STATISTICS IN A
C --- SUMMARIZED FORM AT THE END OF THE PRINTOUT.
c-----------------------------------------------------------------------
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
c-----------------------------------------------------------------------
      INTEGER Mq
c-----------------------------------------------------------------------
      IF(Mq.eq.12)THEN
       CALL writTagOneLine(Ng,'h2','@',
     &                     'Log Entry for the monthly series '//
     &                     Serno(1:Nser))
      ELSE IF(Mq.eq.4)THEN
       CALL writTagOneLine(Ng,'h2','@',
     &                     'Log Entry for the quarterly series '//
     &                     Serno(1:Nser))
      ELSE
       CALL writTagOneLine(Ng,'h2','@','Log Entry for '//
     &                     Serno(1:Nser))
      END IF
      CALL writTagOneLine(Ng,'h3','@',Title)
      CALL mkPOneLine(Ng,'@','&nbsp;')
c-----------------------------------------------------------------------
      RETURN
      END
