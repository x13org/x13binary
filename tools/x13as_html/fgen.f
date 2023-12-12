C     Last change:  BCM  16 Feb 1999    3:39 pm
**==fgen.f    processed by SPAG 4.03F  at 11:18 on 14 Sep 1994
      SUBROUTINE fgen(Nw,Kfmt,Lf2,Lf3,Ldirect)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE GENERATES THE F2 AND F3 TABLES.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'mq3.cmn'
c-----------------------------------------------------------------------
      CHARACTER mqf2*(7)
      INTEGER Kfmt,khcfm,Nw,Lf2,Lf3
      LOGICAL Ldirect
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      IF(Prttab(Lf2))THEN
       IF(Ny.eq.4)THEN
        mqf2=Moqu
       ELSE
        mqf2='  month'
       END IF
       khcfm=1
       IF(Kfmt.gt.0)khcfm=2
       CALL genSkip(Lf2)
       IF (Ldirect) THEN
         CALL writTagOneLine(Nw,'h3','@','F 2. Summary Measures')
       ELSE
         CALL writTagOneLine(Nw,'h3','@','F 2. Summary Measures for Indi
     &rect Adjustment')
       END IF
       CALL prtf2(Nw,mqf2,khcfm)
      END IF
      IF(Prttab(Lf3))THEN
       CALL genSkip(Lf3)
       IF (Ldirect) THEN
         CALL writTagOneLine(Nw,'h3','@',
     &              'F 3. Monitoring and Quality Assessment Statistics')
       ELSE
         CALL writTagOneLine(Nw,'h3','@',
     &'F 3. Monitoring and Quality Assessment Statistics for Indirect
     &Adjustment')
       END IF
       CALL f3gen(Nw,Ny,Kfulsm,Lcmpaq)
      END IF
      RETURN
      END
