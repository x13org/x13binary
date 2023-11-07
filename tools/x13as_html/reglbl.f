      SUBROUTINE reglbl(Grpstr,Ngrpcr,Rglabl,Nrglbl,Rtype)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Provide shorter group names for summary diagnostic output, and
c     group all outliers declared by the user into a group called user.
c     Return label of length Nrglbl in character variable Rglabl.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c-----------------------------------------------------------------------
      CHARACTER grpstr*(PGRPCR),Rglabl*(PGRPCR)
      INTEGER Ngrpcr,Nrglbl,Rtype
c-----------------------------------------------------------------------
c     Initialize Rglabl to blanks
c-----------------------------------------------------------------------
      CALL setchr(' ',PGRPCR,Rglabl)
c-----------------------------------------------------------------------
c     Shorten label for automatic outliers
c-----------------------------------------------------------------------
*      IF(Rtype.eq.PRGTAA.or.Rtype.eq.PRGTAL.or.Rtype.eq.PRGTAT.or.
*     &   Rtype.eq.PRGTAS)THEN
      IF(Rtype.eq.PRGTAA.or.Rtype.eq.PRGTAL.or.Rtype.eq.PRGTAT)THEN
       Nrglbl=11
       Rglabl(1:Nrglbl)='AutoOutlier'
c-----------------------------------------------------------------------
c     Group other outliers into Outlier group
c-----------------------------------------------------------------------
      ELSE IF(Rtype.eq.PRGTAO.or.Rtype.eq.PRGTLS.or.Rtype.eq.PRGTTC.or.
     &        Rtype.eq.PRGTRP.or.Rtype.eq.PRGTSO.or.Rtype.eq.PRGTTL.or.
     &        Rtype.eq.PRGTQI.or.Rtype.eq.PRGTQD.or.Rtype.eq.PRSQAO.or.
     &        Rtype.eq.PRSQLS)THEN
       Nrglbl=7
       Rglabl(1:Nrglbl)='Outlier'
      ELSE
       Nrglbl=Ngrpcr
       Rglabl(1:Nrglbl)=Grpstr(1:Ngrpcr)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      
