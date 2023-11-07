C     Last change:  BCM  12 Mar 98    9:52 am
      SUBROUTINE rmpadj(Series,Sprior,Pos1,Pos2,Muladd)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Remove the permanent prior adjustment factors from the final
c     seasonally adjusted series.
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'lzero.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Series,Sprior
      INTEGER i,i2,i3,Muladd,Pos1,Pos2
      DIMENSION Series(*),Sprior(*)
c     ------------------------------------------------------------------
c     If no predetermined prior adjustments => adjust by Usrpad
c     If predetermined prior adjustments & no temporary prior 
c       adjustments => adjust by Sprior
c     If predetermined prior adjustments & temporary prior adjustments
c       => adjust by Sprior minus/divided Usrtad
c     ------------------------------------------------------------------
      DO i=Pos1,Pos2
       i2=Frstap+i-Pos1+Lsp-1
       IF(Muladd.eq.1)THEN
c     ------------------------------------------------------------------
        IF(Priadj.le.1)THEN
         Series(i)=Series(i)-Usrpad(i2)
        ELSE
         IF(Nustad.eq.0)THEN
          Series(i)=Series(i)-Sprior(i)
         ELSE
          i3=Frstat+i-Pos1+Lsp-1
          Series(i)=Series(i)-(Sprior(i)-Usrtad(i3))
         END IF
        END IF
       ELSE
        IF(Priadj.LE.1)THEN
         Series(i)=Series(i)/Usrpad(i2)
        ELSE
         IF(Nustad.eq.0)THEN
          Series(i)=Series(i)/Sprior(i)
         ELSE
          i3=Frstat+i-Pos1+Lsp-1
          Series(i)=Series(i)/(Sprior(i)/Usrtad(i3))
         END IF
        END IF
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
c     ------------------------------------------------------------------
      END
