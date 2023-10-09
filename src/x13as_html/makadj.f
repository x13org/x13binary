      SUBROUTINE makadj(Adjtmp,Muladd)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'adj.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Adjtmp(PLEN)
      INTEGER Muladd
c     ------------------------------------------------------------------
c     Copy prior adjustment factors read in by user into a temporary
c     variable.
c     ------------------------------------------------------------------
      IF(Nustad.gt.0.or.Nuspad.gt.0)THEN
       IF(Nustad.gt.0)THEN
        CALL copy(Usrtad(Frstat),Nadj,1,Adjtmp(Setpri))
        IF(Nuspad.gt.0)
     &  CALL addmul(Adjtmp(Setpri),Usrpad(Frstap),Adjtmp(Setpri),1,Nadj)
       ELSE IF(Nuspad.gt.0)THEN
        CALL copy(Usrpad(Frstap),Nadj,1,Adjtmp(Setpri))
       END IF
       IF(Muladd.ne.1.and.Adjmod.eq.0)
     &    CALL invfcn(Adjtmp(Setpri),Nadj,1,0d0,Adjtmp(Setpri))
c     ------------------------------------------------------------------
c     IF no prior adjustments, set Adjtmp to unity
c     ------------------------------------------------------------------
      ELSE IF(Adjmod.eq.2)THEN
       CALL setdp(0D0,PLEN,Adjtmp)
      ELSE
       CALL setdp(1D0,PLEN,Adjtmp)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
