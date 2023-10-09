C     Last change:  BCM  13 Oct 1998    3:20 pm
      SUBROUTINE regfix()
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Test whether all the fixed parameters have values.  If there are
c     no regression variables, the default is not to fix in case any
c     automatic outliers are found.
c-----------------------------------------------------------------------
c     Also, will set up Regfx variable for each regressor based on the
c     value of Ifix (BMonsell, 1998)
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.TRUE.,F=.FALSE.)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c      INCLUDE 'x11adj.cmn'
c     ------------------------------------------------------------------
      INTEGER ieff
      LOGICAL allfix
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      Iregfx=0
      allfix=T
      IF(Nb.gt.0)THEN
       DO ieff=1,Nb
        IF(dpeq(B(ieff),DNOTST))THEN
         IF(allfix)allfix=F
        ELSE
         allfix=allfix.and.Regfx(ieff)
         IF(Iregfx.eq.0)Iregfx=1
         IF(Regfx(ieff).and.Iregfx.eq.1)Iregfx=2
        END IF
       END DO
      END IF
      IF(allfix.and.Iregfx.gt.0)Iregfx=3
c     ------------------------------------------------------------------
      RETURN
      END
