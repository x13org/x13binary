      LOGICAL FUNCTION mdlmch(Nrar,Nrdiff,Nrma,Nsar,Nsdiff,Nsma,Bstrar,
     &                        Bstrdf,Bstrma,Bstsar,Bstsdf,Bstsma,Bstbic)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Bstbic
      INTEGER i,Nrar,Nrdiff,Nrma,Nsar,Nsdiff,Nsma,Bstrar,Bstrdf,Bstrma,
     &        Bstsar,Bstsdf,Bstsma
      DIMENSION Bstrar(5),Bstrdf(5),Bstrma(5),Bstsar(5),Bstsdf(5),
     &          Bstsma(5),Bstbic(5)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      mdlmch=.false.
      DO i=1,5
       IF(dpeq(Bstbic(i),DNOTST))RETURN
       IF(Nrar.eq.Bstrar(i).and.Nrdiff.eq.Bstrdf(i).and.
     &    Nrma.eq.Bstrma(i).and.Nsar.eq.Bstsar(i).and.
     &    Nsdiff.eq.Bstsdf(i).and.Nsma.eq.Bstsma(i))THEN
        mdlmch=.true.
        RETURN
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
