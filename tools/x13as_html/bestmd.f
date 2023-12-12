      SUBROUTINE bestmd(Irar,Irdf,Irma,Isar,Isdf,Isma,Bstrar,Bstrdf,
     &                  Bstrma,Bstsar,Bstsdf,Bstsma,Bstbic)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'lkhd.cmn'
c     ------------------------------------------------------------------
      INTEGER Bstrar,Bstrdf,Bstrma,Bstsar,Bstsdf,Bstsma,i,Irar,Irdf,
     &        Irma,Isar,Isdf,Isma,j
      DOUBLE PRECISION Bstbic
      DIMENSION Bstrar(5),Bstrdf(5),Bstrma(5),Bstsar(5),Bstsdf(5),
     &          Bstsma(5),Bstbic(5)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      i=1
      DO WHILE(i.le.5)
       IF(dpeq(Bstbic(i),DNOTST).or.Bstbic(i).gt.Bic2)THEN
        IF(i.lt.5.and.(.not.dpeq(Bstbic(i),DNOTST)))THEN
         DO j=4,i,-1
          IF(.not.dpeq(Bstbic(j),DNOTST))THEN
           Bstbic(j+1)=Bstbic(j)
           Bstrdf(j+1)=Bstrdf(j)
           Bstrar(j+1)=Bstrar(j)
           Bstrma(j+1)=Bstrma(j)
           Bstsdf(j+1)=Bstsdf(j)
           Bstsar(j+1)=Bstsar(j)
           Bstsma(j+1)=Bstsma(j)
          END IF
         END DO
        END IF
        Bstbic(i)=Bic2
        Bstrdf(i)=Irdf
        Bstrar(i)=Irar
        Bstrma(i)=Irma
        Bstsdf(i)=Isdf
        Bstsar(i)=Isar
        Bstsma(i)=Isma
        i=6
       ELSE
        i=i+1
       END IF
      END DO
      RETURN
      END 
