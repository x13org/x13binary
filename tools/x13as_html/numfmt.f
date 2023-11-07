C     Last change:  BCM  17 Nov 97    1:18 pm
**==numfmt.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE numfmt(Vec,Nelt,Outdec,Clwdth,Mindec)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Figures out the minimum number of columns needed to print out
c a vector given the outdec nmber of digits are needed after the decimal
c Mindec shows that every element will have at least 1 significant
c digit if mindec digits are used instead of outdec.
c-----------------------------------------------------------------------
      INTEGER Clwdth,ielt,imndec,iwdth,Mindec,Nelt,Outdec
      DOUBLE PRECISION absx,elti,lgabsx,lg9p5,Vec
      DIMENSION Vec(Nelt)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Intialize the column width, and minimum number of necessary
c decimals.
c-----------------------------------------------------------------------
      lg9p5=log10(9.5D0)
      Clwdth=0
      Mindec=0
c-----------------------------------------------------------------------
c     Max of clwdth, and mindec through the vector
c-----------------------------------------------------------------------
      DO ielt=1,Nelt
       elti=Vec(ielt)
       IF(dpeq(elti,0D0))THEN
        lgabsx=1D0
       ELSE
        lgabsx=log10(abs(elti))
       END IF
       iwdth=max(1,int(lgabsx)+1)
       IF(elti.lt.0D0)iwdth=iwdth+1
       IF(Outdec.gt.0)iwdth=iwdth+Outdec+1
       Clwdth=max(Clwdth,iwdth)
       IF(dpeq(elti,0D0))THEN
        imndec=0
c     ------------------------------------------------------------------
       ELSE
        lgabsx=lgabsx-lg9p5
c-----------------------------------------------------------------------
c     ceiling(abs(x))-1
c-----------------------------------------------------------------------
        IF(lgabsx.lt.lg9p5)THEN
         absx=abs(lgabsx)
         IF(absx.gt.dble(int(absx)))THEN
          imndec=int(absx)+1
c     ------------------------------------------------------------------
         ELSE
          imndec=int(absx)
         END IF
         imndec=imndec-1
c     ------------------------------------------------------------------
        ELSE
         imndec=0
        END IF
       END IF
       Mindec=max(Mindec,imndec)
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
