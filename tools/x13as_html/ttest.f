C     Last change:  BCM   2 Apr 98    1:02 pm
**==ttest.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      SUBROUTINE ttest(Xy,Nspobs,Ncxy,Chlxpx,Otlvar,Ltstpt,Mxcol,Propt,
     &                 Snglr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculates a value proportional to the t-statistic for
c p(AO|regression) and p(LS|regression).  The values returned are
c for the outlier type that has the largest statistic.
c propt = b/sqrt((X'X)**-1/mse))*mse which is compared to t*sqrt(mse).
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c chlxpx  d  Input ncxy(ncxy+1)/2 vector for the packed form the the
c             Cholesky decompostion of [X:y]'[X:y]
c i       i  Local do loop index
c ielt    i  Local index for the current element in a matrix
c j       i  Local do loop index
c l1      d  Local pb+1, ncxy used, long vector for part of the
c             augmented cholesky decomposition of [X:o]'[X:o] where l1
c              is [L  0   ]
c                 [l' lmd1] for the AO outlier regression
c l2      d  Local pb+1, ncxy used, long vector like l1 except for
c             the LS outlier regression
c ltstpt  l  Input 2 long logical vector to decide what tests to perform
c mxcol   i  Output type to outlier that had the largest t value
c             1=AO, 2=LS.
c nb      i  Local number of b elements and the number of columns in the
c             X matrix
c ncxy    i  Input number of columns in the X:y matrix and rows in the b
c             vector
c neltxy  i  Local number of elements in xy
c notlr   i  Local number of types of outliers to be tested notlr
c             can be 1 only be 1 for A0 only or 2 for AO and LS.
c             Also, is the number  of columns in otlvar.
c nspobs    i  Input number of rows in the X:y matrix
c nxpx    i  Local number of elements in the packed chol(X'X) matrix
c oomll1  d  Local scalar for lmd**2=o'o-l'l where 1/sqrt(o'o-l'l) is
c             se(b(AO)).  Is also the denominator for b(AO)
c oomll2  d  Local scalar same as oomll1 but for the LS b estimate and t
c             statistic
c otlvar  d  Input nspobs by notlr matrix of filtered outlier regression
c             effects
c oymlw1  d  Local scalar for o'y-l'w for the AO regression where oymlw1
c             is the numerator for b(AO) and t(AO)*sqrt(mse)
c oymlw2  d  Local scalar same as oymlw1 but for the LS b estimate and t
c             statistic
c propt  d   Output 2 long vector of the proptional t values (t*rmse),
c             where the first element is for the AO and the second
c             for the LS
c snglr   l  Output 2 long array which is true if [X:o]'[X:o] is
c             singular
c tmp1    d  Local scalar used for the AO regression
c tmp2    d  Local scalar used for the LS regression
c w       d  Input nb long vector of Lw=X'y
c xy      d  Input nspobs by ncxy matrix in vector form of regression
c             variables and data
c zero    d  Local PARAMETER for 0d0
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      LOGICAL Snglr,lgo
c      LOGICAL lflag
      INTEGER Ltstpt,i,i2,ielt,j,Mxcol,nb,Ncxy,neltxy,notlr,Nspobs,nxpx,
     &        otype
      DOUBLE PRECISION Chlxpx,l,l1,oomll,Otlvar,oymlw,Propt,tmp,
     &                 Xy,ddot,xl
      DIMENSION Chlxpx(Ncxy*(Ncxy+1)/2),l(POTLR,PB+1),l1(PB+1),
     &          Ltstpt(POTLR),Otlvar(*),Propt(POTLR),Snglr(POTLR),
     &          Xy(Nspobs*Ncxy),Mxcol(POTLR),oomll(POTLR),oymlw(POTLR),
     &          tmp(POTLR),otype(POTLR)
c-----------------------------------------------------------------------
      DOUBLE PRECISION dpmpar
      EXTERNAL dpmpar
c-----------------------------------------------------------------------
c     Find the dimensions of the regression and data matrix and outlier
c effect matrix.  ltstpt(AO) for AO's and ltstpt(LS) for LS's are both
c true then there are two outlier effects.
c-----------------------------------------------------------------------
      Snglr(AO)=F
      Snglr(LS)=F
      Snglr(TC)=F
*      Snglr(SO)=F
      nb=Ncxy-1
      neltxy=Nspobs*Ncxy
      nxpx=nb*Ncxy/2
      xl=sqrt(dpmpar(2))
c     ------------------------------------------------------------------
      notlr=0
      DO i=1,POTLR
       IF(Ltstpt(i).eq.1)THEN
        notlr=notlr+1
        otype(notlr)=i
       END IF
      END DO
c-----------------------------------------------------------------------
      CALL setdp(ZERO,notlr,oomll)
      i2=1
      DO ielt=1,notlr*Nspobs
       IF(abs(Otlvar(ielt)).gt.xl)THEN
        oomll(i2)=oomll(i2)+Otlvar(ielt)**2
       END IF
       i2=i2+1
       IF(i2.gt.notlr)i2=1
      END DO
c-----------------------------------------------------------------------
c     Form [X:y]o = [X'o]
c                   [y'o]
c Note y'o is in the last element of l1.
c-----------------------------------------------------------------------
      DO j=1,Ncxy
       CALL setdp(ZERO,notlr,tmp)
       i=1
c     ------------------------------------------------------------------
       DO ielt=j,neltxy,Ncxy
        DO i2=1,notlr
         tmp(i2)=tmp(i2)+Xy(ielt)*Otlvar(i)
         i=i+1
        END DO
       END DO
c     ------------------------------------------------------------------
       DO i2=1,notlr
        l(i2,j)=tmp(i2)
       END DO
      END DO
c-----------------------------------------------------------------------
c     Solve L*l=X'o for l and make running calculations of o'y-l'w and
c o'o-l'l.    Note w is the (ncxy-1)ncxy/2+1 to ncxy(ncxy+1)/2-1
c elements of chlxpx
c-----------------------------------------------------------------------
      DO i2=1,notlr
       oymlw(i2)=l(i2,Ncxy)
      END DO
      ielt=0
c     ------------------------------------------------------------------
      DO i=1,nb
       DO i2=1,notlr
        DO j=1,Ncxy
         l1(j)=l(i2,j)
        END DO
        tmp(i2)=l(i2,i)-ddot(i-1,Chlxpx(ielt+1),1,l1,1)
       END DO
       ielt=ielt+i
c     -----------------------------------------------------------------
       DO i2=1,notlr
        tmp(i2)=tmp(i2)/Chlxpx(ielt)
        l(i2,i)=tmp(i2)
        oymlw(i2)=oymlw(i2)-Chlxpx(nxpx+i)*tmp(i2)
        oomll(i2)=oomll(i2)-tmp(i2)**2
       END DO
c     ------------------------------------------------------------------
      END DO
c-----------------------------------------------------------------------
c     b(outlier at t0)=(o'y-l'w)/(o'o-l'l) and
c     t*sqrt(mse)=b/se(b)=[(o'y-l'w)/(o'o-l'l)]/[1/sqrt(o'o-l'l)
c and whether the AO or LS outlier has the larger absolute t statistic.
c-----------------------------------------------------------------------
      DO i2=1,notlr
       IF(oomll(i2).le.ZERO)THEN
        Snglr(otype(i2))=T
        Propt(otype(i2))=ZERO
       ELSE
        Propt(otype(i2))=oymlw(i2)/sqrt(oomll(i2))
       END IF
      END DO
c     ------------------------------------------------------------------
      Mxcol(1)=otype(1)
      IF(notlr.gt.1)THEN
       tmp(1)=propt(otype(1))
       DO i=2,notlr
        tmp(i)=propt(otype(i))
        Mxcol(i)=otype(i)
        j=i-1
        lgo=.true.
        DO WHILE (lgo.and.j.gt.0)
         IF(abs(tmp(j)).lt.abs(tmp(j+1)))THEN
          tmp(j+1)=tmp(j)
          Mxcol(j+1)=Mxcol(j)
          tmp(j)=propt(otype(i))
          Mxcol(j)=otype(i)
         ELSE
          lgo=.false.
         END IF
         j=j-1
        END DO
       END DO
      END IF
cdos  ------------------------------------------------------------------
c      CALL undfl(lflag)
cunix ------------------------------------------------------------------
      RETURN
      END
