C     Last change:  BCM  14 May 1998    7:57 am
      SUBROUTINE invfcn(Trny,Nsrs,Fcntyp,Lam,Y)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     invfcn.f, Release 1, Subroutine Version 1.5, Modified 07 Nov 1994.
c-----------------------------------------------------------------------
c     Inverse Box-Cox Transformation, of trny, 1 to nsrs, putting the
c result in y.  Transformation is:
c     y=trny                 , lam=1
c     y=exp(trny)            , lam=0, y>0
c     trny=lam+(y**lam-1)/lam, lam<>0 or 1, y>0
c-----------------------------------------------------------------------
c Name    Type Description
c-----------------------------------------------------------------------
c i       i    Local do loop index
c lam     d    Box-Cox transformation parameter
c nsrs    i    Length of the vectors
c trny    d    Transformed vector of length nsrs
c y       d    Vector to be untransformed length nsrs
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZO,ONE
      PARAMETER(ZO=0.0D0,ONE=1.0D0)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER Fcntyp,i,Nsrs
      DOUBLE PRECISION fact,Lam,invlam,tmp,Trny(*),Y(*)
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Inverse of the logit
c-----------------------------------------------------------------------
      IF(Fcntyp.eq.3)THEN
       DO i=1,Nsrs
        tmp=exp(Trny(i))
        Y(i)=tmp/(ONE+tmp)
       END DO
c-----------------------------------------------------------------------
c     Lam=1, no transformation, just copy the vector
c-----------------------------------------------------------------------
      ELSE IF(dpeq(Lam,ONE).or.Fcntyp.eq.4)THEN
       CALL copy(Trny,Nsrs,1,Y)
c-----------------------------------------------------------------------
c     Lam=0, log transformation if y>0
c-----------------------------------------------------------------------
      ELSE IF(dpeq(Lam,ZO).or.Fcntyp.eq.1)THEN
       DO i=1,Nsrs
        Y(i)=exp(Trny(i))
       END DO
c-----------------------------------------------------------------------
c     Lam not equal to 1 or 0
c-----------------------------------------------------------------------
      ELSE
       invlam=ONE/Lam
c     ------------------------------------------------------------------
       DO i=1,Nsrs
        fact=Lam*(Trny(i)-Lam**2)+ONE
c     ------------------------------------------------------------------
        IF(fact.gt.ZO)THEN
         Y(i)=fact**invlam
c     ------------------------------------------------------------------
        ELSE
         WRITE(STDERR,1010)i,Y(i),invlam
         WRITE(Mt2,1020)i,Y(i),invlam
 1010    FORMAT(' ERROR: Cox-Box routine-y(',i5,')=',f16.8,'and 1/lam=',
     &          f5.2,'.')
 1020    FORMAT('<p><strong>ERROR:</strong> Cox-Box routine-y(',i5,')=',
     &          f16.8,'and 1/lam=',f5.2,'.</p>')
        END IF
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
