**==fvalue.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION fvalue(X,M,N)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION X
      INTEGER i,i1,j,j1,k,l,M,N
C*** End of declarations inserted by SPAG
C --- THIS FUNCTION CALCULATES F-DISTRIBUTION PROBABILITY LEVELS FOR
C ---   PR(R.V. WITH F-DISTRIBUTION WITH M AND N DEGREES OF FREEDOM > X)
      DOUBLE PRECISION w,z,p,y,d,zk
      IF(X.gt.0D0)THEN
       IF(X.le.90D0)THEN
        IF(X.le.40D0.or.N.le.150)THEN
         l=(M/2)*2-M+2
         k=(N/2)*2-N+2
         w=X*dble(M)/dble(N)
         z=1.0D0/(1.0D0+w)
         IF(l.ne.1)THEN
          IF(k.ne.1)THEN
           d=z*z
           p=w*z
          ELSE
           p=dsqrt(z)
           d=0.5D0*z*p
           p=1.0D0-p
          END IF
         ELSE IF(k.ne.1)THEN
          p=dsqrt(w*z)
          d=0.5D0*p*z/w
         ELSE
          p=dsqrt(w)
          y=0.31830988618379D0
          d=y*z/p
          p=2.0D0*y*datan(p)
         END IF
         y=2.0D0*w/z
         j1=k+2
         IF(N.ge.j1)THEN
          IF(l.ne.1)THEN
           zk=z**((N-1)/2)
           d=d*zk*dble(N)/dble(k)
           p=p*zk+w*z*(zk-1.0D0)/(z-1.0D0)
          ELSE
           DO j=j1,N,2
            d=(1.0D0+dble(l)/dble(j-2))*d*z
            p=p+d*y/dble(j-1)
           END DO
          END IF
         END IF
         y=w*z
         i1=l+2
         IF(M.ge.i1)THEN
          z=2.0D0/z
          k=N-2
          DO i=i1,M,2
           zk=dble(i+k)
           d=y*d*zk/dble(i-2)
           p=p-z*d/zk
          END DO
         END IF
         IF(p.lt.1.0D0)THEN
          IF(p.gt.0.0D0)GO TO 20
          GO TO 10
         END IF
        END IF
       END IF
       fvalue=0D0
       RETURN
      END IF
   10 fvalue=1D0
      X=0D0
      RETURN
   20 fvalue=1.0D0-p
      RETURN
      END

