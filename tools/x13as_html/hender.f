**==hender.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE hender(W,N)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION ONE,THREE,FOUR,NINE
      PARAMETER(NINE=9D0,THREE=3D0,FOUR=4D0,ONE=1D0)
      DOUBLE PRECISION denomi,W,x,y,y1,y2,y3,y4,y5
      INTEGER i,m,N
C*** End of declarations inserted by SPAG
C
C     THIS SUBROUTINE GENERATES THE WEIGHTS FOR AN N-TERM HENDERSON
C     MOVING AVERAGE. THE WEIGHTS ARE STORED IN W. ONLY HALF THE WEIGHTS
C     ARE GENERATED SINCE W IS SYMMETRIC.
C
      DIMENSION W(*)
      y=dble((N+3)/2)
      m=(N+1)/2
      y1=(y-ONE)*(y-ONE)
      y2=y*y
      y3=(y+ONE)*(y+ONE)
      y4=THREE*y2-16D0
      y5=FOUR*y2
      denomi=8D0*y*(y2-ONE)*(y5-ONE)*(y5-NINE)*(y5-25D0)/315D0
      DO i=1,m
       x=dble((i-1)*(i-1))
       W(i)=(y1-x)*(y2-x)*(y3-x)*(y4-11D0*x)/denomi
      END DO
      RETURN
      END
