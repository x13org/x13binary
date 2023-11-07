C     Last change:  BCM  22 Dec 97    3:01 pm
**==ssort.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE ssort(X,Y,N,Kflag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c-----------------------------------------------------------------------
C***BEGIN PROLOGUE  SSORT
C***DATE WRITTEN   761101   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  N6A2B1
C***KEYWORDS  QUICKSORT,SINGLETON QUICKSORT,SORT,SORTING
C***AUTHOR  JONES, R. E., (SNLA)
C           WISNIEWSKI, J. A., (SNLA)
C***PURPOSE  SSORT SORTS ARRAY X AND OPTIONALLY MAKES THE SAME
C            INTERCHANGES IN ARRAY Y.  THE ARRAY X MAY BE SORTED IN
C            INCREASING ORDER OR DECREASING ORDER.  A SLIGHTLY MODIFIED
C            QUICKSORT ALGORITHM IS USED.
C***DESCRIPTION
C
C     WRITTEN BY RONDALL E. JONES
C     MODIFIED BY JOHN A. WISNIEWSKI TO USE THE SINGLETON QUICKSORT
C     ALGORITHM.  DATE 18 NOVEMBER 1976.
C
C     ABSTRACT
C         SSORT SORTS ARRAY X AND OPTIONALLY MAKES THE SAME
C         INTERCHANGES IN ARRAY Y.  THE ARRAY X MAY BE SORTED IN
C         INCREASING ORDER OR DECREASING ORDER.  A SLIGHTLY MODIFIED
C         QUICKSORT ALGORITHM IS USED.
C
C     REFERENCE
C         SINGLETON, R. C., ALGORITHM 347, AN EFFICIENT ALGORITHM FOR
C         SORTING WITH MINIMAL STORAGE, CACM,12(3),1969,185-7.
C
C     DESCRIPTION OF PARAMETERS
C         X - ARRAY OF VALUES TO BE SORTED   (USUALLY ABSCISSAS)
C         Y - ARRAY TO BE (OPTIONALLY) CARRIED ALONG
C         N - NUMBER OF VALUES IN ARRAY X TO BE SORTED
C         KFLAG - CONTROL PARAMETER
C             =2  MEANS SORT X IN INCREASING ORDER AND CARRY Y ALONG.
C             =1  MEANS SORT X IN INCREASING ORDER (IGNORING Y)
C             =-1 MEANS SORT X IN DECREASING ORDER (IGNORING Y)
C             =-2 MEANS SORT X IN DECREASING ORDER AND CARRY Y ALONG.
C***REFERENCES  SINGLETON,R.C., ALGORITHM 347, AN EFFICIENT ALGORITHM
C                 FOR SORTING WITH MINIMAL STORAGE, CACM,12(3),1969,
C                 185-7.
C***END PROLOGUE  SSORT
c-----------------------------------------------------------------------
      DOUBLE PRECISION r,tmp,tt,tty,ty,X,Y
      INTEGER i,iabs,ij,il,iu,j,k,Kflag,kk,l,m,N,nn
c-----------------------------------------------------------------------
      DIMENSION X(N),Y(N),il(21),iu(21)
C***FIRST EXECUTABLE STATEMENT  SSORT
      nn=N
      kk=iabs(Kflag)
C
C ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
C
      IF(Kflag.lt.1)THEN
       DO i=1,nn
        X(i)=-X(i)
       END DO
      END IF
      IF(kk.eq.2)THEN
C
C SORT X AND CARRY Y ALONG
C
       m=1
       i=1
       j=nn
       r=.375D0
       GO TO 70
      ELSE
C
C SORT X ONLY
C
       m=1
       i=1
       j=nn
       r=.375D0
      END IF
   10 IF(i.eq.j)GO TO 40
      IF(r.gt..5898437D0)THEN
       r=r-.21875D0
      ELSE
       r=r+3.90625D-2
      END IF
   20 k=i
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      ij=i+idint(dble(j-i)*r)
      tmp=X(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF(X(i).gt.tmp)THEN
       X(ij)=X(i)
       X(i)=tmp
       tmp=X(ij)
      END IF
      l=j
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      IF(X(j).lt.tmp)THEN
       X(ij)=X(j)
       X(j)=tmp
       tmp=X(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
       IF(X(i).gt.tmp)THEN
        X(ij)=X(i)
        X(i)=tmp
        tmp=X(ij)
       END IF
      END IF
      DO WHILE (T)
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
       l=l-1
       IF(X(l).le.tmp)THEN
        DO WHILE (T)
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
         k=k+1
         IF(X(k).ge.tmp)THEN
C                                  INTERCHANGE THESE ELEMENTS
          IF(k.le.l)THEN
           tt=X(l)
           X(l)=X(k)
           X(k)=tt
           GO TO 30
          ELSE
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
           IF(l-i.le.j-k)THEN
            il(m)=k
            iu(m)=j
            j=l
            m=m+1
           ELSE
            il(m)=i
            iu(m)=l
            i=k
            m=m+1
           END IF
           GO TO 50
          END IF
         END IF
        END DO
       END IF
   30  CONTINUE
      END DO
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
   40 m=m-1
      IF(m.eq.0)GO TO 130
      i=il(m)
      j=iu(m)
   50 IF(j-i.ge.1)GO TO 20
      IF(i.eq.1)GO TO 10
      i=i-1
      DO WHILE (T)
       i=i+1
       IF(i.eq.j)GO TO 40
       tmp=X(i+1)
       IF(X(i).gt.tmp)THEN
        k=i
        DO WHILE (T)
         X(k+1)=X(k)
         k=k-1
         IF(tmp.ge.X(k))THEN
          X(k+1)=tmp
          GO TO 60
         END IF
        END DO
       END IF
   60  CONTINUE
      END DO
   70 IF(i.eq.j)GO TO 100
      IF(r.gt..5898437D0)THEN
       r=r-.21875D0
      ELSE
       r=r+3.90625D-2
      END IF
   80 k=i
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      ij=i+idint(dble(j-i)*r)
      tmp=X(ij)
      ty=Y(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF(X(i).gt.tmp)THEN
       X(ij)=X(i)
       X(i)=tmp
       tmp=X(ij)
       Y(ij)=Y(i)
       Y(i)=ty
       ty=Y(ij)
      END IF
      l=j
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      IF(X(j).lt.tmp)THEN
       X(ij)=X(j)
       X(j)=tmp
       tmp=X(ij)
       Y(ij)=Y(j)
       Y(j)=ty
       ty=Y(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
       IF(X(i).gt.tmp)THEN
        X(ij)=X(i)
        X(i)=tmp
        tmp=X(ij)
        Y(ij)=Y(i)
        Y(i)=ty
        ty=Y(ij)
       END IF
      END IF
      DO WHILE (T)
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
       l=l-1
       IF(X(l).le.tmp)THEN
        DO WHILE (T)
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
         k=k+1
         IF(X(k).ge.tmp)THEN
C                                  INTERCHANGE THESE ELEMENTS
          IF(k.le.l)THEN
           tt=X(l)
           X(l)=X(k)
           X(k)=tt
           tty=Y(l)
           Y(l)=Y(k)
           Y(k)=tty
           GO TO 90
          ELSE
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
           IF(l-i.le.j-k)THEN
            il(m)=k
            iu(m)=j
            j=l
            m=m+1
           ELSE
            il(m)=i
            iu(m)=l
            i=k
            m=m+1
           END IF
           GO TO 110
          END IF
         END IF
        END DO
       END IF
   90  CONTINUE
      END DO
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  100 m=m-1
      IF(m.eq.0)GO TO 130
      i=il(m)
      j=iu(m)
  110 IF(j-i.ge.1)GO TO 80
      IF(i.eq.1)GO TO 70
      i=i-1
      DO WHILE (T)
       i=i+1
       IF(i.eq.j)GO TO 100
       tmp=X(i+1)
       ty=Y(i+1)
       IF(X(i).gt.tmp)THEN
        k=i
        DO WHILE (T)
         X(k+1)=X(k)
         Y(k+1)=Y(k)
         k=k-1
         IF(tmp.ge.X(k))THEN
          X(k+1)=tmp
          Y(k+1)=ty
          GO TO 120
         END IF
        END DO
       END IF
  120  CONTINUE
      END DO
C
C CLEAN UP
C
  130 IF(Kflag.ge.1)RETURN
      DO i=1,nn
       X(i)=-X(i)
      END DO
      RETURN
      END
