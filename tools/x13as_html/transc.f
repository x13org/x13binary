C*==transc.f    processed by SPAG 6.01Fc at 13:51 on 29 Jan 1999
C
C
      subroutine TRANS2(P,Nn,X,M,N)
C
C  PARAMETROS DE ENTRADA:
C  M = EL PRIMER INDICE DE PARAMETROS MENOS UNO
C  N = EL ULTIMO INDICE DE PARAMETROS
C
C     IMPLICIT NONE
C*--********************************************************************
CA OUTPUT - P
CA INPUT  - NN
CA INPUT  - X
CA INPUT  - M
CA INPUT  - N
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C calls       ** NOTHING **
C called by   ESFICH3
C modifies    ** NOTHING **
C uses value  ** NOTHING **
C local vars  ALPH     C        D        DELTA    DISC     E        I
C             ICOUNT   IROOT    J        S        Y
C uses PARAMs *** NONE ****
C*++********************************************************************
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer Nn
      real*8 P(Nn)
      real*8 X(Nn)
      integer M
      integer N
C
C.. Local Scalars ..
      integer I,Icount,Iroot,J
      real*8 D,Delta,Disc,E,S,Y
C
C.. Local Arrays ..
      real*8 Alph(3),C(3)
C
C.. Intrinsic Functions ..
      intrinsic ABS, SQRT
C
C*** End of declarations rewritten by SPAG
C
C ****  Start of Executable Program
C
C  TO TRANSFORM SEARCH PARAMETERS INTO MODEL  PARAMETERS
C
      J = N - M
      Iroot = J
      if (J .lt. 2) then
       C(1) = X(N)
       Alph(1) = C(1)
      elseif (J .eq. 2) then
       C(1) = X(M+1) * (1.0d0-X(N))
       C(2) = X(N)
       Disc = C(1)**2 + 4.0d0*C(2)
       if (Disc .ge. 0.0d0) then
        Disc = SQRT(Disc)
        Alph(1) = 0.5d0 * (C(1)+Disc)
        Alph(2) = 0.5d0 * (C(1)-Disc)
       else
        Iroot = 0
       end if
      else
       S = (2.0d0*X(M+1)-1.0d0) * (1.0d0-X(N))
       D = (1.0d0+X(N)) * ((1.0d0+X(M+1))*(1.0d0+X(M+2))-1.0d0)
       C(1) = 0.5d0 * (S+D)
       C(2) = 0.5d0 * (S-D)
       C(3) = X(N)
C
C  TO FIND REAL ROOTS OF X**3-C(1)*X**2-C(2)*X-C(3)=0.
C  PUT X=Y+C(1)/3. EQUATION BECOMES Y**3-D*Y-E=0
C  FIND ROOT BY NEWTON-RAPHSON
C
       D = C(1)*C(1)/3.0d0 + C(2)
       E = (2.0d0*C(1)**3+9.0d0*C(1)*C(2))/27.0d0 + C(3)
       Disc = 4.0d0*D**3 - 27.0d0*E**2
       if (Disc .gt. 0.0d0) then
        Y = -E/D
       elseif (E .gt. 0.0d0) then
        Y = 1 - C(1)/3
       else
        Y = -1 - C(1)/3
       end if
       Icount = 0
       do while (.true.)
        Delta = (Y**3-D*Y-E) / (3.0d0*Y*Y-D)
        Y = Y - Delta
        if (ABS(Delta) .le. 0.00005d0) goto 1000
        Icount = Icount + 1
        if (Icount .gt. 10) then
 7000    format (/,'  CUBIC ITERATIONS EXCEEDED')
         write (7,7000)
         goto 1000
        end if
       end do
       goto 1005
 1000  Alph(1) = Y
C
C  TEST IF ALL ROOTS ARE REAL
C
       if (Disc .ge. 0.0d0) then
C
C  ROOTS REAL.DIVIDE BY (Y-ALPH(1))
C  Y**2+ALPH(1)*Y+E/ALPH(1)=0
C
        Disc = SQRT(Alph(1)**2-4.0d0*E/Alph(1))
        Alph(2) = 0.5d0 * (-Alph(1)+Disc)
        Alph(3) = 0.5d0 * (-Alph(1)-Disc)
       else
        Iroot = 1
       end if
       do I = 1,Iroot
        Alph(I) = Alph(I) + C(1)/3.0d0
       end do
      end if
 1005 do I = 1,N-M
       P(M+I) = -C(I)
      end do
      end
C*==trans1.f    processed by SPAG 6.01Fc at 13:51 on 29 Jan 1999
C
C SUBROUTINE TRANSFORMS  PARAMETER VALUES WITHIN GROUPS
C
C PHITH = ARRAY OF MODEL PARAMETERS E.G.THETA  PARAMETERS
C   NPQ = NO OF ELEMENTS IN FITH
C    X = ARRAY OF TRANSFORMED  PARAMETERS
C XMIN = MINIMUM BOUNDS FOR X
C XMAX = MAXIMUM BOUNDS FOR X
C   IB = POSITION OF FIRST TRANSFORMED MODEL  PARAMETER WITHIN X
C   IE = POSITION OF LAST TRANSFORMED MODEL  PARAMETER WITHIN X
C
C
      subroutine TRANS0(P,Nn,X,Ib,Ie,Iprs,Ur,Xl)
C     IMPLICIT NONE
C*--********************************************************************
CA INPUT  - P
CA INPUT  - NN
CA OUTPUT - X
CA INPUT  - IB
CA INPUT  - IE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C calls       ** NOTHING **
C called by   ESFICH3
C modifies    ** NOTHING **
C uses value  /DIM/    /TRAN/   IPRS     UR       XL
C local vars  I        J        NPQ      PHITH    XMAX     XMIN
C             XTEST
C uses PARAMs *** NONE ****
C*++********************************************************************
C
C*** Start of declarations rewritten by SPAG
C
C COMMON variables
C
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer Nn
      real*8 P(Nn)
      real*8 X(Nn)
      integer Ib,Ie,Iprs
      DOUBLE PRECISION Xl,Ur
C
C.. Local Scalars ..
      integer I,J,Npq
      real*8 Xmax,Xmin,Xtest
C
C.. Local Arrays ..
      real*8 Phith(3)
C
C.. Intrinsic Functions ..
      intrinsic ABS, SIGN
C
C*** End of declarations rewritten by SPAG
C
C---
C-
C EXPLORE
C
C ****  Start of Executable Program
C
      Xmin = -XL
      Xmax = XL
      Npq = Ie - Ib + 1
      do I = 1,Npq
       Phith(I) = -P(Ib+I-1)
      end do
      if (Npq .le. 1) then
       X(Ib) = Phith(1)
      elseif (Npq .le. 2) then
       if (ABS(1.0d0-Phith(2)) .lt. 1.0d-9) then
        Phith(2) = UR
       end if
       X(Ib) = Phith(1) / (1.0d0-Phith(2))
       X(Ie) = Phith(2)
      else
       if (ABS(Phith(3)-1.0d0) .lt. 1.0d-9) then
        Phith(3) = SIGN(UR,Phith(3))
       end if
       X(Ib) = 0.5d0 * ((Phith(1)+Phith(2))/(1.0d0-Phith(3))+1.0d0)
       X(Ib+1) = (1.0d0+(Phith(1)-Phith(2))/(1.0d0+Phith(3)))
       if (ABS(X(Ib)+1.0d0) .lt. 1.0d-9) then
        X(Ib) = -UR
       end if
       X(Ib+1) = X(Ib+1)/(1.0d0+X(Ib)) - 1.0d0
       X(Ie) = Phith(3)
      end if
      do J = Ib,Ie
       Xtest = (X(J)-Xmin) / (Xmax-Xmin)
       if (Xtest .lt. 0.01d0) then
        if (J .le. IPRS) then
         X(J) = -UR
        else
         X(J) = Xmin
        end if
       end if
       if (Xtest .gt. 0.99d0) then
        if (J .le. IPRS) then
         X(J) = UR
        else
         X(J) = Xmax
        end if
       end if
      end do
C   12 CONTINUE
      end
