C     Last change: Mar. 2021- fix the program hangs issue when passed
C     certain input
C     previous change:  BCM   4 Oct 2002    2:18 pm
C ... SUBROUTINE RPQ -FINDS THE ROOTS OF A POLINOMYAL IN B-
C
C  INPUT PARAMETER
C       B : POLYNOMIAL  B(1)*X^N-1 + B(2)*X^N-2 + ..... + B(N)
C       N : DIMENSION OF B
C     REZ : REAL PART OF THE ROOTS
C     IMZ : IMAGINARY PART OF THE ROOTS
C       M : THE MODUL OF THE ROOTS
C      AR : THE ARGUMENT OF THE ROOTS
C       P : THE PERIOD OF THE ROOTS
C NOPRINT : NO PRINTOUT OF THE ROOTS
C
      subroutine RPQ(b,n,rez,imz,m,ar,p,noprint,out)
C
C.. Implicits ..
      implicit none
      logical T,F
      parameter (T = .true., F = .false.)
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
      integer n,noprint,out
      real*8 b(*),rez(*),imz(*),m(*),ar(*),p(*)
C
C.. Local Scalars ..
      integer i,ifail,j,k,n1,nroots,iroot
      real*8 pi,tol
      double precision v,w
C
C.. Local Arrays ..
      real*8 a(65)
C
C.. External Functions ..
      double precision X02AAF
      external X02AAF
C
C.. External Calls ..
      external C02AEF
C
C.. Intrinsic Functions ..
      intrinsic ABS, ACOS, SQRT
      include 'stream.i'
C   LINES OF CODE ADDED FOR X-13A-S : 1
      include 'error.cmn'
      include 'htmlout.cmn'
C   END OF CODE BLOCK
C
C.. Data Declarations ..
C
C ... Executable Statements ...
C
C
      tol = X02AAF()
      pi = 3.14159265358979d0
      nroots = n - 1
      if (n.gt.1)THEN
       rez(1) = -b(2)
      ELSE
       rez(1) = ZERO
      END IF
      imz(1) = ZERO
c     if ((n.eq.4) .and. (a(1).ne.0.0D0)) then
c        call Tartaglia(b,n,reZ,imZ)
c      else if (n .gt. 2) then
      if (n .gt. 2) then
       rez(1) = ZERO
       imz(1) = ZERO
C
       do i = 1,n
        a(i) = b(i)
       end do
C
C
       n1 = n
       ifail = 0
C
       call C02AEF(a,n1,rez,imz,tol,ifail)
C   LINES OF CODE ADDED FOR X-13A-S : 1
       IF(Lfatal)RETURN
C   END OF CODE BLOCK
       if (ifail .eq. 2) then
        call C02AEF(a,n1,rez,imz,tol,ifail)
C   LINES OF CODE ADDED FOR X-13A-S : 1
        IF(Lfatal)RETURN
C   END OF CODE BLOCK
       end if
       if ((ifail .ne. 0).and.(noprint.eq.0).and.(out.eq.0)) then
        CALL wWritln(Cbr//'IFAIL=',Nio,0,T,F)
 6000   format (2x,i2,'   C02AEF Unsuccessful')
        write (Nio,6000) ifail
       end if
       do while (.true.)
C
C  THE ROOTS ARE REORDERED LISTING FIRST THOSE ONE THAT ARE
C  COMPLEX
C
C
        k = 0
        j = 0
        do i = 1,nroots
         if (imz(i).lt.tol .and. imz(i).gt.-tol) then
          k = i
         else
          j = i
         end if
         if (j.gt.k .and. k.gt.0) goto 5000
        end do
        goto 5001
 5000   v = rez(j)
        w = imz(j)
        rez(j) = rez(k)
        imz(j) = imz(k)
        rez(k) = v
        imz(k) = w
       end do
      end if
C
C WE PUT IN M THE MODULUS OF THE ROOT AND IN AR ITS ARGUMENT
C
 5001 do i = 1,nroots
C
       m(i) = SQRT(rez(i)**2+imz(i)**2)
       if (m(i) .lt. 1.0d-8) then
        ar(i) = ZERO
       else
        ar(i) = rez(i) / m(i)
       end if
       if (ABS(ar(i)) .le. ONE) then
        ar(i) = ACOS(ar(i))
        if (imz(i) .lt. ZERO) then
         ar(i) = -ar(i)
        end if
       else
        ar(i) = ZERO
        if (rez(i) .lt. ZERO) then
         ar(i) = pi
        end if
       end if
C
C     AR(I)=DSIGN(DACOS(REZ(I)/M(I)),IMZ(I))
C
      end do
C
C WE PUT IN P THE PERIOD OF THE COMPLEX ROOT
C
      do i = 1,nroots
       if ((ABS(ar(i)).gt.1.0d-8) .and.
     $     (imz(i).gt.tol.or.imz(i).lt.-tol)) then
        p(i) = 2.d0 * pi / ar(i)
       else
        p(i) = 999.99
       end if
      end do
C
C THE ARGUMENTS ARE EXPRESSED IN DEGREES
C
      do i = 1,nroots
       ar(i) = 180.0d0 * ar(i) / pi
      end do
C
C
C PRINTING OF THE RESULTS
C
      if ((noprint .ne. 1).and.(out.eq.0)) then
       call OutRPQ(Nio,nroots,rez,imz,m,ar,p)
      end if
      end
c
c      OutRPQ:   PRINTING OF THE RESULTS OF RPQ
C  INPUT PARAMETER
C     NIO: unit file where to write
C    HTML: 1:Yes ; 0:NO
C     REZ : REAL PART OF THE ROOTS
C     IMZ : IMAGINARY PART OF THE ROOTS
C       M : THE MODUL OF THE ROOTS
C      AR : THE ARGUMENT OF THE ROOTS
C       P : THE PERIOD OF THE ROOTS
      subroutine OutRPQ(Nio,nroots,rez,imz,m,ar,p)
      implicit none
C
C.. Data Declarations ..
      character blan*4,two*4
      data blan/' -  '/
      data two/'2.0 '/
      include 'srslen.prm'
      include 'dimensions.i'
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Parameters ..
      integer nio,nroots
      real*8 ar(*),p(*),rez(*),imz(*),m(*)
C 
C.. Local Parameters
      integer iroot,i
      character per(Kp)*4
C
      do i = 1,nroots
        if (rez(i) .lt. ZERO) then
         per(i) = two
        else
         per(i) = blan
        end if
      end do
C PRINTING OF THE RESULTS OF RPQ
       iroot=0
       CALL mkTableTag(Nio,'w60','Roots')
 7001  format ('<tr><td class="head">&nbsp;</td>',
     &         5('<th scope="col">',a,'</th>'),'</tr>')
      write (Nio,7001)
     &         'REAL PART','IMAGINARY PART','MODULUS','ARGUMENT',
     &         'PERIOD (DEGREES)'
      do i = 1,nroots
       iroot=iroot+1
       if (ar(i) .ge. ZERO) then
        if (ABS(p(i)-999.99) .lt. 1.0d-12) then
 7002    format ('<tr><th scope="row">root ',i2,'</th>',
     $           4('<td>',f11.4,'</td>'),'<td>',a4,'</td></tr>')
         write (Nio,7002) iroot, rez(i), imz(i), m(i), ar(i), per(i)
        else
 7003    format ('<tr><th scope="row">root ',i2,'</th>',
     $           5('<td>',f11.4,'</td>'),'</tr>')
         write (Nio,7003) iroot, rez(i), imz(i), m(i), ar(i), p(i)
        end if
       end if
      end do
      CALL writTag(Nio,'</table>')
      end
C
C
c     Tartaglia method to obtain the exact roots of polynomials of third order P(1:4):
c      given P(4)+P(3)*X+P(2)*X**2+P(1)*X**3       where P(1)<>0
c      return the roots [Rez(1)+i*IMz(1),Rez(2)+i*Imz(2),Rez(3)+i*Imz(3)]
      subroutine Tartaglia(P,n,rez,imz)
      implicit none
      real*8 CR2,n2,n3,n4,n6,n9,n27,eps,sqrt3,ZERO
      parameter(CR2=1.259921049894873D0,sqrt3=1.732050807568877D0,
     $          n2=2.0D0,n3=3.0D0,n4=4.0D0,n6=6.0D0,
     $          n9=9.0D0,n27=27.0D0,eps=1.0D-13,ZERO=0.0d0)
c     INPUT
      real*8 P(*)
      integer n    !length of P(1:4) that must be 4 if not error
c     OUTPUT
      real*8 Rez(*),Imz(*)
c     LOCAL
      integer i
      real*8 Q,R,D,rD1,iD1,rS1,iS1,rS2,iS2,mS1,mS2,
     $       b_3a,rQ_aS,iQ_aS,rS_a,iS_a
c     EXTERNAL
      intrinsic ABS
      external SQROOTC,DivCompl,cubicRoot
c
      do i=1,3
        Rez(i)=ZERO
        Imz(i)=ZERO
      enddo
      if ((n.ne.4) .or.(P(1).eq.ZERO)) then
c         ERROR this is not a 3th order polynomial
          return
      end if
      call cubicRoot(p(4)/p(1),ZERO,Rez(1),Imz(1))
      if ((Abs(p(3)/p(1)-n3*Rez(1)*Rez(1)).lt.eps).and.
     $    (Abs(p(2)/p(1)+n3*Rez(1)).lt.eps)) then     
        Rez(2)=Rez(1)        !Triple root
        Rez(3)=Rez(1) 
        return
      end if 
      Q=-p(2)*p(2)+n3*p(1)*p(3)
      R=-n2*p(2)*p(2)*p(2)+n9*p(1)*p(2)*p(3)-n27*p(1)*p(1)*p(4)
      D=n4*Q*Q*Q+R*R
      call SQROOTC(D,ZERO,rD1,iD1)
      call CubicRoot(R+rD1,iD1,rS1,iS1)
      call CubicRoot(R-rD1,-iD1,rS2,iS2)
      mS1=rS1*rS1+iS1*iS1
      mS2=rS2*rS2+iS2*iS2
      if (mS2.gt.mS1) then
        rS1=rS2
        iS1=iS2
      end if
      b_3a=p(2)/(n3*p(1))
      call DivCompl(Q,ZERO,p(1)*rS1,p(1)*iS1,rQ_aS,iQ_aS)
      rS_a=rS1/p(1)
      iS_a=iS1/p(1)
      Rez(3)=-b_3a-(CR2*rQ_aS/n3)+rS_a/(n3*CR2)
      Imz(3)=-CR2*iQ_aS/n3+iS_a/(n3*CR2)
      if (abs(Imz(1)).lt.eps)then
         Imz(1)=ZERO
      end if
      Rez(2)=-b_3a+(rQ_aS-sqrt3*iQ_aS)/(n3*CR2*CR2)
     $       -(rS_a+SQRT3*iS_a)/(n6*CR2)
      Imz(2)=(iQ_aS+sqrt3*rQ_aS)/(n3*CR2*CR2)-(iS_a-SQRT3*rS_a)/(n6*CR2)
      if (abs(Imz(2)).lt.eps)then
         Imz(2)=ZERO
      end if
      Rez(1)=-b_3a+(rQ_aS+sqrt3*iQ_aS)/(n3*CR2*CR2)
     $       -(rS_a-SQRT3*iS_a)/(n6*CR2)
      Imz(1)=(iQ_aS-sqrt3*rQ_aS)/(n3*CR2*CR2)-(iS_a+SQRT3*rS_a)/(n6*CR2)
      if (abs(Imz(3)).lt.eps)then
         Imz(3)=ZERO
      end if
      end
C
C
      double precision function X02AAF()
C
C.. Implicits ..
      implicit none
C
C.. Local Scalars ..
      real*8 z
C
C
      data z/2.225073858507201d-14/
C
C ... Executable Statements ...
C
      X02AAF = z
c      real*8 dbl_eps
c     external dbl_eps
c     X02AAF = dbl_eps()
      end
C
C
C       THIS SUBROUTINE CALCULATES C,THE PRODUCT OF TWO POLYNOMIALS
C       YOUR PRODUCT ARRAY (OUTPUT ARGUMENT "C" ) HAVE TO BE DIFFERENT
C       FROM YOUR FIRST POLYNOMIAL (INPUT ARGUMENT "A").
C
C   I.E. CONV(A,N,B,M,A,J) UNCORRECT
C
C   I.E. CONV(A,N,B,M,B,J) CORRECT
C
C
C    INPUT PARAMETER
C       A : FIRST POLYNOMIAL (true signs)  A(1) + A(2)*B + ... +
C                                          A(MPLUS1)*B^(MPLUS1-1)
C  MPLUS1 : DIMENSION  OF A
C       B : SECOND POLYNOMIAL (true signs)      "     "      "
C  NPLUS1 : DIMENSION OF B
C       C : PRODUCT OF A * B  (true signs)      "     "      "
C  LPLUS1 : DIMENSION OF C
C
C
      subroutine CONV(a,mplus1,b,nplus1,c,lplus1)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(*)
C.. In/Out Status: Read, Not Written ..
      integer mplus1
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 b(*)
C.. In/Out Status: Read, Not Written ..
      integer nplus1
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 c(*)
C.. In/Out Status: Not Read, Overwritten ..
      integer lplus1
C
C.. Local Scalars ..
      integer i,j,jplus1,num
C
C.. Local Arrays ..
      real*8 d(500)
C
C ... Executable Statements ...
C
      jplus1 = nplus1
      lplus1 = mplus1 + jplus1 - 1
      do i = 1,jplus1
       d(i) = b(i)
      end do
      do i = 1,lplus1
       c(i) = ZERO
      end do
      do i = 1,mplus1
       do j = 1,jplus1
        num = i + j - 1
        c(num) = c(num) + a(i)*d(j)
       end do
      end do
      end
C
C
C     THIS SUBROUTINE CALCULATES C,THE PRODUCT OF A(Z)
C     AND B(Z**-1). THE OUTPUT ARGUMENT C MUST BE DIFFERENT
C     FROM A,B THE TWO INPUT ARGUMENTS
C
C      INPUT  PARAMETER
C       A : FIRST POLYNOMIAL (true signs) A(1) + A(2)*COS(W) + ... +
C                                         A(MPLUS1)*COS((MPLUS1-1)*W)
C  MPLUS1 : DIMENSION  OF A
C       B : SECOND POLYNOMIAL (true signs) "    "     "       "
C  NPLUS1 : DIMENSION OF B
C       C : PRODUCT OF A * B (true signs)  "    "     "       "
C  LPLUS1 : DIMENSION OF C
C
C
C
      subroutine CONJ(a,mplus1,b,nplus1,c,lplus1)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(*)
C.. In/Out Status: Read, Not Written ..
      integer mplus1
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 b(*)
C.. In/Out Status: Read, Not Written ..
      integer nplus1
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 c(*)
C.. In/Out Status: Not Read, Overwritten ..
      integer lplus1
C
C.. Local Scalars ..
      integer i,j,k,num
C
C.. Intrinsic Functions ..
      intrinsic ABS, MAX
C
C ... Executable Statements ...
C
      lplus1 = MAX(mplus1,nplus1)
      do i = 1,lplus1
       c(i) = ZERO
      end do
      do i = 1,mplus1
       do j = 1,nplus1
        k = i - j
        num = ABS(k) + 1
        c(num) = c(num) + a(i)*b(j)
       end do
      end do
      end
C
C
C     THIS SUBROUTINE CALCULATES C,THE PRODUCT OF TWO HARMONIC
C     FUNCTIONS A * B.
C     THE OUTPUT ARGUMENT C MUST BE DIFFERENT
C     FROM A,B THE TWO INPUT ARGUMENTS
C
C      INPUT  PARAMETER
C       A : FIRST POLYNOMIAL    (true signs)
C  MPLUS1 : DIMENSION  OF A
C       B : SECOND POLYNOMIAL   (true signs)
C  NPLUS1 : DIMENSION OF B
C       C : PRODUCT OF A * B
C  LPLUS1 : DIMENSION OF C
C
C
      subroutine MULTFN(a,mplus1,b,nplus1,c,lplus1)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(*)
C.. In/Out Status: Read, Not Written ..
      integer mplus1
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 b(*)
C.. In/Out Status: Read, Not Written ..
      integer nplus1
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 c(*)
C.. In/Out Status: Not Read, Overwritten ..
      integer lplus1
C
C.. Local Scalars ..
      integer i,l1,l2
C
C.. Local Arrays ..
      real*8 c1(165),c2(165)
C
C.. External Calls ..
      external CONJ, CONV
C
C ... Executable Statements ...
C
      lplus1 = mplus1 + nplus1 - 1
      do i = 1,lplus1
       c2(i) = ZERO
      end do
      call CONV(a,mplus1,b,nplus1,c1,l1)
*      write(*,*)' MULTFN :   mplus1 = ',mplus1,' nplus1 = ',nplus1,
*     &          ' l1 = ',l1
      call CONJ(a,mplus1,b,nplus1,c2,l2)
      do i = 1,l1
       c(i) = (c1(i)+c2(i)) / 2
C     IF (DABS(C(I)) .LE. 1.0D-10) C(I) = ZERO
      end do
      end
C
C
C      THIS SUBROUTINE CALCULATES THE QUOTIENT,Q,AND REMAINDER,R,OF
C      TWO HARMONIC FUNCTIONS A $ B.
C      THE TWO OUTPUT ARGUMENT Q,R MUST BE DIFFERENT
C      FROM A,B THE TWO INPUT ARGUMENTS
C
C      INPUT  PARAMETER
C       A : FIRST POLYNOMIAL  (true signs)
C  MPLUS1 : DIMENSION  OF A
C       B : SECOND POLYNOMIAL (true signs)
C  NPLUS1 : DIMENSION OF B
C       Q : QUOTIENT          (true signs)
C      NQ : DIMENSION OF Q
C       R : REMAINDER         (true signs)
C      NR : DIMENSION OF R
C
C
      subroutine DIVFCN(a,mplus1,b,nplus1,q,nq,r,nr)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer mplus1,nplus1,nq,nr
      real*8 a(*),b(*),q(*),r(*)
C
C.. Local Scalars ..
      integer i,j,jp,k,kprime,num
      real*8 factor
C
C.. Local Arrays ..
      real*8 wr(100)
C
C.. Intrinsic Functions ..
      intrinsic ABS
C
C ... Executable Statements ...
C
      nq = mplus1 - nplus1 + 1
      do i = 1,mplus1
       wr(i) = a(i)
       if (i .le. nq) then
        q(i) = ZERO
       end if
      end do
      if (nq .ge. 1) then
       if (nplus1 .eq. 1) then
        do i = 1,mplus1
         q(i) = a(i) / b(1)
        end do
        r(1) = ZERO
        nr = 0
        return
       else
        do kprime = 1,nq
         k = nq - kprime
         num = mplus1 - kprime + 1
C       IF (B(NPLUS1).LT.1.0D-12)B(NPLUS1)=1.0D-6
         factor = wr(num) / b(nplus1)
         q(k+1) = 2.0d0 * factor
         if (k .eq. 0) then
          q(k+1) = factor
         end if
         do i = 1,nplus1
          j = i + k
          jp = ABS(i-k-1) + 1
          wr(j) = wr(j) - factor*b(i)
          if (k .ne. 0) then
           wr(jp) = wr(jp) - factor*b(i)
          end if
         end do
        end do
       end if
      else
       nq = 0
      end if
      nr = nplus1 - 1
      do i = 1,nr
       r(i) = wr(i)
      end do
      end
C
C       THIS SUBPROGRAM DEFINES THE FUNCTION TO BE MINIMISED
C       IN MINIM. THE FUNCTIONS ARE SPECIFIED IN THE COMMON
C       "FUNC", "FUNC2", "FUNC3", "FUNC4". THE COMMON "TEST"
C       SPECIFIES WHICH FUNCTION MUST BE EVALUATED.
C              IFUNC = 1 'FUNC'
C              IFUNC = 2 'FUNC2'
C              IFUNC = 3 'FUNC3'
C              IFUNC = 4 'FUNC4'
C              IFUNC = 5 'FUNC5'
C
C     INPUT ARGUMENT
C
C      X : THE VALUE IN WHICH THE FUNCTION MUST BE EVALUATED
C
      double precision function FUNC0(x)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x
C
C.. Local Scalars ..
      integer i,l
      real*8 denom,numer,w
C
C.. Local Arrays ..
      real*8 c(32)
C
C.. Intrinsic Functions ..
      intrinsic ABS, COS, MAX, SIGN
      include 'func.i'
      include 'func2.i'
      include 'func3.i'
      include 'func4.i'
      include 'func5.i'
      include 'test.i'
C
C ... Executable Statements ...
C
      w = ZERO
      numer = ZERO
      denom = ZERO
      if (Ifunc .eq. 5) then
       l = MAX(Ndum,Ndum1)
       do i = 1,l
        c(i) = COS(w)
        w = w + x
       end do
       do i = 1,Ndum
        numer = numer + Dum(i)*c(i)
       end do
       do i = 1,Ndum1
        denom = denom + Dum1(i)*c(i)
       end do
       if (ABS(denom) .lt. 1.0d-13) then
        denom = SIGN(1.0d-13,denom)
       end if
       FUNC0 = numer / denom
      else if (Ifunc .ne. 1) then
       if (Ifunc .eq. 3) then
        l = MAX(Nuc,Nc)
        do i = 1,l
         c(i) = COS(w)
         w = w + x
        end do
        do i = 1,Nuc
         numer = numer + Uc(i)*c(i)
        end do
        do i = 1,Nc
         denom = denom + Fc(i)*c(i)
        end do
        if (ABS(denom) .lt. 1.0d-13) then
         denom = SIGN(1.0d-13,denom)
        end if
        FUNC0 = numer / denom
       else if (Ifunc .eq. 4) then
        l = MAX(Nf,Nh)
        do i = 1,l
         c(i) = COS(w)
         w = w + x
        end do
        do i = 1,Nf
         numer = numer + Ff(i)*c(i)
        end do
        do i = 1,Nh
         denom = denom + Fh(i)*c(i)
        end do
        if (ABS(denom) .lt. 1.0d-13) then
         denom = SIGN(1.0d-13,denom)
        end if
        FUNC0 = numer / denom
       else
        l = MAX(Nut,Nt)
        do i = 1,l
         c(i) = COS(w)
         w = w + x
        end do
        do i = 1,Nut
         numer = numer + Ut(i)*c(i)
        end do
        do i = 1,Nt
         denom = denom + Ft(i)*c(i)
        end do
        if (ABS(denom) .lt. 1.0d-13) then
         denom = SIGN(1.0d-13,denom)
        end if
        FUNC0 = numer / denom
       end if
      else
       l = MAX(Nv,Ns)
       do i = 1,l
        c(i) = COS(w)
        w = w + x
       end do
       do i = 1,Nv
        numer = numer + V(i)*c(i)
       end do
       do i = 1,Ns
        denom = denom + Fs(i)*c(i)
       end do
       if (ABS(denom) .lt. 1.0d-13) then
        denom = SIGN(1.0d-13,denom)
       end if
       FUNC0 = numer / denom
      end if
      end
C
C MINIMISATION OF A FUNCTION IN ONE DIMENSION
C
C     INPUT PARAMETERS ARE SUPPLIED BY THE CALLING PROGRAM
C     VIA THE COMMON BLOCK "MINIM". THEY ARE:
C          START : THE STARTING VALUE FOR THE SEARCH
C          STEP  : THE STEP LENGTH USED IN MOVING
C          STOP  : THE CONVERGENCE PARAMETER
C
C ALSO REQUIRED IS THE FUNCTION SUBPROGRAM WHICH DEFINES THE FUNCTION
C F(X) TO BE MINIMISED
C
C    INPUT PARAMETER :
C    FMIN : THE MINIMUM OF THE FUNCTION
C    XMIN : THE POINT AT WHICH IT OCCURS
C   ICONV : 1 IF THE PROCESS HAS NOT CONVERGED, 0 OTHERWISE
C
C
C Changed (by Donald Martin, 7/23/02) to allow for the lower and upper
c bound of the space being minimized to be specified.
      subroutine MINIM(fmin,xmin,lb,ub,iconv)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Not Read, Overwritten ..
      real*8 fmin
C.. In/Out Status: Not Read, Overwritten ..
      real*8 xmin
C.. In/Out Status: Not Read, Overwritten ..
      integer iconv
C
C.. Local Scalars ..
      integer icount,ilim
      real*8 d,f1,f2,f3,f4,h1,h3,x1,x2,x3,x4,lb,ub
C
C.. External Functions ..
      real*8 FUNC0
      external FUNC0
C
C.. Intrinsic Functions ..
      intrinsic ABS
      include 'min.i'
C
C ... Executable Statements ...
C
C
C STEP 1: SET UP STARTING VALUES
C
      iconv = 0
      icount = 0
C
C ICONV BECOMES 1 IF THE PROCESS DOES NOT CONVERGE
C ICOUNT COUNTS THE NUMBER OF TIMES F4 IS CALCULATED
C
      d = Step
C
      x2 = Start
      x1 = Start - d
      x3 = Start + d
C Changed (by Donald Martin, 7/23/02) change from 0 to lb.
      x4 = lb
      f1 = FUNC0(x1)
      f2 = FUNC0(x2)
      f3 = FUNC0(x3)
C
C
C STEP 2: CALCULATE THE SLOPES
C
C CHECK THE NUMBER OF CALCULATIONS OF F4
C
C Changed (by Donald Martin, 7/15/02) the number of iterations allowed 
C from 20 to 50
c Changed by Brian Monsell - change number of iterations only when new
c models used 
c Changed by Brian Monsell - change number of iterations to 50
c as per Build 525 of SEATS
      ilim = 50
      do 10 while (icount .le. ilim)
C
C Here, ABS(x3-x4) was changed to ABS(x3-x2) in the expression after .or.
C (Changed by Donald Martin, July 15, 2002)
       if ((ABS(x1-x2).lt.1.d-12) .or. (ABS(x3-x2).lt.1.d-12)) goto 5003
       h1 = (f2-f1) / (x2-x1)
       h3 = (f3-f2) / (x3-x2)
C
C WE TEST THE SLOPES
C IF H3=<H1 THE SHAPE OF THE CURVE IS SUCH THAT THERE WILL NOT BE A
C MINIMUM IN THIS AREA, $ WE GO TO STEP 4
C
       if (h3 .le. h1) then
C
C STEP 4: NO MINIMUM IN THIS AREA
C
C Changed (by Donald Martin, 7/23/02) change from pi to ub, 0 to lb.
        if (x3 .le. ub) then
         if (x1 .gt. lb) then
          if (f1 .lt. f3) goto 5001
         end if
C
         x4 = x3 + 25.*d
C Changed (by Donald Martin, 7/23/02) change from pi to ub.
         if (x4 .gt. ub) then
          x4 = .5 * (x3+ub)
         end if
         do while (.true.)
C
          f4 = FUNC0(x4)
          icount = icount + 1
C
          if (f4 .le. f2) goto 5000
          x4 = 0.5 * (x4+x3)
          if (icount .gt. 50) goto 5003
         end do
 5000    x1 = x2
         f1 = f2
         x2 = x3
         f2 = f3
         x3 = x4
         f3 = f4
         goto 10
        end if
C
 5001   x4 = x1 - 25.*d
C
C TEST X4
C
C Changed (by Donald Martin, 7/23/02) change from 0 to lb.
        if (x4 .lt. lb) then
         x4 = .5 * (x1 + lb)
        end if
        do while (.true.)
C
         f4 = FUNC0(x4)
         icount = icount + 1
C
         if (f4 .le. f2) goto 5002
         x4 = 0.5 * (x4+x1)
         if (icount .gt. 50) goto 5003
        end do
 5002   x3 = x2
        f3 = f2
        x2 = x1
        f2 = f1
        x1 = x4
        f1 = f4
       else
C
C WE ARE IN A LIKELY AREA SO WE INTERPOLATE
C
C STEP 3: INTERPOLATION
C
        x4 = x2 - (h3*(x2-x1)+h1*(x3-x2))/(h3-h1)/2.
C
C IF X4 IS WELL AWAY FROM X1 OR X3, WE DO A LONG EXTRAPOLATION,
C IE GO TO STEP 5
C
        if (x4 .lt. (x1-10.*d)) then
C
C STEP 5: LONG EXTRAPOLATION
C
C Changed (by Donald Martin, 7/23/02) change from 0 to lb.
         if (x4 .lt. lb) then
          x4 = 0.5 * (x1 + lb)
         end if
         f4 = FUNC0(x4)
         icount = icount + 1
C
         x3 = x1
         f3 = f1
C
C IF F4 IS BETWEEN F1 $ F3, INTERPOLATE X2
C
         if (f4 .ge. f1) then
C
          x1 = x4
          f1 = f4
         else
C
          x2 = x4
          f2 = f4
          x1 = x2 - 10.*d
C
C
          f1 = FUNC0(x1)
          goto 10
         end if
        else if (x4 .gt. (x3+10.*d)) then
C
C Changed (by Donald Martin, 7/23/02) change from pi to ub.
         if (x4 .gt. ub) then
          x4 = 0.5 * (x3+ub)
         end if
         f4 = FUNC0(x4)
         icount = icount + 1
C
         x1 = x3
         f1 = f3
C
C IF F4 IS BETWEEN F1 $ F3, INTERPOLATE X2
C
         if (f4 .ge. f3) then
C
          x3 = x4
          f3 = f4
         else
C
          x2 = x4
          f2 = f4
          x3 = x2 + 10.*d
C
C
          f3 = FUNC0(x3)
          goto 10
         end if
        else
C Changed (by Donald Martin, 7/23/02) change from pi to ub.
         if (ABS(x4-ub) .lt. 0.0001d0) then
          x4 = ub
         end if
         f4 = FUNC0(x4)
         icount = icount + 1
C
C TEST FOR CONVERGENCE
C
         if (ABS(x4-x2) .lt.Dstop) goto 5004
         if ((ABS(x4-x1).lt.Dstop).or.(ABS(x4-x3).lt.Dstop)) goto 5004
C
C WE NOW REARRANGE THE POINTS SO THAT X1,X2 $ X3 ARE IN ASCENDING ORDER
C
         if (x4 .ge. x2) then
C
          x1 = x2
          f1 = f2
          if (x4 .gt. x3) then
           x2 = x3
           f2 = f3
           x3 = x4
           f3 = f4
           goto 10
          end if
         else
C
          x3 = x2
          f3 = f2
          if (x4 .lt. x1) then
           x2 = x1
           f2 = f1
           x1 = x4
           f1 = f4
           goto 10
          end if
         end if
C
         x2 = x4
         f2 = f4
         goto 10
        end if
C
        x2 = .5 * (x1+x3)
C
C
        f2 = FUNC0(x2)
       end if
 10   continue
C
C STEP 6: TOO MANY ITERATIONS
C
 5003 iconv = 1
C
C STEP 7: FINAL VALUES
C
 5004 xmin = x4
C
      fmin = f4
      end
cc
c
cc
C
C MINIMISATION OF A FUNCTION IN ONE DIMENSION
C
C     INPUT PARAMETERS ARE SUPPLIED BY THE CALLING PROGRAM
C     VIA THE COMMON BLOCK "MINIM". THEY ARE:
C          START : THE STARTING VALUE FOR THE SEARCH
C          STEP  : THE STEP LENGTH USED IN MOVING
C          STOP  : THE CONVERGENCE PARAMETER
C
C ALSO REQUIRED IS THE FUNCTION SUBPROGRAM WHICH DEFINES THE FUNCTION
C FBIS(X) TO BE MINIMISED
C
C    OUTPUT PARAMETER :
C    FMIN : THE MINIMUM OF THE FUNCTION
C    XMIN : THE POINT AT WHICH IT OCCURS
C   ICONV : 1 IF THE PROCESS HAS NOT CONVERGED, 0 OTHERWISE
C
C
      subroutine MINIMbis(fmin,xmin,lb,ub,iconv,haydif,mq,tol)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Not Read, Overwritten ..
      real*8 fmin
C.. In/Out Status: Not Read, Overwritten ..
      real*8 xmin
      real*8 lb,ub
C.. In/Out Status: Not Read, Overwritten ..
      integer iconv
C    
      integer haydif,mq,tol
      real*8  hs
C.. Local Scalars ..
      integer icount
      real*8 d,f1,f2,f3,f4,h1,h3,pi,x1,x2,x3,x4
C
C.. External Functions ..
      double precision Fbis
      external Fbis
C
C.. Intrinsic Functions ..
      intrinsic ABS
      include 'min.i'
C
C ... Executable Statements ...
C
C
C STEP 1: SET UP STARTING VALUES
C
      pi = 3.14159265358979d0
      iconv = 0
      icount = 0
C
C ICONV BECOMES 1 IF THE PROCESS DOES NOT CONVERGE
C ICOUNT COUNTS THE NUMBER OF TIMES F4 IS CALCULATED
C
      d = Step
C
      x2 = Start
      x1 = Start - d
      x3 = Start + d
      x4 = lb
      f1 = Fbis(x1,haydif,mq,tol)
      f2 = Fbis(x2,haydif,mq,tol)
      f3 = Fbis(x3,haydif,mq,tol)
C
C
C STEP 2: CALCULATE THE SLOPES
C
C CHECK THE NUMBER OF CALCULATIONS OF F4
C
      do 10 while (icount .le. 500)
C
       if ((ABS(x1-x2).lt.1.d-12) .or. (ABS(x3-x2).lt.1.d-12)) goto 5003
       h1 = (f2-f1) / (x2-x1)
       h3 = (f3-f2) / (x3-x2)
C
C WE TEST THE SLOPES
C IF H3=<H1 THE SHAPE OF THE CURVE IS SUCH THAT THERE WILL NOT BE A
C MINIMUM IN THIS AREA, $ WE GO TO STEP 4
C
       if (h3 .le. h1) then
C
C STEP 4: NO MINIMUM IN THIS AREA
C
        if (x3 .le. ub) then
         if (x1 .gt. lb) then
          if (f1 .lt. f3) goto 5001
         end if
C
         x4 = x3 + 25.*d
         if (x4 .gt. ub) then
          x4 = .5 * (x3+ub)
         end if
         do while (.true.)
C
          f4 = Fbis(x4,haydif,mq,tol)
          icount = icount + 1
C
          if (f4 .le. f2) goto 5000
          x4 = 0.5 * (x4+x3)
          if (icount .gt. 50) goto 5003
         end do
 5000    x1 = x2
         f1 = f2
         x2 = x3
         f2 = f3
         x3 = x4
         f3 = f4
         goto 10
        end if
C
 5001   x4 = x1 - 25.*d
C
C TEST X4
C
        if (x4 .lt. lb) then
         x4 = .5 * (x1+lb)
        end if
        do while (.true.)
C
         f4 = Fbis(x4,haydif,mq,tol)
         icount = icount + 1
C
         if (f4 .le. f2) goto 5002
         x4 = 0.5 * (x4+x1)
         if (icount .gt. 50) goto 5003
        end do
 5002   x3 = x2
        f3 = f2
        x2 = x1
        f2 = f1
        x1 = x4
        f1 = f4
       else
C
C WE ARE IN A LIKELY AREA SO WE INTERPOLATE
C
C STEP 3: INTERPOLATION
C
        x4 = x2 - (h3*(x2-x1)+h1*(x3-x2))/(h3-h1)/2.
C
C IF X4 IS WELL AWAY FROM X1 OR X3, WE DO A LONG EXTRAPOLATION,
C IE GO TO STEP 5
C
        if (x4 .lt. (x1-10.*d)) then
C
C STEP 5: LONG EXTRAPOLATION
C
         if (x4 .lt. lb) then
          x4 = 0.5 * (x1+lb)
         end if
         f4 = Fbis(x4,haydif,mq,tol)
         icount = icount + 1
C
         x3 = x1
         f3 = f1
C
C IF F4 IS BETWEEN F1 $ F3, INTERPOLATE X2
C
         if (f4 .ge. f1) then
C
          x1 = x4
          f1 = f4
         else
C
          x2 = x4
          f2 = f4
          x1 = x2 - 10.*d
C
C
          f1 = Fbis(x1,haydif,mq,tol)
          goto 10
         end if
        else if (x4 .gt. (x3+10.*d)) then
C
         if (x4 .gt. ub) then
          x4 = 0.5 * (x3+ub)
         end if
         f4 = Fbis(x4,haydif,mq,tol)
         icount = icount + 1
C
         x1 = x3
         f1 = f3
C
C IF F4 IS BETWEEN F1 $ F3, INTERPOLATE X2
C
         if (f4 .ge. f3) then
C
          x3 = x4
          f3 = f4
         else
C
          x2 = x4
          f2 = f4
          x3 = x2 + 10.*d
C
C
          f3 = Fbis(x3,haydif,mq,tol)
          goto 10
         end if
        else
         if (ABS(x4-ub) .lt. 0.0001d0) then
          x4 = ub
         end if
         f4 = Fbis(x4,haydif,mq,tol)
         icount = icount + 1
C
C TEST FOR CONVERGENCE
C
         if (ABS(x4-x2) .lt.Dstop) goto 5004
         if ((ABS(x4-x1).lt.Dstop) .or. (ABS(x4-x3).lt.Dstop)) goto 5004
C
C WE NOW REARRANGE THE POINTS SO THAT X1,X2 $ X3 ARE IN ASCENDING ORDER
C
         if (x4 .ge. x2) then
C
          x1 = x2
          f1 = f2
          if (x4 .gt. x3) then
           x2 = x3
           f2 = f3
           x3 = x4
           f3 = f4
           goto 10
          end if
         else
C
          x3 = x2
          f3 = f2
          if (x4 .lt. x1) then
           x2 = x1
           f2 = f1
           x1 = x4
           f1 = f4
           goto 10
          end if
         end if
C
         x2 = x4
         f2 = f4
         goto 10
        end if
C
        x2 = .5 * (x1+x3)
C
C
        f2 = Fbis(x2,haydif,mq,tol)
       end if
 10   continue
C
C STEP 6: TOO MANY ITERATIONS
C
 5003 iconv = 1
C
C STEP 7: FINAL VALUES
C
 5004 xmin = x4
C
      fmin = f4
      end





      subroutine minimGrid(fmin,xmin,mq,epsphi,wcomp)

C This subroutine will computes minimum by searching over a grid
C INPUTS:mq,epsphi
C        wcomp   1 minimize the seasonal spectrum 
C                  (we don't evaluate the function arround seasonal frequencies (-+Epsphi)
C                2 minimize the trend-cycle spectrum
C                  (we don't evaluate arround 0 (+Epsphi)
C                3 minimize the transitory spectrum  
C                                                 )
C OUTPUTS: xmin,fmin global minimum  of our function F (the spectrum)
C Written by Roberto Lopez, January 2006
C -------------------------------------------------------------------
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      double precision fmin,xmin,freq,epsilon
      integer mq,epsphi,wcomp
C

C
C.. Local 
      double precision fx, pi,x,inf,sup,paso
      integer i
C
C.. External Functions ..
      double precision FUNC0
      external FUNC0
      



C ... Executable Statements ...
C
      pi = 3.14159265358979d0
      epsilon = 2.0d0*pi*dble(epsphi)/360.0d0 
      paso=pi/100000.0d0
         
      if (wcomp .eq. 1) then 

c     we don't evaluate the function arround seasonal frequencies

         xmin= ZERO 
         fmin= FUNC0(xmin)
      
         INF=ZERO
         SUP= 2.0d0*pi/mq- epsilon
         x = paso
         do while (x.le.sup)
c        do x=paso,sup,paso
            fx=FUNC0(x)
            if (fmin .gt. fx) then
               fmin=fx
                 xmin=x
            end if 
            x=x+paso 
         end do
               
         do i=1,mq/2-1
            INF= SUP + 2*epsilon
            SUP=INF+ 2*pi/mq - 2*epsilon
            x=inf
            do while (x.le.sup)
c            do x=inf,sup,paso
              fx=FUNC0(x)
              if (fmin .gt. fx) then
                 fmin=fx
                   xmin=x
              end if 
              x=x+paso
            end do 
         end do
      
      else if (wcomp.eq.2)   then     
c                             trend-cycle
            
         xmin= ZERO+epsilon 
         fmin= FUNC0(xmin) 
         inf= xmin+paso        
         x=inf
         do while (x.le.pi)        
c         do x=inf,pi,paso
            fx=FUNC0(x)
            if (fmin .gt. fx) then
               fmin=fx
                 xmin=x
            end if 
            x=x+paso
         end do
      else 
c                             transitory           
         xmin= ZERO 
         fmin= FUNC0(xmin)
         fmin=FUNC0(ZERO )
        
         x=paso
         do while (x.le.pi)       
c        do x=paso,pi,paso
            fx=FUNC0(x)
            if (fmin .gt. fx) then
               fmin=fx
                 xmin=x
            end if 
            x=x+paso
         end do
      end if   
   
      return
      end

C
C     Esta funcion elimina la discontinuidad de la tendecia en 0 (f-->infinito).
C     Para ello sustituye el valor que obtendriamos llamando a F(w) por F(epsilon) en 
C     [0,epsilon)
C     Parametros de entrada: 
C      w         frecuencia en la cual evaluamos el espectro
C      haydif    0 si no se ha diferenciado
C      mq        #observaciones por año
C      tol       epsilon en grados      
C        
C      ifunc     componente para el cual queremos calcular su espectro

      double precision function Fbis(w,haydif,mq,tol)

C.. Implicits ..
      implicit none
C
C.. Parametros de la funcion
C     Frecuencias (gradianes)      
      real*8 w
      integer haydif,mq,tol

C.. External Functions ..
      real*8 FUNC0
      external FUNC0
C...Commons
      include 'test.i'  

C     Locales
      real*8 epsilon
      
      REAL*8 pi 
      parameter (pi = 3.14159265358979d0)

C     tol está en grados, lo pasamos a radianes  
      epsilon = 2.0d0*pi*dble(tol)/360.0d0 
      
      
      if (IFUNC.eq. 2) then
         if ((w.lt.epsilon) .and.(haydif .ne.0)) then
            FBIS=FUNC0(epsilon)
         else
            FBIS=FUNC0(w)
         end if
      else
          FBIS=FUNC0(w)
      end if
      end
              
            


C     (January 2006-Domingo Perez)
c     GlobalMinim search the minimum of all local minimum that minim searchs
C     INPUT PARAMETERS ARE SUPPLIED BY THE CALLING PROGRAM
C     VIA THE COMMON BLOCK "MINIM". THEY ARE:
C          STEP  : THE STEP LENGTH USED IN MOVING
C          STOP  : THE CONVERGENCE PARAMETER
c     INPUT PARAMETERS
c          lb,ub: lower and upper bound to search the minimum
c          n_step: we try with n_step+1 different Starting value for the search
C
C ALSO REQUIRED IS THE FUNCTION SUBPROGRAM WHICH DEFINES THE FUNCTION
C F(X) TO BE MINIMISED
C
C    OUTPUT PARAMETER :
C    FMIN : THE MINIMUM OF THE FUNCTION
C    XMIN : THE POINT AT WHICH IT OCCURS
C   ICONV : 1 IF THE PROCESS HAS NOT CONVERGED, 0 OTHERWISE

C
C ALSO REQUIRED IS THE FUNCTION SUBPROGRAM WHICH DEFINES THE FUNCTION
C F(X) TO BE MINIMISED
C
C    OUTPUT PARAMETER :
C    FMIN : THE MINIMUM OF THE FUNCTION
C    XMIN : THE POINT AT WHICH IT OCCURS
C   ICONV : 1 IF THE PROCESS HAS NOT CONVERGED, 0 OTHERWISE
c
      subroutine globalMinim(fmin,xmin,lb,ub,iconv,n_step,haydif,
     &           mq,tol)
      implicit none
      real*8 fmin,xmin,lb,ub,fmintmp,xmintmp,hs
      integer iconv,n_step,haydif,mq,tol
      real*8 x,e_step
      include 'min.i'

      intrinsic dble
      fmin=10.0d20
c     !that is a bigger number that the one the function can have
      xmin=10.0d20
c       ! a bigger index to indicate no minimum
      e_step=(ub-lb)/dble(n_step)
      start=lb
      do while(start.le.ub)
c     do start=lb,ub,e_step
c       call minim(fmintmp,xmintmp,lb,ub,iconv)
       call minimbis(fmintmp,xmintmp,lb,ub,iconv,haydif,mq,tol)
c        write(*,*)' fmintmp, fmin = ', fmintmp, fmin
        if (fmintmp .lt. fmin) then
          fmin=fmintmp
          xmin=xmintmp
        end if
        start=start+e_step
      end do
c      write(*,*)' iconv = ',iconv
      end subroutine
cc
c
cc
C
C
C  GIVEN THREE HARMONIC FUNCTIONS RT(X),T(X),S(X).
C  THIS  SUBROUTINE FINDS U(X) AND V(X) SUCH THAT:
C
C       RT(X)/T(X)S(X) = U(X)/T(X) + V(X)/S(X)    (X=COS(KW))
C
C      INPUT PARAMETERS
C    RT : FIRST HARMONIC FUNCTION
C   NRT : DIMENSION OF RT
C     T : SECOND HARMONIC FUNCTION
C     T : DIMENSION OF T
C     S : THIRD HARMONIC FUNCTION
C    NS : DIMENSION OF S
C     U : HARMONIC FUNCTION FIRST PARTIAL FRACTION
C    NU : DIMENSION OF RT
C     V : HARMONIC FUNCTION SECOND PARTIAL FRACTION
C    NV : DIMENSION OF RT
C
      subroutine PARFRA(rt,nrt,t,nt,s,ns,u,nu,v,nv)
C
C.. Implicits ..
      implicit none
      real*8 ZERO,ONE,TWO
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0)
      include 'units.cmn'
C
C.. Formal Arguments ..
      integer nrt,nt,ns,nu,nv
      real*8 rt(*),t(*),s(*),u(*),v(*)
C
C.. Local Scalars ..
      integer i,j,m,n,ncol,p
C
C.. Local Arrays ..
      real*8 a(60),cc(60,66)
C
C.. External Calls ..
      external CONJM, CONVM, MLTSOL
C
C ... Executable Statements ...
C
      do i = 1,60
       do j = 1,66
        cc(i,j) = ZERO
       end do
      end do
      m = nt - 1
      n = ns - 1
      p = m + n
      do i = 1,m
       a(i) = ONE
      end do
      ncol = 0
      call CONVM(s,ns,a,m,cc,ncol)
      call CONJM(s,ns,a,m,cc,ncol)
      do i = 1,n
       a(i) = ONE
      end do
      ncol = m
      call CONVM(t,nt,a,n,cc,ncol)
      call CONJM(t,nt,a,n,cc,ncol)
      do i = 1,p
       do j = 1,p
        cc(i,j) = cc(i,j) / TWO
       end do
      end do
      do i = 1,p
       cc(i,p+1) = rt(i)
      end do
      i = 1
*      WRITE(Mtprof,*)'  subroutine PARFRA, call 1'
      call MLTSOL(cc,p,i,60,66)
      do i = 1,m
       u(i) = cc(i,p+1)
      end do
      nu = m
      do i = m+1,p
       v(i-m) = cc(i,p+1)
      end do
      nv = n
      end
C
C  THIS SUBROUTINE COMPUTES THE PRODUCT OF A COLUMN ARRAY AND
C  B ROW ARRAY. THE OUTPUT IS THE MATRIX C
C    INPUT PARAMETER
C      A : COLUMN ARRAY    (true signs)
C MPLUS1 : DIMENSION OF A
C      B : ROW ARRAY       (true signs)
C NPLUS1 : DIMENSION OF C
C      C : OUTPUT MATRIX
C   NCOL : NUMBER OF FIRST COLUMNS OF C WHERE PUT THE PRODUCT
C
      subroutine CONVM(a,mplus1,b,nplus1,c,ncol)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(*)
C.. In/Out Status: Read, Not Written ..
      integer mplus1
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 b(*)
C.. In/Out Status: Maybe Read, Not Written ..
      integer nplus1
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 c(60,66)
C.. In/Out Status: Maybe Read, Not Written ..
      integer ncol
C
C.. Local Scalars ..
      integer i,j,num
C
C ... Executable Statements ...
C
      do i = 1,mplus1
       do j = 1,nplus1
        num = i + j - 1
        c(num,j+ncol) = c(num,j+ncol) + a(i)*b(j)
       end do
      end do
      end
C
C
C
C THIS SUBROUTINE COMPUTES THE INVERTIBLE MA PROCESS WITH A
C GIVEN AUTOCOVARIANCE FUNCTION
C
C INPUT PARAMETER
C
C    UFIN : COEFFICENTS OF (B+F)
C   NUFIN : DIMENSION OF UFIN
C   THETA : MA POLYNOMIAL
C  NTHETA : DIMENDION OF THETA
C     VAR :   VARIANCE OF THE MA PROCESS
C  NNIO : NO UNIT ROOTS IN THE MA POLYNOMIAL
C NOPRINT : NO PRINTOUT OF THE ROOTS
C
C
      subroutine MAK1(ufin,nufin,theta,ntheta,var,nnio,noprint,caption,
     $                lenCaption,toterr,thisId,lenId)
C
C
C       THE INPUT ARRAY UFIN IS :
C
C       UFIN(1)=GAM(0)
C       UFIN(2)=2*GAM(1)
C       ..
C       ..
C       ..
C       UFIN(NUFIN)=2*GAM(NUFIN-1)
C
C THE PARAMETER NNIO CONTROLS THAT THE MA POLYNOMIAL HAVE NOT UNIT
C ROOTS.
C
C IF a+bi IS |.| = 1 WE TRANSFORM IT AS FOLLOW :
C
C   X=a/b    a' = X * XL / SQRT(X^2+1)  AND   b = XL / SQRT(X^2 + 1)
C
C WHERE XL IS THE INPUT PARAMETER.
C
C WITH THIS TRANSFORMATION THE ARGUMENT AND PERIOD OF THE NEW ROOT IS
C THE SAME OF THE OLD ONE.
C
C
C
C
C.. Implicits ..
      implicit none
C
      logical T
      parameter (T = .true.)
      real*8 ZERO,ONE,TWO
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0)
C..   INPUT PARAMETERS.
      integer nufin,nnio,noprint,lenCaption,lenId
      real*8 ufin(*)
      character caption*(*),thisId*(*)
c     OUTPUT theta(*),var
      integer ntheta
      real*8 theta(*),var,toterr
C
C.. Local Scalars ..
      integer i,ia,ib,irow,j,k,n,nroots,nrpoly,last,ContR
      character blan*4,ctwo*4
      real*8 a,b,gamzer,pi,temp,temp1,tol,v,vv,vw,w,ww,xeps
c      real*8 tmp
C
C.. Local Arrays ..
      character per(64)*4
      real*8 ar(64),ar1(32),imz(64),imz1(32),modul(64),modul1(32),
     $       poly(34),pr(64),pr1(32),r1(2),r2(2),rez(64),rez1(32),
     $       rdpoly(34)
      complex*16 az(64),bz(64)
      real*8 gRez(64),gImz(64),gModul(64),gAR(64),gPR(64)
      character gper(64)*4
      integer gCont(64),ng0
      real*8 vn(64)
      integer nvn
C
C.. External Calls ..
      external MPBC, ROOTC, RPQ, SYMPOLY,ISTRLEN,grRoots,HalfRoots
      integer ISTRLEN
C
C.. Intrinsic Functions ..
      intrinsic ABS, ACOS, DBLE, DCMPLX, SQRT
      include 'stream.i'
      include 'unitmak.i'
C   LINES OF CODE ADDED FOR X-13A-S : 1
      include 'error.cmn'
C   END OF CODE BLOCK
*      include 'indhtml.i'
C
C.. Data Declarations ..
      data blan/' -  '/
      data ctwo/'2.0 '/
C
C ... Executable Statements ...
C
c    added line to initialize per BCM 9-19-2002
      do i=1,64
       per(i)=blan
       grez(i)=0d0
      end do
      pi = 3.14159265358979d0
      tol = 1.0d-5
C
C
C    SET UP THE SYMMETRIC POLYNOMIAL
C
      gamzer = ufin(1)
      n = nufin
      do i = 1,nufin-1
       poly(i) = ufin(nufin+1-i)
      end do
      poly(nufin) = ufin(1) * 2
C
C  FIND THE ROOTS USING RPQ
C
      if (n .le. 2) then
       rez1(1) = -poly(2)/poly(1)
       imz1(1) = ZERO
       nrpoly = 2
      else
       call SYMPOLY(poly,n,rdpoly,nrpoly)
       call RPQ(rdpoly,nrpoly,rez1,imz1,modul1,ar1,pr1,1,noprint)
C   LINES OF CODE ADDED FOR X-13A-S : 1
       IF(Lfatal)RETURN
C   END OF CODE BLOCK
      end if
C
C  FIND THE ROOTS OF THE ORIGINAL SYMMETRIC POLYNOMIAL
C
      k = 1
      do i = 1,nrpoly-1
       a = -rez1(i)
       b = -imz1(i)
       call ROOTC(a,b,r1,r2)
       temp = SQRT(r1(1)**2+r1(2)**2)
       temp1 = SQRT(r2(1)**2+r2(2)**2)
       rez(k) = r1(1)
       imz(k) = r1(2)
       modul(k) = temp
       if (modul(k) .lt. 1.0d-8) then
        ar(k) = ZERO
       else
        ar(k) = rez(k) / modul(k)
       end if
       if (ABS(ar(k)) .le. ONE) then
        ar(k) = ACOS(ar(k))
        if (imz(k) .lt. ZERO) then
         ar(k) = -ar(k)
        end if
       else
        ar(k) = ZERO
        if (rez(k) .lt. ZERO) then
         ar(k) = pi
        end if
       end if
       k = k + 1
       rez(k) = r2(1)
       imz(k) = r2(2)
       modul(k) = temp1
       if (modul(k) .lt. 1.0d-8) then
        ar(k) = ZERO
       else
        ar(k) = rez(k) / modul(k)
       end if
       if (ABS(ar(k)) .le. ONE) then
        ar(k) = ACOS(ar(k))
        if (imz(k) .lt. ZERO) then
         ar(k) = -ar(k)
        end if
       else
        ar(k) = ZERO
        if (rez(k) .lt. ZERO) then
         ar(k) = pi
        end if
       end if
       k = k + 1
      end do
      nroots = k - 1
      do i = 1,nroots
       if ((imz(i).gt.tol) .or. (imz(i).lt.-tol)) then
        pr(i) = TWO * pi / ar(i)
       else
        pr(i) = 999.99
        per(i) = blan
        if (rez(i) .lt. ZERO) then
         per(i) = ctwo
        end if
       end if
       ar(i) = 180.0d0 * ar(i) / pi
      end do
      n = k
      xeps = 1.0d-30
C
C SELECT THE ROOTS TO FIND THE MA PROCESS
C
      call grRoots(rez,imz,modul,AR,PR,Per,n-1,
     $          gRez,gImz,gModul,gAR,gPR,gPer,gCont,ng0)
      call HalfRoots(gRez,gImz,gModul,gAR,gPR,gPer,gCont,ng0,
     $          Rez1,Imz1,modul1,ar1,PR1,Per,ia)
      if (nnio .eq. 1) then
       do i = 1,ia
        if (ABS(modul1(i)-ONE) .lt. 1.0d-8) then
         if ((imz1(i).gt.tol) .or. (imz1(i).lt.-tol)) then
          temp = rez1(i) / imz1(i)
          rez1(i) = (temp*Xl) / SQRT(temp**2+1)
          imz1(i) = Xl / SQRT(temp**2+1)
          modul1(i) = Xl
         else
          rez1(i) = Xl
          modul1(i) = Xl
         end if
        end if
       end do
      end if
      if (noprint .ne. 1) then
       if(lenCaption.gt.0)then
        if (lenid.gt.0) CALL makDivId(Nio,thisId(1:lenId),'@')
        CALL mkTableTag(Nio,'w60',caption(1:lenCaption))
        CALL mkCaption(Nio,caption(1:lenCaption))
       else
        CALL mkTableTag(Nio,'w60','Roots')
       end if
 7001  format ('<tr><td class="head">&nbsp;</td>',
     &         5('<th scope="col">',a,'</th>'),'</tr>')
       write (Nio,7001)
     $         'REAL PART','IMAGINARY PART','MODULUS','ARGUMENT',
     &         'PERIOD (DEGREES)'
       ContR=0
       do i = 1,ia
         if (imz1(i) .ge. -tol) then  
          contR=contR+1
          if (ABS(pr1(i)-999.99) .lt. 1.d-12) then
 6001      format ('<tr><th scope="row">Root ',i2,'</th>',
     $             4('<td class="center">',f11.3,'</td>'),
     $             '<td class="center">',a4,'</td></tr>')
           write(Nio,6001) contR,rez1(i),imz1(i),modul1(i),ar1(i),per(i)
          else
 6002      format ('<tr><th scope="row">Root ',i2,'</th>',
     $             5('<td class="center">',f11.3,'</td>'),'</tr>')
           write(Nio,6002) contR,rez1(i),imz1(i),modul1(i),ar1(i),pr1(i)
          end if
         end if
       end do
       CALL writTag(Nio,'</table>')
       if (lenid.gt.0) CALL writTag(Nio,'</div>')
      end if
C
C  BUILD IN THE POLYNOMIAL IN B (USING MPBC)
C
      do i = 1,64
       az(i) = DCMPLX(ZERO,ZERO)
       bz(i) = DCMPLX(ZERO,ZERO)
      end do
      ntheta = ia + 1
      if (ia .gt. 1) then
       az(1) = DCMPLX(ONE,ZERO)
       az(2) = -DCMPLX(rez1(1),imz1(1))
       bz(1) = DCMPLX(ONE,ZERO)
       bz(2) = -DCMPLX(rez1(2),imz1(2))
       call MPBC(az,bz,1,1,bz)
       if (ia .gt. 2) then
        do i = 2,ia-1
         az(1) = DCMPLX(ONE,ZERO)
         az(2) = -DCMPLX(rez1(i+1),imz1(i+1))
         call MPBC(bz,az,i,1,bz)
        end do
       end if
       do i = 1,ntheta
        theta(i) = DBLE(bz(i))
       end do
      end if
      if (ia .eq. 1) then
       theta(1) = ONE
       theta(2) = -rez1(1)
       ntheta = 2
      end if
C
C     COMPUTE THE VARIANCE
C
      var = ZERO
      do i = 1,ia+1
       var = var + theta(i)**2
      end do
      var = gamzer / var
c      Compute Toterr
      call CONJ(theta,ntheta,theta,ntheta,vn,nvn)
      toterr = ZERO
      do i = 1,nvn
         toterr = toterr + (vn(i)*var-ufin(i))**2
      end do
      if (noprint.ne.1) then
        if (nvn .ne. nufin) then
         CALL wWritln('THE LENGTH OF THE MA DOES NOT MATCH WITH '//
     $                'THE ACF',Nio,0,T,T)
        end if
        write (Nio,7035) toterr
 7035   format('<p class="center"><strong>TOTAL SQUARED ERROR=',
     $         '</strong>',d15.7,'</p>')
      end if
      CALL writTag(Nio,'&nbsp;')
      return
      end
c
c
c   GrRoots group roots that are equal, and put consecutive roots that are conjugate complex
      subroutine grRoots(rez,Imz,modul,Ar,Pr,Per,nr,
     $                  gRez,gImz,gModul,gAr,gPr,gPer,gCont,ng0)
      implicit none
      real*8 Xeps
      parameter(Xeps=1.0D-13)
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
c     INPUT 
      real*8 rez(64),Imz(64),modul(64),Ar(64),Pr(64)
      character Per(64)*4
      integer nr
c     OUTPUT
      real*8 gRez(64),gImz(64),gModul(64),gAr(64),gPr(64)
      character gPer(64)*4
      integer gCont(64),ng0
c     EXTERNAL
      intrinsic abs
      integer getRoot,getRootc,closestRoot
      external getRoot,getRootc,closestRoot
c     Local variables
      integer i,ni,ic,i2
      real*8 Xeps2
c
      ng0=0
      i=1
      do while(i .le. nr)
        if (abs(modul(i)-ONE).lt.xeps) then
          xeps2=1.0D-30
        else
          xeps2=1.0D-30
        end if
        ni=getRoot(gRez,gImz,ng0,rez(i),Imz(i),xeps2)
        if (ni .gt. 0) then
          gCont(ni)=gCont(ni)+1
        else
          ng0=ng0+1
          gRez(ng0)=Rez(i)
          gImz(ng0)=Imz(i)
          gModul(ng0)=Modul(i)
          gAR(ng0)=AR(i)
          gPr(ng0)=Pr(i)
          gPer(ng0)=Per(i)
          gCont(ng0)=1
          if (abs(Imz(i)).gt.xeps2) then
c           The root is complex, we search the conjugate complex
            iC=getRootc(Rez,Imz,i+1,nr,rez(i),-imz(i),xeps2)
            if (ic.eq.0) then
c             ERROR not found conjugate complex root
              ic=ic
            else
              ng0=ng0+1
              gRez(ng0)=Rez(ic)
              gImz(ng0)=Imz(ic)
              gModul(ng0)=Modul(ic)
              gAR(ng0)=AR(ic)
              gPR(ng0)=PR(ic)
              gPer(ng0)=Per(ic)
c   !We will increment later when i reach Ic
              gCont(ng0)=0
            end if
          else
c  !We suppose is 0.0 Imz(i) is too close to 0.0
            gIMz(ng0)=ZERO
          end if
        end if
        i=i+1
      enddo
c     NOW we avoid single unit roots
      i=1
      do while(i.lt.ng0)
        if ((gCont(i).eq.1).and.(abs(gmodul(i)-ONE).lt.Xeps)) then
          i2=closestRoot(gRez,gImz,gModul,i+1,ng0,gRez(i),gImz(i),Xeps)
          call JoinRoot(gRez,gImz,gModul,gAR,gPr,gPer,gCont,ng0,i,i2)
        end if
        i=i+1
      enddo
      end
c
c
c     getRoot return an index to the arrays Rez,Img 
c               so [Rez(getRoot),Imz(getroot)]=[realr,imagr]
      integer function getRoot(Rez,Imz,nr,realr,imagr,Xeps)
      implicit none
c     INPUT
      real*8 Xeps
      real*8 Rez(*),Imz(*),realr,imagr
      integer nr
c     LOCAL
      integer i
c
      i=1
      do while (i.le.nr)
        if ((abs(Rez(i)-realr) .le.xeps) .and. 
     $        (abs(imz(i)-imagr).le.Xeps)) then
          getRoot=i
          return
        else
          i=i+1
        end if
      enddo
      getRoot=0  !Root Not FOUND
      end
c
c
c     getRootc return an index to the arrays Rez,Img 
c               so [Rez(getRoot),Imz(getroot)]=[realr,imagr]
c     we begin searching from the last
      integer function getRootc(Rez,Imz,ni,nr,realr,imagr,Xeps)
      implicit none
c     INPUT
      real*8 Xeps
      real*8 Rez(*),Imz(*),realr,imagr
      integer ni,nr
c     LOCAL
      integer i
c
      i=nr
      do while (i.ge.ni)
        if ((abs(Rez(i)-realr) .le.xeps) .and. 
     $        (abs(imz(i)-imagr).le.Xeps)) then
          getRootc=i
          return
        else
          i=i-1
        end if
      enddo
      getRootc=0  !Root Not FOUND
      end
c
c     ClosestRoot find the closest root to a given one
      integer function closestRoot(gRez,gImz,gModul,ni,ng0,
     $                        Realz,Imagz,Xeps)
c     INPUT
      implicit none
      real*8 gRez(*),gImz(*),gModul(*),Realz,Imagz,Xeps
      integer ng0,ni,nr
c     LOCAL VARIABLES
      real*8 XepsRec,Xeps2,mindist,dist
      integer i,i2
c     EXTERNAL
*      integer getRootM
*      external getRootM
c
      i2=0
      minDist=1.0d10
      do i=ni,ng0
        dist=(gRez(i)-Realz)*(gRez(i)-Realz)+
     $      (gImz(i)-Imagz)*(gImz(i)-Imagz)
        if (dist.lt.mindist) then
          i2=i
          mindist=dist
        end if
      enddo
      ClosestRoot=i2
      end
c
c     JOINROOT join to roots in one
      subroutine JoinRoot(gRez,gImz,gModul,gAr,gPr,gPer,gCont,ng0,
     $                   ni,ni2)
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C     INPUT 
      integer ni,ni2
c     INPUT&OUTPUT
      real*8 gRez(64),gImz(64),gModul(64),gAr(64),gPr(64),pi
      character gPer(64)*4
      integer gCont(64),ng0
c     External
      intrinsic SQRT,ATAN
c     LOCAL VARIABLES
      real*8 SUMgCont,m
      integer i
c
c
      pi = 3.14159265358979d0
      SUMgCont=gCont(ni)+gCont(ni2)
      gModul(ni)=(gModul(ni)*gModul(ni2))
      gRez(ni)=(gRez(ni)*gCont(ni)+gRez(ni2)*gCont(ni2))/SUMgcont
      gImz(ni)=(gImz(ni)*Gcont(ni)+gImz(ni2)*gCont(ni2))/SUMgcont
      gCont(ni)=SUMgCont
      m=gRez(ni)*gRez(ni)+gImz(ni)*gImz(ni)
      m=SQRT(gModul(ni)/m)
      gRez(ni)=gRez(ni)*m
      gImz(ni)=gImz(ni)*m
      gModul(ni)=SQRT(gModul(ni))
      if (gRez(ni).gt.ZERO) then
        gAR(ni)=(atan(gImz(ni)/gRez(ni))*180.0D0)/pi
      else if (gRez(ni).lt.ZERO) then
        gAR(ni)=180.0D0+(atan(gImz(ni)/gRez(ni))*180.0D0)/pi
        if  (gAR(ni).gt.180.0D0) gAR(ni)=180.0D0-gAR(ni)
      else if (gImz(ni).gt.ZERO) then
        gAR(ni)=90.0D0
      else
        gAR(ni)=-90.0D0
      end if
      if (gAR(ni).ne.ZERO) then
      gPR(ni)=360/gAR(ni)
      else
      gPR(ni)=999.99
      end if
      ng0=ng0-1
      do i=ni2,ng0
        gRez(i)=gRez(i+1)
        gImz(i)=gImz(i+1)
        gModul(i)=gModul(i+1)
        gAR(i)=gAR(i+1)
        gPR(i)=gPR(i+1)
        gPer(i)=gPer(i+1)
        gCont(i)=gCont(i+1)
      enddo
      end
c
c
c
c
c
c     HalfRoots: given the roots of a simetrical polynomial S(B,F) grouped by grRoots, 
c            return the roots without grouping that generate a polynomial P(B) 
c            that fullfil P(B)P(F)=S(B,F)
c
      subroutine halfRoots(gRez,gImz,gModul,gAr,gPr,gPer,gCont,ng0,
     $                rez,imz,modul,ar,pr,per,nr)
      implicit none
      real*8 Xeps
      parameter (Xeps=1.0D-13)
      real*8 ZERO,ONE,TWO
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0)
c     INPUT
      real*8 gRez(64),gImz(64),gModul(64),gAr(64),gPr(64)
      character gPer(64)*4
      integer gCont(64),ng0
c     OUTPUT
      real*8 rez(32),imz(32),modul(32),ar(32),pr(32)
      character per(32)*4
      integer nr
c     LOCAL VARIABLES
      integer i,j,nRep,k
      real*8 xeps2
c
      xeps2=1.0D-10
      i=1
      j=1
      do while (i.le.ng0)
        if (abs(gmodul(i)-1).lt.Xeps) then
          nRep=gCont(i)/2
          if (gCont(i).eq.1) then
            if (i.ge.ng0) then
c             ERROR
              i=i
            else if ((gCont(i+1).eq.1).and.
     $          (abs(gModul(i+1)-ONE).lt.xeps)) then
              i=i+1
              if (gRez(i).gt.ZERO) then
                Rez(j)=gModul(i)
                Imz(j)=ZERO
                modul(j)=gModul(i)
                per(j)=gper(i)
                PR(j)=999.0D0
                AR(j)=180.0d0
              else
                Rez(j)=-gModul(i)
                Imz(j)=ZERO
                modul(j)=gModul(i)
                per(j)=gper(i)
                PR(j)=TWO
                AR(j)=ZERO 
              end if
              j=j+1
c            else
c              !ERROR
            end if
          end if
        else if (gmodul(i) .lt. ONE) then
          nRep=gCont(i)
        else
          nRep=0
        end if
        do k=1,nRep
          Rez(j)=gRez(i)
          Imz(j)=gImz(i)
          Modul(j)=gModul(i)
          AR(j)=gAR(i)
          PR(j)=gPR(i)
          Per(j)=gPer(i)
          j=j+1
          if ((abs(gImz(i)).gt.xeps2).and.
     $         (abs(grez(i)-grez(i+1)).lt.xeps2)) then
c           if (gCont(i).ne.gCont(i+1)) then
c             !ERROR
c           end if
            Rez(j)=gRez(i+1)
            Imz(j)=gImz(i+1)
            Modul(j)=gModul(i+1)
            AR(j)=gAR(i+1)
            PR(j)=gPR(i+1)
            Per(j)=gPer(i+1)
            j=j+1
          end if
        enddo
        if ((abs(gImz(i)).gt.xeps2) .and.
     $      (abs(grez(i)-grez(i+1)).lt.Xeps2) .and. (nRep.gt.0)) then
           i=i+2  !We junk the conjugate complex root
        else
           i=i+1  
        end if
      enddo
      nr=j-1
      end
C
C
C  THIS SUBROUTINE COMPUTES THE PRODUCT OF TWO HARMONIC FUNCTION
C  A COLUMN ARRAY AND B ROW ARRAY THE OUTPUT IS THE MATRIX C.
C    INPUT PARAMETER
C      A : COLUMN ARRAY (true signs)
C MPLUS1 : DIMENSION OF A
C      B : ROW ARRAY    (true signs)
C NPLUS1 : DIMENSION OF C
C      C : OUTPUT MATRIX
C   NCOL : NUMBER OF FIRST COLUMNS OF C WHERE PUT THE PRODUCT
C
C
C
      subroutine CONJM(a,mplus1,b,nplus1,c,ncol)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(*)
C.. In/Out Status: Read, Not Written ..
      integer mplus1
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 b(*)
C.. In/Out Status: Maybe Read, Not Written ..
      integer nplus1
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 c(60,66)
C.. In/Out Status: Maybe Read, Not Written ..
      integer ncol
C
C.. Local Scalars ..
      integer i,j,k,num
C
C.. Intrinsic Functions ..
      intrinsic ABS
C
C ... Executable Statements ...
C
      do i = 1,mplus1
       do j = 1,nplus1
        k = i - j
        num = ABS(k) + 1
        c(num,j+ncol) = c(num,j+ncol) + a(i)*b(j)
       end do
      end do
      end
C
C
C  THIS SUBROUTINE COMPUTES THE PRODUCT OF TWO POLYNOMIALS IN B
C  WITH COMPLEX COEFFICIENT C=A*B
C     INPUT PARAMETERS
C       A : FIRST POLYNOMIAL  (true signs)  A(1) + A(2)*X + ... +
C                                           A(N)*X^(N-1)
C       B : SECOND POLYNOMIAL (true signs)  "   "      "      "
C       N : DIMENSION OF A
C       M : DIMENSION OF B
C       E : THE PRODUCT A * B
C
      subroutine MPBC(a,b,n,m,e)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Read, Not Written ..
      integer m
C.. In/Out Status: Maybe Read, Not Written ..
      complex*16 a(0:n)
C.. In/Out Status: Maybe Read, Not Written ..
      complex*16 b(0:m)
C.. In/Out Status: Maybe Read, Maybe Written ..
      complex*16 e(0:n+m)
C
C.. Local Scalars ..
      integer i,j,k
C
C.. Local Arrays ..
      complex*16 aa(0:100),bb(0:100)
      intrinsic DCMPLX
C
C ... Executable Statements ...
C
      do i = 0,m
       bb(i) = b(i)
      end do
      do i = 0,n
       aa(i) = a(i)
      end do
      do i = 0,n+m
       e(i) = DCMPLX(ZERO,ZERO)
      end do
      do i = 0,n
       do j = 0,m
        k = i + j
        e(k) = e(k) + aa(i)*bb(j)
       end do
      end do
      end
C
C THIS SUBROUTINE REDUCES A SYMMETRIC POLYNOMIAL WITH n COEFFICIENTS
C TO A POLYNOMIAL OF n/2 COEFFICIENTS.( USED IN MAK; SEE DOCUMENTATION.)
C
C     INPUT PARAMETER
C    POLY : THE ORIGINAL SYMMETRIC POLYNOMIAL WITH n COEFFICIENTS
C   NPOLY : DIMENSION OF POLY
C  RdPOLY : THE SIMPLIFIED POLYNOMIAL WITH n/2 COEFFICIENTS
C  NRPOLY : DIMENSION OF RPOLY
C
      subroutine SYMPOLY(poly,npoly,rdpoly,nrpoly)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer npoly
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 poly(npoly)
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 rdpoly(34)
C.. In/Out Status: Not Read, Overwritten ..
      integer nrpoly
C
C.. Local Scalars ..
      integer i,j,ns0,ns1,ns2
      real*8 temp
C
C.. Local Arrays ..
      real*8 poly1(64),s(64,64),s0(64),s1(64),s2(64)
C
C ... Executable Statements ...
C
      s0(1) = 2
      ns0 = 1
      s1(1) = 0
      s1(2) = 1
      ns1 = 2
      do i = 1,npoly
       do j = 1,npoly
        s(i,j) = ZERO
       end do
      end do
      s(1,1) = 1
      s(2,2) = 1
      do i = 3,npoly
       s2(1) = ZERO
       do j = 2,ns1+1
        s2(j) = s1(j-1)
       end do
       ns2 = ns1 + 1
       s0(ns0+1) = ZERO
       s0(ns0+2) = ZERO
       ns0 = ns0 + 2
       do j = 1,ns2
        s2(j) = s2(j) - s0(j)
       end do
       do j = 1,ns2
        s(j,i) = s2(j)
       end do
       do j = 1,ns1
        s0(j) = s1(j)
       end do
       do j = 1,ns2
        s1(j) = s2(j)
       end do
       ns1 = ns2
      end do
      do i = 1,npoly
       poly1(npoly+1-i) = poly(i)
      end do
      do i = 1,npoly
       temp = ZERO
       do j = 1,npoly
        temp = temp + s(i,j)*poly1(j)
       end do
       rdpoly(npoly+1-i) = temp
      end do
      nrpoly = npoly
      end
C
C THIS SUBROUTINE COMPUTES THE SQUARE ROOT OF A COMPLEX NUMBER
C
C   INPUT PARAMETERS
C     REZ : REAL PART OF COMPLEX NUMBER
C     IMZ : IMAGINARY PART OF COMPLEX NUMBER
C    REZ1 : REAL PART OF SQUARE ROOT
C    IMZ1 : IMAGINARY PART OF SQUARE ROOT
C
C
C
      subroutine SQROOTC(rez,imz,rez1,imz1)
C
C.. Implicits ..
      implicit none
      real*8 ZERO,ONE,TWO
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      real*8 rez
C.. In/Out Status: Read, Not Written ..
      real*8 imz
C.. In/Out Status: Not Read, Overwritten ..
      real*8 rez1
C.. In/Out Status: Not Read, Overwritten ..
      real*8 imz1
C
C.. Local Scalars ..
      real*8 temp
C
C.. Intrinsic Functions ..
      intrinsic ABS, SQRT
C
C ... Executable Statements ...
C
      if (rez .ge. ZERO) then
       temp = SQRT((rez**2)+(imz**2))
       rez1 = SQRT((rez+temp)/2.0d0)
       if (ABS(rez1) .lt. 1.0d-8) then
        imz1 = ZERO
       else
        imz1 = imz / (2.0d0*rez1)
       end if
      else
       temp = SQRT((rez**2)+(imz**2))
       if (imz .gt. ZERO) then
        imz1 = SQRT((ABS(rez)+temp)/2.0d0)
       else
        imz1 = -SQRT((ABS(rez)+temp)/2.0d0)
       end if
       if (ABS(imz1) .lt. 1.0d-8) then
        rez1 = ZERO
       else
        rez1 = imz / (2.0d0*imz1)
       end if
      end if
      end
c
C
C     CubicRoot return the cubic root of a given complex number  (rX+i*iX)=(rY+iY)^3
C             if the number is real we will return a real root
      Subroutine CubicRoot(rX,iX,rY,iY)
      implicit none
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C     INPUT
      real*8 rX,iX
c     OUTPUT
      real*8 rY,iY
c     LOCAL VARIABLES
      real*8 w,m,s,c
c     EXTERNAL FUNCTIONS
      real*8 ARG
      external ARG
      intrinsic ABS,SQRT,SIN,COS
c
      if (iX.eq.ZERO) then
        iY=ZERO
        rY=abs(rX)**(ONE/3.0D0)
        if (rX.lt.ZERO) then
          rY=-rY
        end if
      else
        w=arg(rX,iX)
        m=rX*rX+iX*iX
        rY=(m**(ONE/6.0D0))*cos(w/3.0D0)
        iY=(m**(ONE/6.0D0))*sin(w/3.0D0)
      end if
      end
c
c     ARG return the argument of a complex number rX+i*iX=sqrt(rX*rX+iX*iX)*exp(i*arg)
      real*8 function Arg(rX,iX)
      implicit none
      real*8 pi
      parameter(pi=3.14159265358979D0)
      real*8 ZERO
      parameter (ZERO=0.0d0)
c     INPUT
      real*8 rX,iX
      intrinsic ABS,atan
c
      if (rX .gt. ZERO) then
        arg=atan(iX/rX)
      else if (rX.lt.ZERO) then
        arg=pi-abs(atan(iX/rX))
      else if (iX.gt.ZERO) then
        arg=pi/2
      else
        arg=-pi/2
      end if
      end
c
c     MulCompl multiply two complex numbers
      subroutine MulCompl(rX,iX,rY,iY,rZ,iZ)
      implicit none
c     INPUT
      real*8 rX,iX,rY,iY
c     OUTPUT
      real*8 rZ,iZ
      rZ=rX*rY-iX*iY
      iZ=rX*iY+rY*iX
      end
c
c     DivCompl divide two complex numbers
      subroutine DivCompl(rX,iX,rY,iY,rZ,iZ)
      implicit none
c     INPUT
      real*8 rX,iX,rY,iY
c     OUTPUT
      real*8 rZ,iZ
c     LOCAL 
      real*8 m2
      m2=rY*rY+iY*iY
      rZ=(rX*rY+iX*iY)/m2
      iZ=(iX*rY-iY*rX)/m2
      end
C
C THIS SUBROUTINE COMPUTES THE COMPLEX ROOTS OF A SECOND ORDER EQUATION
C WITH COMPLEX COEFFICIENT OF THE FOLLOWING TYPE :
C            (X^2 + (REZ,IMZ) * X + 1) = 0
C
C      INPUT PARAMETERS
C    REZ : REAL PART OF THE SECOND COEFFICIENT OF EQUATION
C    IMZ : IMAGINARY PART OF THE SECOND COEFFICIENT OF EQUATION
C     R1 : (R1(1) REAL R1(2) IMAGINARY) FIRST SOLUTION
C     R2 : (R2(1) REAL R2(2) IMAGINARY) SECOND SOLUTION
C
      subroutine ROOTC(rez,imz,r1,r2)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      real*8 rez
C.. In/Out Status: Read, Not Written ..
      real*8 imz
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 r1(2)
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 r2(2)
C
C.. Local Scalars ..
      real*8 a,b,delta,deltai
C
C.. External Calls ..
      external SQROOTC
C
C ... Executable Statements ...
C
      delta = (rez**2) - (imz**2) - 4.0d0
      deltai = 2.0d0 * rez * imz
      call SQROOTC(delta,deltai,a,b)
      r1(1) = (-rez+a) / 2.0d0
      r1(2) = (-imz+b) / 2.0d0
      r2(1) = (-rez-a) / 2.0d0
      r2(2) = (-imz-b) / 2.0d0
      end
C
C
C
C THIS SUBROUTINE SOLVES UP TO 6 SETS OF EQUATIONS BY INVERTING THE
C SPARSE MATRIX A.   NOTE: A**(-1) IS NOT STORED
C
C  INPUT PARAMETERS
C    A : THE MATRIX WITH THE SET OF EQUATIONS
C    N : DIMENSION OF THE SET IN A
C    L : NUMBER OF SET OF EQUATIONS
C
C
      subroutine MLTSOL(a,n,l,pr,pc)
C
C.. Implicits ..
      implicit none
      include 'units.cmn'
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Read, Not Written ..
      integer l,pr,pc
C.. In/Out Status: Maybe Read, Maybe Written ..
c      real*8 a(n,n+l)
      real*8 a(pr,pc)
C
C.. Local Scalars ..
      integer i,i1,irev,j,k,n1,nl
      real*8 fac,pivot,u,min1,min2
*      logical ldebug
C
C.. Local Arrays ..
      integer m(66)
      real*8 b(60)
C
C.. Intrinsic Functions ..
      intrinsic ABS
C
C ... Executable Statements ...
C
      min1=10.d0**(-15.d0)
      min2=0-min1
*      ldebug = .true.
      nl = n + l
      do k = 1,nl
       m(k) = 0
      end do
      do irev = 1,n
       i = n - irev + 1
       u = 10.d-30
       do k = 1,n
        if (ABS(a(i,k)).gt.u .and. m(k).eq.0) then
         u = ABS(a(i,k))
         i1 = k
        end if
       end do
       m(i1) = i
       pivot = 1 / a(i,i1)
       if (pivot.ge.min2 .and. pivot.le.min1) then
        pivot = ZERO
       end if
*       if (ldebug) then
*         write(Mtprof,*)' i = ',i,' n = ',n,
*     &              ' irev = ', irev,' pivot = ',pivot,' '
*       end if
       do j = 1,n
        if (j .ne. i) then
         if (ABS(a(j,i1)) .ge. 1.0d-13) then
          fac = pivot * a(j,i1)
          do k = 1,nl
           if (a(i,k).ge.min2 .and. a(i,k).le.min1) then
*       if (ldebug) then
*         write(Mtprof,*)' a(',i,',',k,') = ',a(i,k)
*       end if
            a(i,k) = ZERO
           else
            if (m(k) .eq. 0) then
*       if (ldebug) then
*         write(Mtprof,*)' a(',j,',',k,') = ',a(j,k), ' fac = ', fac,
*     $                  ' a(',i,',',k,') = ',a(i,k), ' m(k) = ', m(k)
*       end if
             a(j,k) = a(j,k) - fac*a(i,k)
*            else
*       if (ldebug) then
*         write(Mtprof,*)' a(',j,',',k,') = ',a(j,k), ' m(k) = ', m(k)
*       end if
            end if
           end if
          end do
         end if
        end if
       end do
       do k = 1,nl
        if (m(k) .eq. 0) then
*       if (ldebug) then
*         write(Mtprof,*)' a(',i,',',k,') = ',a(i,k), ' m(k) = ', m(k)
*       end if
         a(i,k) = pivot * a(i,k)
        end if
       end do
*       if (ldebug) then
*         write(Mtprof,*)' '
*       end if
      end do
      n1 = n + 1
      do k = n1,nl
       do i1 = 1,n
         if (m(i1) .ne. 0) then
           b(i1) = a(m(i1),k)
         else
           b(i1)=0
         end if
       end do
       do i1 = 1,n
        a(i1,k) = b(i1)
       end do
      end do
      end
C
C
C
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      subroutine TABLE(data,decp)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      subroutine TABLE2(datax)
C   END OF CODE BLOCK
C
C.. Implicits ..
      implicit none
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 datax(*)
C.. In/Out Status: Read, Overwritten ..
C
C.. Local Scalars ..
      integer i,i1,i2,ifact,j,jfact,kfreq,ndecp,nnper,ny,nyr
      integer*4 yr
      integer*4 decp
      real*8 sum,zz
C
C.. Local Arrays ..
      character fdecp1(7)*8,fn1(12)*8,fn2(12)*8,mth(12)*4,srt(11)*4,
     $          srt0(4)*4,srt1(6)*4,wrt0(8)*8,wrt2(7)*8,wrt99(7)*8
C
C.. Intrinsic Functions ..
      intrinsic ABS, INT, LOG10
      include 'sform.i'
      include 'stream.i'
C
C.. Data Declarations ..
      data mth/
     $     'JAN ','FEB ','MAR ','APR ','MAY ','JUN','JUL','AUG ','SEP',
     $     'OCT ','NOV ','DEC '/
      data srt/
     $     '1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH','9TH','10TH',
     $     '11TH'/
      data srt0/'1ST','2ND','1ST','2ND'/
      data srt1/'1ST','2ND','3RD','1ST','2ND','3RD'/
      data wrt2/'(1H ,I4,','N2','X,','N1','(F10','.DECP','))'/
      data wrt0/
     $     '(1H ,I4,','''-'',I4,','N2','X,','N1','(F10','.DECP','))'/
      data wrt99/'(/,1X,','''YEAR''','2X,','N2','(6X,','A4','))'/
      data fdecp1/'.0','.1','.2','.3','.4','.5','.6'/
      data fn1/'1','2','3','4','5','6','7','8','9','10','11','12'/
      data fn2/
     $     '2','12','22','32','42','52','62','72','82','092','102','112'
     $     /
C
C ... Executable Statements ...
C
      decp = 3
      kfreq = Nfreq
      if (kfreq .lt. 4) then
       if (Nfreq .eq. 3) then
        kfreq = 6
       else
        kfreq = 4
       end if
      end if
      nnper = Nper
      if (Nper .gt. Nfreq) then
       Nper = Nfreq
      end if
      ndecp = decp
      if (decp .ge. 6) then
       decp = 6
      end if
C  250 IF (DECP.NE.0) THEN
C        MDECP=10-DECP
C        A=0.00999999*10**MDECP
C        DO 151 I=1,NZ
C          IF(DATAx(I).LT.A) GO TO 151
C          DECP=DECP-1
C  151   CONTINUE
C      end if
C  251 CONTINUE
      ifact = 0
      zz = LOG10(ABS(datax(1))+.0000000001d0)
      sum = ABS(zz)
      do i = 2,Nz
       if (zz .gt. ZERO) then
        sum = ZERO
        goto 5000
       else
        zz = LOG10(ABS(datax(i))+.0000000001d0)
        if ((ABS(zz).lt.sum) .and. (zz.lt.ZERO)) then
         sum = ABS(zz)
        end if
       end if
      end do
 5000 if (zz .gt. ZERO) then
       sum = ZERO
      end if
      if (sum .gt. ONE) then
       ifact = INT(sum)
       if (ifact .gt. 6) then
        ifact = 6
       end if
       if (ifact .gt. 0) then
        write (Nio,'(4X, ''X  10.0D'',I2,/)') -ifact
       end if
      end if
      jfact = 0
      zz = LOG10(ABS(datax(1))+.0000000001d0)
      sum = zz
      do i = 2,Nz
       zz = LOG10(ABS(datax(i))+.0000000001d0)
       if ((zz.gt.sum) .and. (zz.gt.ZERO)) then
        sum = zz
       end if
      end do
      if (sum .gt. 4.0d0) then
       jfact = INT(sum) - 2
       if (jfact .gt. 0) then
        write (Nio,'(4X, ''X  10.0D'',I2,/)') jfact
       end if
      end if
      yr = Nyer
      if (Nfreq .eq. 12) then
 7000  format (/,1x,'YEAR',2x,12(6x,a4)/)
       write (Nio,7000) (mth(i), i = 1,12)
C      ELSE IF (NFREQ.EQ.4) THEN
C        WRITE(NIO,2002) (QRT(I),I=1,4)
C      ELSE IF (NFREQ.EQ.6) THEN
C        WRITE(NIO,2003) (SRT(I),I=1,6)
      else if (Nfreq .eq. 3) then
 7001  format (/,3x,'YEAR',5x,6(6x,a4)/)
       write (Nio,7001) (srt1(i), i = 1,6)
      else if (Nfreq .eq. 2) then
 7002  format (/,3x,'YEAR',5x,4(6x,a4)/)
       write (Nio,7002) (srt0(i), i = 1,4)
      else if (Nfreq .eq. 1) then
       write (Nio,7002) (srt(i), i = 1,4)
      else
       wrt99(4) = fn1(Nfreq)
       write (Nio,wrt99) (srt(i), i = 1,Nfreq)
      end if
      nyr = (Nz-(Nfreq-Nper+1)) / Nfreq
      ny = (Nz-(Nfreq-Nper+1)) - nyr*Nfreq
      if (ny .ne. 0) then
       nyr = nyr + 1
      end if
      nyr = nyr + 1
      wrt2(6) = fdecp1(decp+1)
      do i = 1,nyr
       i1 = (i-1)*kfreq - (Nper-2)
       i2 = i*kfreq - (Nper-1)
       if (i2 .ge. Nz) then
        i2 = Nz
       end if
       if (Nfreq .ge. 4) then
        wrt2(2) = fn2(1)
        wrt2(4) = fn1(kfreq)
       else
        wrt0(3) = fn2(1)
        wrt0(5) = fn1(kfreq)
        wrt0(7) = fdecp1(decp+1)
       end if
       if (i .eq. 1) then
        if (Nfreq .ge. 4) then
         wrt2(4) = fn1(kfreq-Nper+1)
         wrt2(2) = fn2(Nper)
        else
         wrt0(3) = fn2(Nper)
         wrt0(5) = fn1(kfreq-Nper+1)
        end if
        i1 = 1
       end if
       if (Nfreq .lt. 4) then
        if (ifact .gt. 0) then
         write (Nio,wrt0)
     $         yr, (yr+kfreq/Nfreq-1),
     $         (datax(j)*(10.0d0**ifact), j = i1,i2)
        else
         write (Nio,wrt0)
     $         yr, (yr+kfreq/Nfreq-1),
     $         (datax(j)*(10.0d0**(-jfact)), j = i1,i2)
        end if
       else if (ifact .gt. 0) then
        write (Nio,wrt2) yr, (datax(j)*(10.0d0**ifact), j = i1,i2)
       else
        write (Nio,wrt2) yr, (datax(j)*(10.0d0**(-jfact)), j = i1,i2)
       end if
       if (Nfreq .lt. 4) then
        yr = yr + kfreq/Nfreq
       else
        yr = yr + 1
       end if
       if (i2 .ge. Nz) goto 5001
      end do
 5001 decp = ndecp
      Nper = nnper
      end
C
C FUNCTION TO COMPUTE THE MEAN OF X SERIES
C
      double precision function DMEAN(n,x)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x(*)
C
C.. Local Scalars ..
      integer i
C
C ... Executable Statements ...
C
      DMEAN = ZERO
      do i = 1,n
       DMEAN = DMEAN + x(i)
      end do
      DMEAN = DMEAN / dble(n)
      end
C
C  ALL THE FOLLOWING SUBROUTINES AND FUNCTIONS COMPUTE THE ROOTS OF A
C  REAL POLYNOMIAL
C
      subroutine C02AEF(a,n,rez,imz,tol,ifail)
C     THIS ROUTINE ATTEMPTS TO SOLVE A REAL POLYNOMIAL EQUATION
C     HAVING N COEFFICIENTS (DEGREE  EQUALS  N-1) USING THE SEARCH
C     ALGORITHM PROPOSED IN GRANT AND HITCHINS (1971) TO
C     LIMITING MACHINE PRECISION.  ON ENTRY THE COEFFICIENTS
C     OF THE POLYNOMIAL ARE HELD IN THE ARRAY A(N), WITH A(0)
C     HOLDING THE COEFFICIENT OF THE HIGHEST POWER.  ON NORMAL
C     ENTRY THE PARAMETER IFAIL HAS VALUE 0 (HARD FAIL) OR 1
C     (SOFT FAIL) AND WILL BE ZERO ON SUCCESFUL EXIT WITH
C     THE CALCULATED ESTIMATES OF THE ROOTS HELD AS
C     REZ(K)+I*IMZ(K), K EQUALS N-1, IN APPROXIMATE DECREASING
C     ORDER OF MODULUS.  THE VALUE OF TOL IS OBTAINED BY
C     CALLING THE ROUTINE X02AJF.
C     ABNORMAL EXITS WILL BE INDICATED BY IFAIL HAVING
C     VALUE 1 OR 2.  THE FORMER IMPLIES THAT EITHER A(1) EQUALS 0
C     OR N.LT.2 OR N.GT.100.  FOR IFAIL  EQUALS  2, A POSSIBLE
C     SADDLE POINT HAS BEEN DETECTED.  THE NUMBER OF COEFFICIENTS
C     OF THE REDUCED POLYNOMIAL IS STORED IN N AND ITS
C     COEFFICIENTS ARE STORED IN A(1) TO A(N), THE ROOTS
C     THUS FAR BEING STORED IN THE ARRAYS REZ AND IMZ
C     STARTING WITH REZ(N)+I*IMZ(N).  AN IMMEDIATE RE-ENTRY
C     IS POSSIBLE WITH IFAIL UNCHANGED AND WITH A NEW
C     STARTING POINT FOR THE SEARCH HELD IN REZ(1)+IIMZ(1).
C     REF - J.I.M.A., VOL.8., PP122-129 (1971).
C     .. Parameters ..
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      character srname*6
      parameter (srname='C02AEF')
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Maybe Written ..
      integer n
C.. In/Out Status: Maybe Read, Maybe Written ..
      double precision a(n)
C.. In/Out Status: Maybe Read, Maybe Written ..
      double precision rez(n)
C.. In/Out Status: Maybe Read, Maybe Written ..
      double precision imz(n)
C.. In/Out Status: Read, Maybe Written ..
      double precision tol
C.. In/Out Status: Read, Overwritten ..
      integer ifail
C
C.. Local Scalars ..
      integer i,i2,ii,ind,jtemp,k,jj
      logical cbig,flag
      double precision a1p5,cmax,fac,four,fun,g,nfun,one,p1,p2z1,p3z2,
     $                 p4z1,p5,s,s1,s2,scale,sig,t,tol2,two,xxx,zero
C
C.. Local Arrays ..
      character p01rec(1)
      double precision b(100),c(100)
C
C.. External Functions ..
      integer P01ABF
      double precision X02AJF
      double precision X02ALF
      external P01ABF, X02AJF, X02ALF
C
C.. External Calls ..
      external C02AEZ,tartaglia
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
C
C.. Intrinsic Functions ..
      intrinsic ABS, DBLE, INT, LOG, SQRT
      include 'ac02ae.i'
C   LINES OF CODE ADDED FOR X-13A-S : 2
      DOUBLE PRECISION zzz
      include 'error.cmn'
C   END OF CODE BLOCK
C
C.. Data Declarations ..
C     .. Data statements ..
      data one/1.0d0/ a1p5/1.5d0/ zero/0.0d0/ p4z1/1.0d-5/
      data two/2.0d0/ p5/0.5d0/ p2z1/1.0d-3/ p1/0.1d0/
      data p3z2/2.0d-4/ four/4.0d0/
C     .. Executable Statements ..
      xxx = X02AJF()
      if (tol .lt. xxx) then
       tol = xxx
      end if
C     THE ABOVE TEST WAS ADDED AT 4.5 TO PREVENT TOL BEING TOO
C     SMALL
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      cmax = SQRT(X02ALF())
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
      ZZZ = X02ALF()
      CMAX = SQRT(ZZZ)
C   END OF CODE BLOCK      
      fac = one
      flag = ifail .eq. 2
      if (flag) then
       ifail = 1
      end if
      ind = 0
      tol2 = tol**a1p5
      if (
c     $(ABS(a(1)-zero).gt.1.d-15) .and. 
     $   (n.ge.2) .and. (n.le.100)) then
       do while (dpeq(a(n), 0.d0) .and. n.ge.2) 
        rez(n-1) = zero
        imz(n-1) = zero
        n = n - 1
       end do
       do while (.true.)
        scale = zero
        do i = 1,n
         if (ABS(a(i)) .ge. p4z1) then
          scale = scale + LOG(ABS(a(i)))
         end if
        end do
        k = INT(scale/(DBLE(n)*LOG(two))+p5)
        scale = two**(-k)
        do i = 1,n
         a(i) = a(i) * scale
         b(i) = a(i)
        end do
C     TEST FOR LOW ORDER POLYNOMIAL FOR EXPLICIT SOLUTION
        if (n .le. 3) then
         goto (5009,5005,5006) n
         goto 5000
 5005    rez(1) = -a(2)/a(1)*fac
         imz(1) = zero
         goto 5007
        end if
 5000   do 10 while (.true.)
         do i = 2,n
          ii = n - i + 2
          if (dpeq(b(ii), ZERO)) goto 5001
          t = b(1) / b(ii)
          if (ABS(t) .ge. one) goto 5001
          do k = 2,ii
           i2 = ii - k + 1
           c(k-1) = b(k) - t*b(i2)
          end do
          jtemp = ii - 1
          do k = 1,jtemp
           b(k) = c(k)
          end do
         end do
         fac = fac * two
         scale = one
         jj = n
         do while (.true.)
          jj = jj - 1
          if (jj .lt. 1) goto 10
          scale = scale * two
          a(jj) = a(jj) * scale
          b(jj) = a(jj)
         end do
 10     continue
 5001   if (.not. flag) then
         X = p2z1
         Y0 = p1
        else
         X = rez(1)
         Y0 = imz(1) + tol
         flag = .false.
        end if
        call C02AEZ(a,n,tol)
        fun = R*R + J*J
        do while (.true.)
         g = Rx*Rx + Jx*Jx
         if (g .lt. fun*tol2) goto 5008
         s1 = -(R*Rx+J*Jx)/g
         s2 = (R*Jx-J*Rx) / g
         sig = p3z2
         s = SQRT(s1*s1+s2*s2)
         if (s .gt. one) then
          s1 = s1 / s
          s2 = s2 / s
          sig = sig / s
         end if
C     VALID DIRECTION OF SEARCH HAS BEEN DETERMINED, NOW
C     PROCEED TO DETERMINE SUITABLE STEP
         X = X + s1
         Y0 = Y0 + s2
         do while (.true.)
          call C02AEZ(a,n,tol)
          if (Sat) goto 5003
          nfun = R*R + J*J
          if (fun-nfun .ge. sig*fun) goto 5002
          s1 = p5 * s1
          s2 = p5 * s2
          if (ABS(s1).le.xxx*ABS(X) .and. ABS(s2).le.xxx*ABS(Y0))
     $      goto 5008
          s = p5 * s
          sig = p5 * sig
          X = X - s1
          Y0 = Y0 - s2
         end do
 5002    fun = nfun
        end do
 5003   fun = one / tol2
        k = 0
        imz(n-1) = Y0 * fac
        if (ABS(Y0) .le. p1) then
C     CHECK POSSIBILITY OF REAL ROOT
         s1 = Y0
         Y0 = zero
         call C02AEZ(a,n,tol)
         Y0 = s1
         if (Sat) then
C     REAL ROOT ACCEPTED AND BOTH BACKWARD AND FORWARD DEFLATIONS
C     ARE PERFORMED WITH LINEAR FACTOR
          rez(n-1) = X * fac
          imz(n-1) = zero
          n = n - 1
          b(1) = a(1)
          c(n) = -a(n+1)/X
          cbig = .false.
          do 15 i = 2,n
           b(i) = a(i) + X*b(i-1)
           ii = n - i + 1
           if (.not. cbig) then
            c(ii) = (c(ii+1)-a(ii+1)) / X
            if (ABS(c(ii)) .le. cmax) goto 15
            cbig = .true.
           end if
           c(ii) = cmax
 15       continue
          goto 5004
         end if
        end if
C     COMPLEX ROOT ACCEPTED AND BOTH BACKWARD AND FORWARD
C     DEFLATIONS ARE PERFORMED WITH QUADRATIC FACTOR
        rez(n-1) = X * fac
        rez(n-2) = X * fac
        imz(n-2) = -imz(n-1)
        n = n - 2
        R = two * X
        J = -(X*X+Y0*Y0)
        b(1) = a(1)
        b(2) = a(2) + R*b(1)
        c(n) = -a(n+2)/J
        c(n-1) = -(a(n+1)+R*c(n))/J
        if (n .ne. 2) then
         cbig = .false.
         do 20 i = 3,n
          b(i) = a(i) + R*b(i-1) + J*b(i-2)
          ii = n - i + 1
          if (.not. cbig) then
           c(ii) = -(a(ii+2)-c(ii+2)+R*c(ii+1))/J
           if (ABS(c(ii)) .le. cmax) goto 20
           cbig = .true.
          end if
          c(ii) = cmax
 20      continue
        end if
C     MATCHING POINT FOR COMPOSITE DEFLATION
 5004   do i = 1,n
         nfun = ABS(b(i)) + ABS(c(i))
         if (nfun .gt. tol) then
          nfun = ABS(b(i)-c(i)) / nfun
          if (nfun .lt. fun) then
           fun = nfun
           k = i
          end if
         end if
        end do
        if (k .ne. 1) then
         jtemp = k - 1
         do i = 1,jtemp
          a(i) = b(i)
         end do
        end if
        if (k.ne.0) then
          a(k) = p5 * (b(k)+c(k))
        end if
        if (k .ne. n) then
         jtemp = k + 1
         do i = jtemp,n
          a(i) = c(i)
         end do
        end if
       end do
 5006  R = a(2)*a(2) - four*a(1)*a(3)
       if (R .gt. zero) then
        imz(1) = zero
        imz(2) = zero
        if (a(2) .lt. ZERO) then
         rez(1) = p5 * (-a(2)+SQRT(R)) / a(1) * fac
        else if (dpeq(a(2), ZERO)) then
         rez(1) = -p5*SQRT(R)/a(1)*fac
        else
         rez(1) = p5 * (-a(2)-SQRT(R)) / a(1) * fac
        end if
        rez(2) = a(3) / (rez(1)*a(1)) * fac * fac
       else
        rez(2) = -p5*a(2)/a(1)*fac
        rez(1) = rez(2)
        imz(2) = p5 * SQRT(-R) / a(1) * fac
        imz(1) = -imz(2)
       end if
 5007  n = 1
       goto 5009
 5008  ifail=1
c       ind = P01ABF(ifail,2,srname,0,p01rec) cc No queremos que se corte la ejecucion de Seats
C   LINES OF CODE ADDED FOR X-13A-S : 1
       IF(Lfatal)RETURN
C   END OF CODE BLOCK
       return
       scale = one
       i = n
       do while (.true.)
        i = i - 1
        if (i .lt. 1) goto 5009
        scale = scale * fac
        a(i) = a(i) / scale
       end do
      else
       ifail=1
       return
c       ind = P01ABF(ifail,1,srname,0,p01rec) cc no queremos que se corte la ejecucion de Seats
      end if
 5009 ifail = ind
      end
C
C
      subroutine C02AEZ(a,n,tol)
C     EVALUATES R,RX,J,JX AT THE POINT X+IY AND APPLIES THE ADAMS
C     TEST.
C     THE BOOLEAN VARIABLE SAT IS GIVEN THE VALUE TRUE IF THE TEST
C     IS SATISFIED.
C     .. Scalar Arguments ..
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Overwritten ..
      integer n
C.. In/Out Status: Maybe Read, Not Written ..
      double precision a(n)
C.. In/Out Status: Read, Not Written ..
      double precision tol
C
C.. Local Scalars ..
      integer k
      double precision a1,a2,a3,a8,b1,b2,b3,c,p,p8,q,t,ten,two,zero
C
C.. Intrinsic Functions ..
      intrinsic ABS, SQRT
      include 'ac02ae.i'
C
C.. Data Declarations ..
C     .. Data statements ..
      data two/2.0d0/ zero/0.0d0/ p8/0.8d0/ ten/1.0d1/ a8/8.0d0/
C     .. Executable Statements ..
      p = -two*X
      q = X*X + Y0*Y0
      t = SQRT(q)
      a2 = zero
      b2 = zero
      b1 = a(1)
      a1 = a(1)
      c = ABS(a1) * p8
      n = n - 2
      do k = 2,n
       a3 = a2
       a2 = a1
       a1 = a(k) - p*a2 - q*a3
       c = t*c + ABS(a1)
       b3 = b2
       b2 = b1
       b1 = a1 - p*b2 - q*b3
      end do
      n = n + 2
      a3 = a2
      a2 = a1
      a1 = a(n-1) - p*a2 - q*a3
      R = a(n) + X*a1 - q*a2
      J = a1 * Y0
      Rx = a1 - two*b2*Y0*Y0
      Jx = two * Y0 * (b1-X*b2)
      c = t*(t*c+ABS(a1)) + ABS(R)
      Sat = (SQRT(R*R+J*J)) .lt.
     $      ((ten*c-a8*(ABS(R)+ABS(a1)*t)+two*ABS(X*a1))*tol)
      end
C
C
      integer function P01ABF(ifail,ierror,srname,nrec,rec)
C
C     P01ABF either returns the value of IERROR through the routine
C     name (soft failure), or terminates execution of the program
C     (hard failure). Diagnostic messages may be output.
C
C     If IERROR = 0 (successful exit from the calling routine),
C     the value 0 is returned through the routine name, and no
C     message is output
C
C     If IERROR is non-zero (abnormal exit from the calling routine),
C     the action taken depends on the value of IFAIL.
C
C     IFAIL =  1: soft failure, silent exit (i.e. no messages are
C                 output)
C     IFAIL = -1: soft failure, noisy exit (i.e. messages are output)
C     IFAIL =-13: soft failure, noisy exit but standard messages from
C                 P01ABF are suppressed
C     IFAIL =  0: hard failure, noisy exit
C
C
C     a = 0: hard failure  a = 1: soft failure
C     b = 0: silent exit   b = 1: noisy exit
C
C     except that hard failure now always implies a noisy exit.
C
C
C     .. Scalar Arguments ..
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      integer ifail
C.. In/Out Status: Read, Not Written ..
      integer ierror
C.. In/Out Status: Not Read, Not Written ..
      character*(*) srname
C.. In/Out Status: Maybe Read, Not Written ..
      integer nrec
C.. In/Out Status: Maybe Read, Not Written ..
      character*(*) rec(*)
C
C.. Local Scalars ..
      integer i,nerr
      character mess*72
C
C.. External Calls ..
      external P01ABZ, X04AAF, X04BAF
C
C.. Intrinsic Functions ..
      intrinsic ABS, MOD
      include 'stream.i'
C   LINES OF CODE ADDED FOR X-13A-S : 1
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
      nerr = Nio
      if (ierror .ne. 0) then
C        Abnormal exit from calling routine
       if (ifail.eq.-1 .or. ifail.eq.0 .or. ifail.eq.-13 .or.
     $     (ifail.gt.0.and.MOD(ifail/10,10).ne.0)) then
C           Noisy exit
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C        call X04AAF(1,nerr)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
        nerr = STDERR
C   END OF CODE BLOCK
        do i = 1,nrec
         call X04BAF(nerr,rec(i))
C   LINES OF CODE ADDED FOR X-13A-S : 1
         call X04BAF(Mt2,rec(i))
C   END OF CODE BLOCK
        end do
        if (ifail .ne. -13) then
C
 7000    format (
     $   ' ** ABNORMAL EXIT from RPQ                 ',a,': IFAIL',' ='
     $   ,i6)
         write (mess,FMT = 7000) 'RPQ', ierror
         call X04BAF(nerr,mess)
C   LINES OF CODE ADDED FOR X-13A-S : 1
         call X04BAF(Mt2,mess)
C   END OF CODE BLOCK
         if (ABS(MOD(ifail,10)) .ne. 1) then
C                 Hard failure
          call X04BAF(nerr,' ** RPQ hard failure - execution terminated'
     $               )
C   LINES OF CODE ADDED FOR X-13A-S : 1
          call X04BAF(Mt2,' ** RPQ hard failure - execution terminated')
C   END OF CODE BLOCK
          call P01ABZ
         else
C                 Soft failure
          call X04BAF(nerr,' ** RPQ soft failure - control returned')
C   LINES OF CODE ADDED FOR X-13A-S : 1
          call X04BAF(Mt2,' ** RPQ soft failure - control returned')
C   END OF CODE BLOCK
         end if
        end if
       end if
      end if
      P01ABF = ierror
      end
C
C
      subroutine P01ABZ
C
C.. Implicits ..
      implicit none
C
C ... Executable Statements ...
C
C   LINES OF CODE ADDED FOR X-13A-S : 2
      call abend()
      return
C   END OF CODE BLOCK
*      call RAISE
*      stop
      end
C
C
      double precision function X02AJF()
C
C     RETURNS  (1/2)*B**(1-P)  IF ROUNDS IS .TRUE.
C     RETURNS  B**(1-P)  OTHERWISE
C
C     For Prime: X02AJF = 2.0D0**(-45) = 2.842170943040D-14
C
C
C.. Implicits ..
      implicit none
C
C.. Local Scalars ..
      double precision z
C
C.. Local Arrays ..
      integer*2 l(4)
C
C.. Equivalences ..
      equivalence (z,l(1))
C   LINES OF CODE ADDED FOR X-13A-S : 2
      DOUBLE PRECISION dpmpar
      EXTERNAL dpmpar
C   END OF CODE BLOCK
C
C.. Data Declarations ..
C      DATA L(1),L(2),L(3),L(4)/:040000,:000000,:000000,:000124/
      data l(1),l(2),l(3),l(4)/16384,0,0,84/
C     .. Executable Statements ..
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      X02AJF = z
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      X02AJF = dpmpar(1)
C   END OF CODE BLOCK
      end
C
C
      subroutine X04AAF(i,nerr)
C
C     IF I = 0, SETS NERR TO CURRENT ERROR MESSAGE UNIT NUMBER
C     (STORED IN NERR1).
C     IF I = 1, CHANGES CURRENT ERROR  MESSAGE UNIT NUMBER TO
C     VALUE SPECIFIED BY NERR.
C
C     .. Scalar Arguments ..
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer i
C.. In/Out Status: Maybe Read, Maybe Written ..
      integer nerr
C
C.. Local Scalars ..
      integer nerr1
C
C.. Save Declarations ..
      save nerr1
C
C.. Data Declarations ..
C     .. Data statements ..
      data nerr1/1/
C     .. Executable Statements ..
      if (i .eq. 0) then
       nerr = nerr1
      end if
      if (i .eq. 1) then
       nerr1 = nerr
      end if
      end
C
C
      subroutine X04BAF(nout,rec)
C
C     X04BAF writes the contents of REC to the unit defined by NOUT.
C
C     Trailing blanks are not output, except that if REC is entirely
C     blank, a single blank character is output.
C     If NOUT.lt.0, i.e. if NOUT is not a valid Fortran unit identifier,
C     then no output occurs.
C
C     .. Scalar Arguments ..
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer nout
C.. In/Out Status: Maybe Read, Not Written ..
      character*(*) rec
C
C.. Local Scalars ..
      integer i
C
C.. Intrinsic Functions ..
      intrinsic LEN
C     .. Executable Statements ..
      if (nout .lt. 0) return
C        Remove trailing blanks
      do i = LEN(rec),2,-1
       if (rec(i:i) .ne. ' ') goto 5000
      end do
C
 7000 format (a)
C        Write record to external file
 5000 write (nout,FMT = 7000) rec(1:i)
      end
C
C
      double precision function X02ALF()
C
C     RETURNS  (1 - B**(-P)) * B**EMAX  (THE LARGEST POSITIVE MODEL
C     NUMBER)
C
C
C
C.. Implicits ..
      implicit none
C   LINES OF CODE COMMENTED FOR X-13A-S : 13
CC
CC.. Local Scalars ..
C      double precision z
CC
CC.. Local Arrays ..
C      integer*2 l(4)
CC
CC.. Equivalences ..
C      equivalence (l(1),z)
CC
CC.. Data Declarations ..
CC      DATA L(1),L(2),L(3),L(4)/:077777,:177777,:177776,:040301/
C      data l(1),l(2),l(3),l(4)/32767,65535,65534,16577/
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
      DOUBLE PRECISION dpmpar
      EXTERNAL dpmpar
C   END OF CODE BLOCK
C     .. Executable Statements ..
c      X02ALF = z
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      X02ALF = dpmpar(3)
C   END OF CODE BLOCK
      end
C
C
C
CC      SUBROUTINE TABLE(DATA,DECP)
CC      IMPLICIT REAL*8 (A-H,O-Z)
CC---
CC-
CC EXPLORE
CC
CC     REAL*4 MTH(12),QRT(4),SRT(6)
CC     REAL*8 DATA(1),FDECP1(7),WRT2(7),FN1(12),FN2(12),FNFREQ(3)
C      CHARACTER*4 MTH(12),QRT(4),SRT(6)
C      REAL*8 DATA(1)
C      CHARACTER*8 FDECP1(7),WRT2(7),FN1(12),FN2(12),FNFREQ(3)
C      INTEGER*4 DECP,YR
C      COMMON/SFORM/NZ,NYER,NPER,NFREQ
C       COMMON /STREAM/ NIO
C      DATA MTH/'JAN ','FEB ','MAR ','APR ','MAY ','JUN','JUL','AUG ',
C     $'SEP','OCT ','NOV ','DEC '/
C      DATA QRT/'1ST','2ND','3RD','4TH'/
C      DATA SRT/'1ST','2ND','3RD','4TH','5TH','6TH'/
C      DATA WRT2/'(1H ,I4,','N2','X,','N1','(F10','.DECP','))'/
C      DATA FDECP1/'.0','.1','.2','.3','.4','.5','.6'/
C      DATA FN1/'1','2','3','4','5','6','7','8','9','10','11','12'/
C      DATA FN2/'2','12','22','32','42','52','62','72','82','092','102',
C     $        '112'/
C      DATA FNFREQ/'4','12','6'/
C 2001 FORMAT(/,1H ,'YEAR',1X,12(6X,A4)/)
C 2002 FORMAT(/,1H ,'YEAR',1X,4(6X,A4)/)
C 2003 FORMAT(/,1H ,'YEAR',1X,6(6X,A4)/)
C      NDECP=DECP
C      IF (DECP.GE.6) DECP=6
C  250 IF (DECP.EQ.0) GO TO 251
C      MDECP=10-DECP
C      A=0.00999999*10**MDECP
C      DO 151 I=1,NZ
C      IF(DATA(I).LT.A) GO TO 151
C      DECP=DECP-1
C      GO TO 250
C  151 CONTINUE
C  251 CONTINUE
C       IFACT=0
C       ZZ=DLOG10(DABS(DATA(1)+.0000000001D0))
C       SUM=DABS(ZZ)
C       DO 678 I=2,NZ
C       IF (ZZ.GT.ZERO) THEN
C       SUM=ZERO
C       GOTO 679
C       end if
C       ZZ=DLOG10(DABS(DATA(I)+.0000000001D0))
C       IF ((DABS(ZZ).LT.SUM).AND.(ZZ.LT.ZERO)) SUM=DABS(ZZ)
C 678  CONTINUE
C 679  IF (SUM.GT.ONE) THEN
C        IFACT=IDINT(SUM)
C        IF (IFACT.GT.6) IFACT=6
C        IF (IFACT.GT.0) WRITE(NIO,'(4X, ''X  10.0D'',I2,/)') -IFACT
C      end if
C       JFACT=0
C       ZZ=DLOG10(DABS(DATA(1)+.0000000001D0))
C       SUM=ZZ
C       DO 878 I=2,NZ
C       ZZ=DLOG10(DABS(DATA(I)+.0000000001D0))
C       IF ((ZZ.GT.SUM).AND.(ZZ.GT.ZERO)) SUM=ZZ
C 878  CONTINUE
C      IF (SUM.GT.4.0D0) THEN
C        JFACT=IDINT(SUM)-2
C        IF (JFACT.GT.0) WRITE(NIO,'(4X, ''X  10.0D'',I2,/)') JFACT
C      end if
C      YR=NYER
C      IF (NFREQ.EQ.12) WRITE(NIO,2001) (MTH(I),I=1,12)
C      IF (NFREQ.EQ.4) WRITE(NIO,2002) (QRT(I),I=1,4)
C      IF (NFREQ.EQ.6) WRITE(NIO,2003) (SRT(I),I=1,6)
C      NYR=(NZ-(NFREQ-NPER+1))/NFREQ
C      NY=(NZ-(NFREQ-NPER+1))-NYR*NFREQ
C      IF (NY.EQ.0) GO TO 520
C      NYR=NYR+1
C  520 NYR=NYR+1
C      WRT2(6)=FDECP1(DECP+1)
C      DO 75 I=1,NYR
C      I1=(I-1)*NFREQ-(NPER-2)
C      I2=I*NFREQ-(NPER-1)
C      IF (I2.GE.NZ) I2=NZ
C      WRT2(2)=FN2(1)
C      WRT2(4)=FNFREQ(1)
C      IF (NFREQ.EQ.12) WRT2(4)=FNFREQ(2)
C      IF (NFREQ.EQ.6) WRT2(4)=FNFREQ(3)
C      IF (I.NE.1) GO TO 150
C      WRT2(4)=FN1(NFREQ-NPER+1)
C      WRT2(2)=FN2(NPER)
C      I1=1
C  150 IF (IFACT.GT.0) THEN
C         WRITE(NIO,WRT2)YR,(DATA(J)*(10.0D0**IFACT),J=I1,I2)
C      ELSE
C         WRITE(NIO,WRT2)YR,(DATA(J)*(10.0D0**(-JFACT)),J=I1,I2)
C      end if
C   75 YR=YR+1
C      DECP=NDECP
C      RETURN
C      END
C
