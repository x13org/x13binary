      SUBROUTINE BenchMark(Series,Stci,Stci2,Lfda,Llda,Ny,Iter,
     &                     Title,nfperiod,nfyear)
C
C Arguments
C
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer Lfda,Llda,Ny,Iter,Nfyear,Nfperiod
      Real*8 Series(*),Stci(*),Stci2(*)
      character*80 title
c Local scalar        
      integer ntitle
      character fname*12,subtitle*50
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
C
C.. External Calls ..
      include 'calfor.i'
      include 'sform.i'
      include 'dirs.i'
      include 'titl.i'
cc
c
cc
      if ((Ny.eq.4) .or. (Ny.eq.12)) then 
       Mq=Ny
       NFREQ=Ny
       NPER=Nfperiod
       NYER=Nfyear
       TitleG=title
       call qmap2(Series,Stci,Stci2,Lfda,Llda,Ny,0)
       ntitle=istrlen(title)
*       if (iter.ne.0)  then
*        fname = title(1:ntitle) // '.SAY'
*        subtitle = 'FINAL SA SERIES WITH REVISED YEARLY'
*        call PLOTSERIES(fname,subtitle,Stci2,Llda,1,0.0d0)
*        call AddList(fname)
*       else
*        fname = 'FSAYFIN.T'
*        subtitle = 'FINAL SA SERIES WITH REVISED YEARLY'
*        call PLOTSERIES(fname,subtitle,Stci2,Llda,1,0.0d0)
*       end if
      end if
      return
      end
cc
c
cc
      subroutine AddList(fname)
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      character fname*12
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
c
      include 'dirs.i'
cdos      close(17)
cdos
cdos      open (17,FILE=Graphdir(1:ISTRLEN(GraphDir)) // 
cdos     $                      '\\series\\graph.lst',
cdos     $                   status='old',access='append')
cunix
cunix      open (17,FILE=Graphdir(1:ISTRLEN(GraphDir)) // 
cunix     $                      '/series/graph.lst',
cunix     $                   status='old',access='append')
cunix      write (17,'(A)') fname
cunix      close(17)
      return
      end
cc
c
*cc
***==aa0001.f    processed by SPAG 6.05Fc at 11:55 on  4 Oct 2004
*      SUBROUTINE qmap2(Series,Stci,Stci2,Lfda,Llda,Ny,Iagr)
*      IMPLICIT NONE
***--AA00013
*C
*C*** Start of declarations rewritten by SPAG
*c     ------------------------------------------------------------------
*c-----------------------------------------------------------------------
*c    PLEN is the integer PARAMETER  for the maximum length of a
*c series.
*c-----------------------------------------------------------------------
*c Name  Type Description
*c-----------------------------------------------------------------------
*c PFCST   i  PARAMETER for the maximum number of forecasts
*c POBS    i  PARAMETER for the maximum length of the series 
*c PLEN    i  PARAMETER for the maximum length of the series + back and
*c             forecasts
*c PYRS    i  PARAMETER for the maximum number of years in the series +
*c             back and forecasts
*c PYR1    i  PARAMETER for the maximum number of years in the series 
*c PTD     i  PARAMETER for the number of types of trading day factors
*c             (based on lenght of month, starting period)
*c     PSP - maximum length of seasonal period  (formerly in model.cmn)
*c-----------------------------------------------------------------------
*      INTEGER POBS,PLEN,PFCST,PYR1,PYRS,PSRSCR,PTD,PSP
*      PARAMETER(PSP=12,PFCST=10*PSP,PYR1=65,POBS=PYR1*PSP,PYRS=PYR1+10,
*     &          PLEN=POBS+(2*PFCST),PSRSCR=79,PTD=28)
*c      INCLUDE 'force.i'
*c     ------------------------------------------------------------------
*      DOUBLE PRECISION ONE,ZERO,MONE
*      LOGICAL F
*      PARAMETER(F=.false.,ONE=1D0,ZERO=0D0,MONE=-1D0)
*C
*C Arguments
*C
*      DOUBLE PRECISION Series,Stci,Stci2
*      INTEGER Lfda,Llda,Ny
*      DIMENSION Series(*),Stci(*),Stci2(*)
*C
*C Local variables
*C
*      REAL*8 and11(PYRS),ansum(PYRS),ttf,cratio(PLEN),rratio(PLEN),
*     &       delta(PLEN,PLEN),deltapi(PLEN,PLEN),det,esp,r2(PLEN,PYRS),
*     &       rtz(PLEN),wcomp1(PLEN,PLEN),wcomp2(PLEN,PLEN),
*     &       wcomp3(PLEN,PLEN),xa(PLEN,1),xd(PLEN,1),xx(PLEN,1)
*      DOUBLE PRECISION DABS
*      INTEGER*4 i,j,k,kk,kkt,kmq,naly,
*     &          np,npnp
*      INTEGER knpn,npn1,Iagr,ns
*      REAL*8 mx1(PYRS,1),mx2(PYRS,1),mx3(PYRS,1)
*      REAL*8 Cmat(PLEN,PLEN),Omec(PLEN,PLEN),R1(PLEN,PYRS),
*     &       Tmx1(PLEN,PLEN),Ttmat(PLEN,PLEN),Ttmat2(PYRS,PLEN)
*      REAL*8 Invr(PYRS,PYRS),Jmat(PYRS,PLEN),Jmatpi(PLEN,PYRS)
*C
*      DOUBLE PRECISION SIMUL
*      EXTERNAL SIMUL
*      INCLUDE 'force.i'
*C
*      np=Llda-Lfda+1
**      ngrid=Ny
*C
**      WRITE (*,'('' NO. OF POINTS   '',I4)') np
*      Begyrt=1
*      ns=Begyrt-Lfda+1
*      DO WHILE (ns.le.0)
*       ns=ns+ny
*      END DO
*      naly=(np-ns+1)/Ny
*      npnp=naly*Ny
**      ne=ns+npnp-1
**      write(6,*) 'stpos, naly, npnp = ',ns, naly, npnp
*C CONSTRUCTION OF MATRIX J AND J PI
*      DO i=1,naly
*       DO j=1,np
*        Jmat(i,j)=ZERO
*       END DO
*      END DO
*      k=ns
*      DO i=1,naly
*       kmq=k+Ny-1
*       DO j=k,kmq
*        Jmat(i,j)=ONE
*       END DO
*       k=kmq+1
*      END DO
*      DO i=1,naly
*       DO j=1,np
*        Jmatpi(j,i)=Jmat(i,j)
*       END DO
*      END DO
*      DO j=1,np
*       xx(j,1)=Series(j+Lfda-1)
*       xa(j,1)=Stci(j+Lfda-1)
*      END DO
*C CONSTRUCTION OF MATRIX C
*c TFF added by Statstics Canada, March 2006 
*C ***************       modify factor TTF                   *****************
*      TTF = 0.0D0
*      DO I =1, NP
*            TTF = TTF + DABS(XA(I,1))
*      END DO
*      TTF = TTF/NP
*      DO i=1,np
*       DO j=1,np
*        Cmat(i,j)=ZERO
*       END DO
*      END DO
*c TFF used to modifiy CMAT by Statstics Canada, March 2006 
*      DO i=1,np
*       CMAT(I,I) = DABS(XA(I,1)/TTF)**LAMDA
*      END DO
*C      ******************************************************
*      IF (rol.LE.0.99999D00) THEN
*C CONSTRUCTION OF MATRIX OMECA OMEC
*       IF (rol.LT.1.0D-10) THEN
*        DO i=1,np
*         DO j=1,np
*          Omec(i,j)=ZERO
*         END DO
*        END DO
*        DO i=1,np
*         Omec(i,i)=ONE
*        END DO
*       ELSE
*        DO i=1,np
*         DO j=1,np
*          k=ABS(i-j)
*          Omec(i,j)=rol**k
*         END DO
*        END DO
*       END IF
*       kk=PLEN
*       kkt=PYRS
*       CALL MATMLT(Cmat,Omec,Ttmat,np,np,np,kk,kk,kk)
*       CALL MATMLT(Ttmat,Cmat,Tmx1,np,np,np,kk,kk,kk)
*       CALL MATMLT(Tmx1,Jmatpi,R1,np,np,naly,kk,kk,kk)
*       CALL MATMLT(Jmat,Tmx1,Ttmat2,naly,np,np,kkt,kk,kkt)
*       CALL MATMLT(Ttmat2,Jmatpi,Invr,naly,np,naly,kkt,kk,kkt)
*       esp=1.0D-20
*       det=SIMUL(naly,Invr,ansum,esp,-1,kkt)
*       CALL MATMLT(R1,Invr,r2,np,naly,naly,kk,kkt,kk) 
*       CALL MATMLT(Jmat,xx,mx1,naly,np,1,kkt,kk,kkt)
*       CALL MATMLT(Jmat,xa,mx2,naly,np,1,kkt,kk,kkt)
*       CALL ADD_SUB(mx1,mx2,mx3,naly,1,kkt,0)
*       CALL MATMLT(r2,mx3,xd,np,naly,1,kk,kkt,kk)
*       CALL ADD_SUB(xa,xd,xx,np,1,kk,1)
*       DO j=1,np
*        Stci2(j+Lfda-1)=xx(j,1)
*       END DO
*      ELSE
*C
*c inverse of CMAT computed by Statstics Canada, March 2006 
*C     ***************************************************************
*C     ***************** FIND THE INVERSE OF CMAT ********************
*C     ***************************************************************
*       DO I = 1, NP
*        CMAT(I,I) = ONE/CMAT(I,I)
*       END DO
*C     ****************************************************************
*C       CONSTRUCTION OF MATRIX DELTA
*       npn1=np-1
*       DO i=1,np
*        DO j=1,np
*         delta(i,j)=ZERO
*        END DO
*       END DO
*       DO i=1,npn1
*        delta(i,i)=MONE
*        delta(i,i+1)=ONE
*       END DO
*       DO i=1,npn1
*        DO j=1,np
*         deltapi(j,i)=delta(i,j)
*        END DO
*       END DO
*C CONSTRUCTION OF MATRIX OMECA OMEC
*       kk=PLEN
*       kkt=PYRS
*       CALL MATMLT(deltapi,delta,Ttmat,np,npn1,np,kk,kk,kk)
*       CALL MATMLT(Cmat,Ttmat,Omec,np,np,np,kk,kk,kk)
*       CALL MATMLT(Omec,Cmat,Tmx1,np,np,np,kk,kk,kk)
*C      TMX1 = C*DEL'*DEL*C,   A T by T square matrix.
*C     Construction of the big matrix.
*       knpn=np+naly
*       DO i=1,knpn
*        DO j=1,knpn
*         wcomp1(i,j)=ZERO
*        END DO
*       END DO
*       DO i=1,np
*        DO j=1,np
*         wcomp1(i,j)=Tmx1(i,j)
*        END DO
*       END DO
*       DO i=1,np
*        DO j=1,naly
*         wcomp1(i,np+j)=Jmatpi(i,j)
*        END DO
*       END DO
*       DO i=1,naly
*        DO j=1,np
*         wcomp1(np+i,j)=Jmat(i,j)
*        END DO
*       END DO
*       DO i=1,knpn
*        DO j=1,knpn
*         wcomp2(i,j)=ZERO
*        END DO
*       END DO
*       DO i=1,np
*        DO j=1,np
*         wcomp2(i,j)=Tmx1(i,j)
*        END DO
*       END DO
*       DO i=1,naly
*        wcomp2(np+i,np+i)=ONE
*       END DO
*       DO i=1,naly
*        DO j=1,np
*         wcomp2(np+i,j)=Jmat(i,j)
*        END DO
*       END DO
*C     Find the inverse of WCOMP1
*       esp=1.0D-10
*       det=SIMUL(knpn,wcomp1,rratio,esp,-1,kk)
*       CALL MATMLT(wcomp1,wcomp2,wcomp3,knpn,knpn,knpn,kk,kk,kk)
*C     R2 IS A SUBMATRIX OF WCOMP3
*       DO i=1,np
*        DO j=1,naly
*         r2(i,j)=wcomp3(i,j+np)
*        END DO
*       END DO
*       DO j=1,np
*        xx(j,1)=Series(j+Lfda-1)
*        xa(j,1)=Stci(j+Lfda-1)
*       END DO
*       CALL MATMLT(Jmat,xx,mx1,naly,np,1,kkt,kk,kkt)
*       CALL MATMLT(Jmat,xa,mx2,naly,np,1,kkt,kk,kkt)
*       CALL ADD_SUB(mx1,mx2,mx3,naly,1,kkt,0)
*       CALL MATMLT(r2,mx3,xd,np,naly,1,kk,kkt,kk)
*       CALL ADD_SUB(xa,xd,xx,np,1,kk,1)
*       DO j=1,np
*        Stci2(j+Lfda-1)=xx(j,1)
*       END DO
*      END IF
*C     ***********   ADDITION NEW OUTPUT    **************
*      DO j=1,naly
*       ansum(j)=mx1(j,1)
*       and11(j)=mx2(j,1)
*      END DO
*      IF (Mid.EQ.0) THEN
*       DO j=1,np
*        cratio(j)=Stci2(j+Lfda-1)/Stci(j+Lfda-1)-ONE
*        rratio(j)=ZERO
*       END DO
*      ELSE
*       DO j=1,np
*        cratio(j)=Stci2(j+Lfda-1)-Stci(j+Lfda-1)
*        rratio(j)=ZERO
*       END DO
*      END IF
*      CALL MEANCRA(ansum,and11,rtz,Mid,Ny,naly)
*      npnp=naly*Ny
*      DO j=1,npnp
*       rratio(j+ns-1)=rtz(j)
*      END DO
*c      IF(Iagr.eq.4)THEN
*c       IF(Savtab(LCPCRI))CALL punch(cratio,Lfda,Llda,LCPCRI,F,F)
*c       IF(.not.Lfatal.and.Savtab(LCPRRI))
*c     &    CALL punch(rratio,Lfda,Llda,LCPRRI,F,F)
*c      ELSE
*c       IF(Savtab(LFRCCR))CALL punch(cratio,Lfda,Llda,LFRCCR,F,F)
*c       IF(.not.Lfatal.and.Savtab(LFRCRR))
*c     &    CALL punch(rratio,Lfda,Llda,LFRCRR,F,F)
*c      END IF
*      RETURN
*      END
***==mult.f    processed by SPAG 6.05Fc at 12:31 on 12 Oct 2004
*      SUBROUTINE MATMLT(A,B,C,M,Ip,Iq,Ia,Ib,Ic)
*      IMPLICIT NONE
***--MULT5
*C
*C*** Start of declarations rewritten by SPAG
*C
*C Dummy arguments
*C
*      INTEGER*4 Ia,Ib,Ic,Ip,Iq,M
*      REAL*8 A(Ia,*),B(Ib,*),C(Ic,*)
*C
*C Local variables
*C
*      INTEGER*4 i,ir,is
*      REAL*8 sum
*C
*C*** End of declarations rewritten by SPAG
*C
*c ****  Start of Executable Program                                     
*C      a(m,p)*b(p,q) = c(m,q)
*      DO ir=1,M
*       DO is=1,Iq
*        sum=0.0D0
*        DO i=1,Ip
*         sum=sum+(A(ir,i)*B(i,is))
*        END DO
*        C(ir,is)=sum
*       END DO
*      END DO
*      END
***==simul.f    processed by SPAG 6.05Fc at 12:31 on 12 Oct 2004
*      DOUBLE PRECISION FUNCTION SIMUL(N,A,X,Eps,Indic,Ia)
*      IMPLICIT NONE
***--SIMUL7
*C
*C*** Start of declarations rewritten by SPAG
*      INCLUDE 'srslen.i'
*C
*C Dummy arguments
*C
*      REAL*8 Eps
*      INTEGER Ia,Indic,N
*      REAL*8 A(Ia,*),X(N)
*C
*C Local variables
*C
*      REAL*8 aijck,deter,pivot,y(PLEN)
*      DOUBLE PRECISION DABS,DBLE
*      INTEGER i,intch,ip1,irowi,irowj,irowk,iscan,j,jcoli,jcolj,jcolk,
*     &        jscan,jtemp,k,km1,imax,nm1,INT
*      REAL*8 irow(PLEN),jcol(PLEN),jord(PLEN)
*      LOGICAL dpeq
*      EXTERNAL dpeq
*C
*C*** End of declarations rewritten by SPAG
*C
*c ****  Start of Executable Program                                     
*      imax=N
*      DO i=1,N
*       irow(i)=0D0
*       jcol(i)=0D0
*      END DO
*      IF (Indic.GE.0) imax=N+1
*      IF (N.LE.396) THEN
*       deter=1.0D0
*       DO k=1,N
*        km1=k-1
*        pivot=0.0D0
*        DO i=1,N
*         DO j=1,N
*          IF (k.NE.1) THEN
*           DO iscan=1,km1
*            DO jscan=1,km1
*             IF (dpeq(DBLE(i),irow(iscan))) GO TO 10
*             IF (dpeq(DBLE(j),jcol(jscan))) GO TO 10
*            END DO
*           END DO
*          END IF
*          IF (DABS(A(i,j)).GT.DABS(pivot)) THEN
*           pivot=A(i,j)
*           irow(k)=DBLE(i)
*           jcol(k)=DBLE(j)
*          END IF
*   10    END DO
*        END DO
*        IF (DABS(pivot).GT.Eps) THEN
*         irowk=INT(irow(k))
*         jcolk=INT(jcol(k))
*         deter=deter*pivot
*         DO j=1,imax
*          A(irowk,j)=A(irowk,j)/pivot
*         END DO
*         A(irowk,jcolk)=1.0D0/pivot
*         DO i=1,N
*          aijck=A(i,jcolk)
*          IF (i.NE.irowk) THEN
*           A(i,jcolk)=-aijck/pivot
*           DO j=1,imax
*            IF (j.NE.jcolk) A(i,j)=A(i,j)-aijck*A(irowk,j)
*           END DO
*          END IF
*         END DO
*        ELSE
*         SIMUL=0.0D0
*         RETURN
*        END IF
*       END DO
*       DO i=1,N
*        irowi=INT(irow(i))
*        jcoli=INT(jcol(i))
*        jord(irowi)=jcol(i)
*        IF (Indic.GE.0) X(jcoli)=A(irowi,imax)
*       END DO
*       intch=0
*       nm1=N-1
*       DO i=1,nm1
*        ip1=i+1
*        DO j=ip1,N
*         IF (jord(j).LT.jord(i)) THEN
*          jtemp=INT(jord(j))
*          jord(j)=jord(i)
*          jord(i)=DBLE(jtemp)
*          intch=intch+1
*         END IF
*        END DO
*       END DO
*       IF (intch/2*2.NE.intch) deter=-deter
*       IF (Indic.LE.0) THEN
*        DO j=1,N
*         DO i=1,N
*          irowi=INT(irow(i))
*          jcoli=INT(jcol(i))
*          y(jcoli)=A(irowi,j)
*         END DO
*         DO i=1,N
*          A(i,j)=y(i)
*         END DO
*        END DO
*        DO i=1,N
*         DO j=1,N
*          irowj=INT(irow(j))
*          jcolj=INT(jcol(j))
*          y(irowj)=A(i,jcolj)
*         END DO
*         DO j=1,N
*          A(i,j)=y(j)
*         END DO
*        END DO
*        SIMUL=deter
*        RETURN
*       END IF
*      ELSE
*c       WRITE (6,1010)
*c 1010  FORMAT ('ON TOO BIG')
*       SIMUL=0.0D0
*       RETURN
*      END IF
*      SIMUL=deter
*      RETURN
*      END
***==addsub.f    processed by SPAG 6.05Fc at 12:31 on 12 Oct 2004
*      SUBROUTINE ADD_SUB(A,B,C,N,M,Id,Ind)
*      IMPLICIT NONE
***--ADDSUB6
*C
*C*** Start of declarations rewritten by SPAG
*C
*C Dummy arguments
*C
*      INTEGER Id,Ind,M,N
*      REAL*8 A(Id,*),B(Id,*),C(Id,*)
*C
*C Local variables
*C
*      INTEGER i,j
*C
*C*** End of declarations rewritten by SPAG
*C
*c ****  Start of Executable Program                                     
*C      INTEGER*4 N,M,ID,IND
*      DO i=1,N
*       DO j=1,M
*        IF (Ind.GT.0) THEN
*         C(i,j)=A(i,j)+B(i,j)
*        ELSE
*         C(i,j)=A(i,j)-B(i,j)
*        END IF
*       END DO
*      END DO
*      END
***==meancra.f    processed by SPAG 6.05Fc at 12:31 on 12 Oct 2004
*      SUBROUTINE MEANCRA(A1x,Aty,Rtz,Modlid,Mq,Ny)
*      IMPLICIT NONE
***--MEANCRA5
*C
*C*** Start of declarations rewritten by SPAG
*C
*C Dummy arguments
*C
*      INTEGER Modlid,Mq,Ny
*      REAL*8 A1x(*),Aty(*),Rtz(*)
*C
*C Local variables
*C
*      INTEGER i,j,k
*      REAL*8 tt
*C
*C*** End of declarations rewritten by SPAG
*C
*C   -----------  ATY IS D11
*c ****  Start of Executable Program                                     
*      IF (Modlid.EQ.0) THEN
*       DO i=1,Ny
*        tt=A1x(i)/Aty(i)-1D0
*        k=Mq*(i-1)
*        DO j=1,Mq
*         Rtz(k+j)=tt
*        END DO
*       END DO
*      ELSE
*       DO i=1,Ny
*        tt=A1x(i)-Aty(i)
*        k=Mq*(i-1)
*        DO j=1,Mq
*         Rtz(k+j)=tt/Mq
*        END DO
*       END DO
*      END IF
*      END
      