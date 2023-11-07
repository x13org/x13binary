**==aa0001.f    processed by SPAG 6.05Fc at 11:55 on  4 Oct 2004
      SUBROUTINE qmap2(Series,Stci,Stci2,Lfda,Llda,Ny,Iagr)
      IMPLICIT NONE
**--AA00013
C
C*** Start of declarations rewritten by SPAG
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'force.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'frctbl.i'
      INCLUDE 'cmptbl.i'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO,MONE
      LOGICAL F
      PARAMETER(F=.false.,ONE=1D0,ZERO=0D0,MONE=-1D0)
C
C Arguments
C
      DOUBLE PRECISION Series,Stci,Stci2
      INTEGER Lfda,Llda,Ny
      DIMENSION Series(*),Stci(*),Stci2(*)
C
C Local variables
C
      REAL*8 and11(PYRS),ansum(PYRS),ttf,cratio(PLEN),rratio(PLEN),
     &       delta(PLEN,PLEN),deltapi(PLEN,PLEN),det,esp,r2(PLEN,PYRS),
     &       rtz(PLEN),wcomp1(PLEN,PLEN),wcomp2(PLEN,PLEN),
     &       wcomp3(PLEN,PLEN),xa(PLEN,1),xd(PLEN,1),xx(PLEN,1)
      DOUBLE PRECISION DABS
      INTEGER*4 i,j,k,kk,kkt,kmq,naly,
     &          np,npnp
      INTEGER knpn,npn1,Iagr,ns
      REAL*8 mx1(PYRS,1),mx2(PYRS,1),mx3(PYRS,1)
      REAL*8 Cmat(PLEN,PLEN),Omec(PLEN,PLEN),R1(PLEN,PYRS),
     &       Tmx1(PLEN,PLEN),Ttmat(PLEN,PLEN),Ttmat2(PYRS,PLEN)
      REAL*8 Invr(PYRS,PYRS),Jmat(PYRS,PLEN),Jmatpi(PLEN,PYRS)
     
C
      DOUBLE PRECISION SIMUL
      EXTERNAL SIMUL
C
      np=Llda-Lfda+1
*      ngrid=Ny
C
 
*      WRITE (*,'('' NO. OF POINTS   '',I4)') np
 
      ns=Begyrt-Lfda+1
      DO WHILE (ns.le.0)
       ns=ns+ny
      END DO
     
      naly=(np-ns+1)/Ny
      npnp=naly*Ny
*      ne=ns+npnp-1
*      write(6,*) 'stpos, naly, npnp = ',ns, naly, npnp
 
C CONSTRUCTION OF MATRIX J AND J PI
 
      DO i=1,naly
       DO j=1,np
        Jmat(i,j)=ZERO
       END DO
      END DO
 
      k=ns
      DO i=1,naly
       kmq=k+Ny-1
       DO j=k,kmq
        Jmat(i,j)=ONE
       END DO
       k=kmq+1
      END DO
 
      DO i=1,naly
       DO j=1,np
        Jmatpi(j,i)=Jmat(i,j)
       END DO
      END DO
 
      DO j=1,np
       xx(j,1)=Series(j+Lfda-1)
       xa(j,1)=Stci(j+Lfda-1)
      END DO

C CONSTRUCTION OF MATRIX C
c TFF added by Statstics Canada, March 2006 
C ***************       modify factor TTF                   *****************

      TTF = 0.0D0
      DO I =1, NP
            TTF = TTF + DABS(XA(I,1))
      END DO

      TTF = TTF/NP

      DO i=1,np
       DO j=1,np
        Cmat(i,j)=ZERO
       END DO
      END DO
 
c TFF used to modifiy CMAT by Statstics Canada, March 2006 
      DO i=1,np
       CMAT(I,I) = DABS(XA(I,1)/TTF)**LAMDA
      END DO
 
C      ******************************************************
 
      IF (rol.LE.0.99999D00) THEN
 
C CONSTRUCTION OF MATRIX OMECA OMEC
 
       IF (rol.LT.1.0D-10) THEN
        DO i=1,np
         DO j=1,np
          Omec(i,j)=ZERO
         END DO
        END DO
        DO i=1,np
         Omec(i,i)=ONE
        END DO
       ELSE
        DO i=1,np
         DO j=1,np
          k=ABS(i-j)
          Omec(i,j)=rol**k
         END DO
        END DO
       END IF
       kk=PLEN
       kkt=PYRS
       CALL MATMLT(Cmat,Omec,Ttmat,np,np,np,kk,kk,kk)
       CALL MATMLT(Ttmat,Cmat,Tmx1,np,np,np,kk,kk,kk)
       CALL MATMLT(Tmx1,Jmatpi,R1,np,np,naly,kk,kk,kk)
       CALL MATMLT(Jmat,Tmx1,Ttmat2,naly,np,np,kkt,kk,kkt)
       CALL MATMLT(Ttmat2,Jmatpi,Invr,naly,np,naly,kkt,kk,kkt)
       esp=1.0D-20
 
       det=SIMUL(naly,Invr,ansum,esp,-1,kkt)
 
       CALL MATMLT(R1,Invr,r2,np,naly,naly,kk,kkt,kk) 
 
       CALL MATMLT(Jmat,xx,mx1,naly,np,1,kkt,kk,kkt)
       CALL MATMLT(Jmat,xa,mx2,naly,np,1,kkt,kk,kkt)
       CALL ADD_SUB(mx1,mx2,mx3,naly,1,kkt,0)
 
       CALL MATMLT(r2,mx3,xd,np,naly,1,kk,kkt,kk)
       CALL ADD_SUB(xa,xd,xx,np,1,kk,1)
 
       DO j=1,np
        Stci2(j+Lfda-1)=xx(j,1)
       END DO
 
      ELSE
C
c inverse of CMAT computed by Statstics Canada, March 2006 
C     ***************************************************************
C     ***************** FIND THE INVERSE OF CMAT ********************
C     ***************************************************************
       DO I = 1, NP
        CMAT(I,I) = ONE/CMAT(I,I)
       END DO

C     ****************************************************************
 
C       CONSTRUCTION OF MATRIX DELTA
 
       npn1=np-1
 
       DO i=1,np
        DO j=1,np
         delta(i,j)=ZERO
        END DO
       END DO
 
       DO i=1,npn1
        delta(i,i)=MONE
        delta(i,i+1)=ONE
       END DO
 
       DO i=1,npn1
        DO j=1,np
         deltapi(j,i)=delta(i,j)
        END DO
       END DO
 
C CONSTRUCTION OF MATRIX OMECA OMEC
 
       kk=PLEN
       kkt=PYRS
 
       CALL MATMLT(deltapi,delta,Ttmat,np,npn1,np,kk,kk,kk)
 
       CALL MATMLT(Cmat,Ttmat,Omec,np,np,np,kk,kk,kk)
       CALL MATMLT(Omec,Cmat,Tmx1,np,np,np,kk,kk,kk)
 
C      TMX1 = C*DEL'*DEL*C,   A T by T square matrix.
 
C     Construction of the big matrix.
 
       knpn=np+naly
 
       DO i=1,knpn
        DO j=1,knpn
         wcomp1(i,j)=ZERO
        END DO
       END DO
 
       DO i=1,np
        DO j=1,np
         wcomp1(i,j)=Tmx1(i,j)
        END DO
       END DO
 
 
       DO i=1,np
        DO j=1,naly
         wcomp1(i,np+j)=Jmatpi(i,j)
        END DO
       END DO
 
       DO i=1,naly
        DO j=1,np
         wcomp1(np+i,j)=Jmat(i,j)
        END DO
       END DO
 
       DO i=1,knpn
        DO j=1,knpn
         wcomp2(i,j)=ZERO
        END DO
       END DO
 
       DO i=1,np
        DO j=1,np
         wcomp2(i,j)=Tmx1(i,j)
        END DO
       END DO
 
       DO i=1,naly
        wcomp2(np+i,np+i)=ONE
       END DO
 
       DO i=1,naly
        DO j=1,np
         wcomp2(np+i,j)=Jmat(i,j)
        END DO
       END DO
 
C     Find the inverse of WCOMP1
 
       esp=1.0D-10
 
       det=SIMUL(knpn,wcomp1,rratio,esp,-1,kk)
 
       CALL MATMLT(wcomp1,wcomp2,wcomp3,knpn,knpn,knpn,kk,kk,kk)
 
 
C     R2 IS A SUBMATRIX OF WCOMP3
 
       DO i=1,np
        DO j=1,naly
         r2(i,j)=wcomp3(i,j+np)
        END DO
       END DO
 
 
       DO j=1,np
        xx(j,1)=Series(j+Lfda-1)
        xa(j,1)=Stci(j+Lfda-1)
       END DO
 
       CALL MATMLT(Jmat,xx,mx1,naly,np,1,kkt,kk,kkt)
       CALL MATMLT(Jmat,xa,mx2,naly,np,1,kkt,kk,kkt)
       CALL ADD_SUB(mx1,mx2,mx3,naly,1,kkt,0)
 
       CALL MATMLT(r2,mx3,xd,np,naly,1,kk,kkt,kk)
       CALL ADD_SUB(xa,xd,xx,np,1,kk,1)
 
       DO j=1,np
        Stci2(j+Lfda-1)=xx(j,1)
       END DO
 
      END IF

C     ***********   ADDITION NEW OUTPUT    **************
 
      DO j=1,naly
       ansum(j)=mx1(j,1)
       and11(j)=mx2(j,1)
      END DO
 
      IF (Mid.EQ.0) THEN
       DO j=1,np
        cratio(j)=Stci2(j+Lfda-1)/Stci(j+Lfda-1)-ONE
        rratio(j)=ZERO
       END DO
      ELSE
       DO j=1,np
        cratio(j)=Stci2(j+Lfda-1)-Stci(j+Lfda-1)
        rratio(j)=ZERO
       END DO
      END IF
 
      CALL MEANCRA(ansum,and11,rtz,Mid,Ny,naly)
 
      npnp=naly*Ny
 
      DO j=1,npnp
       rratio(j+ns-1)=rtz(j)
      END DO

      IF(Iagr.eq.4)THEN
       IF(Savtab(LCPCRI))CALL punch(cratio,Lfda,Llda,LCPCRI,F,F)
       IF(.not.Lfatal.and.Savtab(LCPRRI))
     &    CALL punch(rratio,Lfda,Llda,LCPRRI,F,F)
      ELSE
       IF(Savtab(LFRCCR))CALL punch(cratio,Lfda,Llda,LFRCCR,F,F)
       IF(.not.Lfatal.and.Savtab(LFRCRR))
     &    CALL punch(rratio,Lfda,Llda,LFRCRR,F,F)
      END IF
      
      RETURN
      END
