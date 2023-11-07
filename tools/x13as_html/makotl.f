C     Last change:  BCM  17 Jul 2003    8:57 pm
**==makotl.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE makotl(T0,Nr,Ltest,Otlvar,Notlr,Tcalfa,Sp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Makes AO and LS outlier variables for effects at time t0.
c Variables are nr long.  If notlr is 1 only and AO outlier is made
c and if notlr is 2 then the AO is in column 1 and the LS in column 2.
c AO is 1 and time t0 and 0 otherwise.  LS is -1 if t<t0 and 0 is t>=t0.
c Note that the array in the calling program is otlvar(2,nr) and is
c referenced by otlvar(outlier type,time).
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c i       i  Local do loop index
c ltest   l  Input logical array ltest(1)=true means creat an AO
c             variable and ltest(2)=true creat an LS variable
c mone    d  Local PARAMETER for a double precision -1
c notlr   i  Output number of outlier types 1 to make just AO or LS,
c             and 2 for both AO and LS
c nr      i  Input number of rows in the outlier variable matrix
c one     d  Local PARAMETER for a double precision  1
c otlvar  d  Output nr by notlr outlier array variable
c t0      i  Input index for the time point the outlier occured at
c zero    d  Local PARAMETER for a double precision  0
c-----------------------------------------------------------------------
c     Data typing and definition
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION MONE,ONE,ZERO
      PARAMETER(ONE=1D0,MONE=-ONE,ZERO=0D0)
c     ------------------------------------------------------------------
      INTEGER Ltest(POTLR)
      INTEGER i,Notlr,Nr,T0,dsp,Sp,imod0,t1
      DOUBLE PRECISION Otlvar,Tcalfa,drmp
      DIMENSION Otlvar(Nr*POTLR),dsp(POTLR-1)
c-----------------------------------------------------------------------
c     Set up the dispensation variable for AO and LS and Notlr.
c-----------------------------------------------------------------------
      CALL setint(0,POTLR-1,dsp)
*      IF(Ltest(SO).eq.1)THEN
*       dsp(AO)=1
*       dsp(LS)=1
*       dsp(TC)=1
*      END IF
      IF(Ltest(TC).eq.1)THEN
       dsp(AO)=dsp(AO)+1
       dsp(LS)=dsp(LS)+1
      END IF
      IF(Ltest(LS).eq.1)dsp(AO)=dsp(AO)+1
      Notlr=dsp(AO)
      IF(Ltest(AO).eq.1)Notlr=Notlr+1
c-----------------------------------------------------------------------
c     Begin generating matrix of outliers by generating observations
c     before T0
c-----------------------------------------------------------------------
      DO i=Notlr,Notlr*(T0-1),Notlr
       IF(Ltest(AO).eq.1)Otlvar(i-dsp(AO))=ZERO
       IF(Ltest(LS).eq.1)Otlvar(i-dsp(LS))=MONE
*       IF(Ltest(TC).eq.1)Otlvar(i-dsp(TC))=ZERO
*       IF(Ltest(SO).eq.1)Otlvar(i)=ZERO
       IF(Ltest(TC).eq.1)Otlvar(i)=ZERO
      END DO
c     ------------------------------------------------------------------
c     Now geneate values of observation T0
c     ------------------------------------------------------------------
      i=T0*Notlr
      IF(Ltest(AO).eq.1)Otlvar(i-dsp(AO))=ONE
      IF(Ltest(LS).eq.1)Otlvar(i-dsp(LS))=ZERO
*      IF(Ltest(TC).eq.1)Otlvar(i-dsp(TC))=ONE
*      IF(Ltest(SO).eq.1)THEN
*       Otlvar(i)=ONE
*       imod0=mod(T0,Sp)
*       t1=T0
*       drmp=MONE/DBLE(Sp-1)
*      END IF
      IF(Ltest(TC).eq.1)Otlvar(i)=ONE
c     ------------------------------------------------------------------
      DO i=i+Notlr,Nr*Notlr,Notlr
       IF(Ltest(AO).eq.1)Otlvar(i-dsp(AO))=ZERO
       IF(Ltest(LS).eq.1)Otlvar(i-dsp(LS))=ZERO
*       IF(Ltest(TC).eq.1)
*     &    Otlvar(i-dsp(TC))=Otlvar(i-dsp(TC)-Notlr)*Tcalfa
*       IF(Ltest(SO).eq.1)THEN
*        t1=t1+1
*        IF(imod0.eq.mod(t1,Sp))THEN
*         Otlvar(i)=ONE
*        ELSE
*         Otlvar(i)=drmp
*        END IF
*       END IF
       IF(Ltest(TC).eq.1)
     &    Otlvar(i)=Otlvar(i-Notlr)*Tcalfa
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
