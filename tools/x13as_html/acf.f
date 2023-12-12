C     Last change:  BCM   5 Mar 1999   11:04 am
**==acf.f    processed by SPAG 4.03F  at 14:08 on  8 Sep 1994
      SUBROUTINE acf(Z,Nz,Nefobs,R,Se,Nr,Np,Sp,Iqtype,Lmu,Lprt,Lsqr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     ACF computes the sample autocorrelation function of vector z
c-----------------------------------------------------------------------
c Name  type description
c-----------------------------------------------------------------------
c lprt    l  Input logical to print out the table
c ncol    i  Local number of column of autocorrelations to print
c nlag    i  Local number of lags
c np      i  Local number of parameters in the ARIMA model
c nr      i  Output length of vector r, sample autocorrelations
c nefobs  i  Input number of effective observations
c nz      i  Input number of observations in the data vector
c nmaopr  i  Local number of lag operators in the all the components
c             of the structural model
c one     d  Local PARAMETER for 1.0d0
c r       d  Ouput vector of sample autocorrelations
c se      d  Local vector of standard errors
c sp      i  Input length of the seasonal period
c z       d  Input vector of data
c zero    d  Local PARAMETER for 0.0d0
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      INTEGER PR
      DOUBLE PRECISION ONE,ZERO
      LOGICAL T
      PARAMETER(PR=PLEN/4,ONE=1D0,ZERO=0D0,T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'autoq.cmn'
c     ------------------------------------------------------------------
      CHARACTER thisP*(2)
      INTEGER i,ip1,isp,j,jsp,k,mp1,ncol,Nefobs,Nz,Np,Nr,nrm1,Iqtype,Sp,
     &        fh2
      DOUBLE PRECISION c,mu,R,Se,sr,sq,Z
      DIMENSION c(PR),R(PR),Se(PR),Z(Nz)
      LOGICAL Lprt,Lmu,Lsqr
c     ------------------------------------------------------------------
      DOUBLE PRECISION chisq
      EXTERNAL chisq
c     ------------------------------------------------------------------
      IF(Nr.le.0)Nr=min(3*Sp,int(Nz/4.0+.99))
      mu=ZERO
      C0=ZERO
      fh2=0
      IF(.not.Lquiet)fh2=STDERR
c     ------------------------------------------------------------------
      IF(Lmu)THEN
       DO k=1,Nz
        mu=mu+Z(k)
       END DO
      END IF
c     ------------------------------------------------------------------
      mu=mu/Nz
      DO k=1,Nz
       C0=C0+(Z(k)-mu)**2
      END DO
c     ------------------------------------------------------------------
      IF(C0.le.ZERO)THEN
       CALL nWritln('Can''t calculate an autocorrelation function '//
     &              'for a model with no variance.',fh2,Mt2,T,T)
       CALL setdp(DNOTST,PR,Qpv)
c     ------------------------------------------------------------------
      ELSE
       C0=C0/Nz
       sq=ZERO
c----------------------------------------------------------------------
c     formula for computing autocovariances
c     c0=mle var
c              n
c     ck=1/n* sum (z j-mu)(z j-i-mu)
c             j=i+1
c     rk=ck/c0
c----------------------------------------------------------------------
       DO i=1,Nr
        c(i)=ZERO
        ip1=i+1
c     ------------------------------------------------------------------
        DO j=ip1,Nz
         c(i)=c(i)+(Z(j)-mu)*(Z(j-i)-mu)
        END DO
c     ------------------------------------------------------------------
        c(i)=c(i)/Nz
        R(i)=c(i)/C0
        IF(Iqtype.eq.0)THEN
         sq=sq+R(i)**2/(Nefobs-i)
         Qs(i)=sq*Nefobs*(Nefobs+2)
        ELSE
         sq=sq+R(i)**2
         Qs(i)=sq*Nefobs
        END IF
        Dgf(i)=max(0,i-Np)
        IF(Dgf(i).gt.0)THEN
         Qpv(i)=chisq(Qs(i),Dgf(i))
        ELSE
         Qpv(i)=ZERO
        END IF
       END DO
c----------------------------------------------------------------------
c     Use Bartlett's formula for computing standard errors of acf
c     se(rq) =~ 1/sqrt(n)*(1+2(r1**2+r2**2+...+rq**2))
c----------------------------------------------------------------------
       Se(1)=ONE/sqrt(dble(Nz))
       sr=ZERO
       nrm1=Nr-1
c     ------------------------------------------------------------------
       DO i=1,nrm1
        sr=sr+R(i)*R(i)
        Se(i+1)=sqrt((ONE+2.0D0*sr)/dble(Nz))
       END DO
c     ------------------------------------------------------------------
       IF(Lprt)THEN
        ncol=Sp
        IF(Sp.eq.1)ncol=10
        IF(Sp.gt.12)ncol=12
        Inpacf=Inpacf+1
c     ------------------------------------------------------------------
        mp1=(Nr-1)/ncol+1
        DO i=1,mp1
         WRITE(thisP,1011)i
 1011    FORMAT(i2)
         IF(Lsqr)THEN
          write(Mt1,1000)'sacf',Inpacf,i
 1000     FORMAT('<div id="',a,i3.3,'.',i2.2,'">')
          CALL mkTableTag(Mt1,'w80','Sample Autocorrelation of '//
     &                    'Squared Innovations, Part '//thisP)
          CALL mkCaption(Mt1,'Sample Autocorrelation of Squared '//
     &                    'Innovations, Part '//thisP)
         ELSE
          write(Mt1,1000)'acf',Inpacf,i
          CALL mkTableTag(Mt1,'w80','Sample Autocorrelation of '//
     &                    'Innovations, Part '//thisP)
          CALL mkCaption(Mt1,'Sample Autocorrelation of Innovations, '//
     &                   'Part '//thisP)
         END IF
         CALL writTag(Mt1,'<tr>')
         CALL mkTableCell(Mt1,'head','&nbsp;')
         isp=(i-1)*ncol+1
         jsp=min(isp+ncol-1,Nr)
         DO j=isp,jsp
          WRITE(Mt1,1020)j
 1020     FORMAT('<th scope="col"> Lag ',I6,'</th>')
         END DO
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row',
     &                          'Autocorrelation Function','ACF')
         DO j=isp,jsp
          WRITE(Mt1,1030)R(j)
         END DO
 1030    FORMAT('<td>',F6.2,'</td>')
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','Standard Error','SE')
         DO j=isp,jsp
          WRITE(Mt1,1030)se(j)
         END DO
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'<tr>')
         IF(Iqtype.eq.0)THEN
          CALL mkHeaderCellScope(Mt1,0,0,'row','Ljung-Box Q','LB Q')
         ELSE
          CALL mkHeaderCellScope(Mt1,0,0,'row','Box-Pierce Q','BP Q')
         END IF
         DO j=isp,jsp
          WRITE(Mt1,1030)Qs(j)
         END DO
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','Degrees of Freedom','DF')
         DO j=isp,jsp
          WRITE(Mt1,1060)dgf(j)
         END DO
 1060    FORMAT(' <td>',I6,'</td>')
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','Probability','P')
         DO j=isp,jsp
          WRITE(Mt1,1030)Qpv(j)
         END DO
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'</table></div>')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END DO
c     ------------------------------------------------------------------
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
