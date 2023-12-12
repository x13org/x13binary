C     Last change:  BCM   5 Mar 1999    9:44 am
**==savacf.f    processed by SPAG 4.03F  at 10:31 on 29 Jul 1994
      SUBROUTINE savacf(Fh,Itbl,Rho,Se,Mxlag,Ndf,Nsdf)
c-----------------------------------------------------------------------
c     SAVACF() prints out the sample autocorrelation function, standard
c errors.  Called by prtacf()
c-----------------------------------------------------------------------
c Name  type description
c-----------------------------------------------------------------------
c rho     d  Ouput vector of sample autocorrelations
c se      d  The standard errors
c mxlag   i  Length of vector rho, se
c itbl    i  Table id for PACF or ACF, see tbllog.i
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER outstr*108
      INTEGER i,Itbl,Mxlag,Fh,ipos,Ndf,Nsdf
      DOUBLE PRECISION Rho,Se
      DIMENSION Rho(Mxlag),Se(Mxlag)
c-----------------------------------------------------------------------
      INTEGER PR
      PARAMETER(PR=PLEN/4)
      INCLUDE 'autoq.cmn'
c-----------------------------------------------------------------------
c     Print the seasonal and nonseasonal differences used in the ACFs
c     and PACFs generated from the identify spec.
c-----------------------------------------------------------------------
      IF(Ndf.ne.NOTSET)WRITE(Fh,1001)'$diff=',Ndf
      IF(Nsdf.ne.NOTSET)WRITE(Fh,1001)'$sdiff=',Nsdf
 1001 FORMAT(a,i2)
c-----------------------------------------------------------------------
c     Print the autocorrelation function
c-----------------------------------------------------------------------
      IF(Itbl.eq.LCKACF.or.Itbl.eq.LIDACF.or.Itbl.eq.LCKAC2)THEN
       IF(Itbl.eq.LCKAC2)THEN
        WRITE(Fh,1010)'Lag',TABCHR,'Sample_ACF2',TABCHR,'SE_of_ACF2',
     &                TABCHR,'Ljung-Box_Q',TABCHR,'df_of_Q',TABCHR,
     &                'P-value'
       ELSE
        WRITE(Fh,1010)'Lag',TABCHR,'Sample_ACF',TABCHR,'SE_of_ACF',
     &                TABCHR,'Ljung-Box_Q',TABCHR,'df_of_Q',TABCHR,
     &                'P-value'
       END IF
       WRITE(Fh,1010)'---',TABCHR,'-----------------------',TABCHR,
     &               '-----------------------',TABCHR,
     &               '-----------------------',TABCHR,'---',TABCHR,
     &               '-----------------------'
      ELSE
       WRITE(Fh,1010)'Lag',TABCHR,'Sample_PACF',TABCHR,'S.E._of_PACF'
       WRITE(Fh,1010)'---',TABCHR,'-----------------------',TABCHR,
     &               '-----------------------'
      END IF
c     ------------------------------------------------------------------
      DO i=1,Mxlag
       ipos=1
       IF(i.lt.10)THEN
        outstr(ipos:ipos)='0'
        ipos=ipos+1
       END IF
       CALL itoc(i,outstr,ipos)
       IF(Lfatal)RETURN
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       CALL dtoc(Rho(i),outstr,ipos)
       IF(Lfatal)RETURN
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       CALL dtoc(Se(i),outstr,ipos)
       IF(Lfatal)RETURN
       IF(Itbl.eq.LCKACF.or.Itbl.eq.LCKAC2.or.Itbl.eq.LIDACF)THEN
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Qs(i),outstr,ipos)
        IF(Lfatal)RETURN
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
        IF(Dgf(i).lt.10)THEN
         outstr(ipos:ipos)='0'
         ipos=ipos+1
        END IF
        CALL itoc(Dgf(i),outstr,ipos)
        IF(Lfatal)RETURN
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Qpv(i),outstr,ipos)
        IF(Lfatal)RETURN
       END IF
       WRITE(Fh,1010)outstr(1:ipos-1)
      END DO
c     ------------------------------------------------------------------
      RETURN
c     ------------------------------------------------------------------
 1010 FORMAT(a:,a,a,a,a:,a,a,a,a,a,a)
      END
