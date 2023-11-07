C     Last change:  BCM  13 Nov 97    9:55 am
**==pacf.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE pacf(Nefobs,Sp,R,Se,Nr,Lprt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     PACF computes the sample partial autocorrelation function
c by solving the Yule-Walker Equations.  Must first have called ACF
c to get the sample autocorrelations.
c-----------------------------------------------------------------------
c Name  type description
c-----------------------------------------------------------------------
c lprt    l  Input logical to print the table
c ncol    i  Local number of column of autocorrelations to print
c nefobs  i  Input of effective observations
c nr      i  Input length of vector r, sample autocorrelations and
c             pacf's
c r       d  Input vector of sample autocorrelations
c fkk     d  Local vector of sample partial autocorrelations
c se      d  Local vector of standard errors
c sp      i  Input length of the seasonal period
c zero    d  Local PARAMETER for 0.0d0
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      INTEGER PR
      DOUBLE PRECISION ZERO
      PARAMETER(PR=PLEN/4,ZERO=0D0)
c     ------------------------------------------------------------------
      CHARACTER thisP*(2)
      LOGICAL Lprt
      INTEGER i,isp,j,jsp,l,mp1,ncol,Nefobs,Nr,Sp
      DOUBLE PRECISION dtop,dbot,fkk,p,R,Se,sep
      DIMENSION fkk(PR),p(PR,PR),R(PR),Se(PR)
c     ------------------------------------------------------------------
      sep=1/sqrt(dble(Nefobs))
      p(1,1)=R(1)
      fkk(1)=p(1,1)
      Se(1)=sep
c     ------------------------------------------------------------------
      DO l=2,Nr
       Se(l)=sep
       dtop=ZERO
       dbot=ZERO
c     ------------------------------------------------------------------
       DO j=1,l-1
        dtop=dtop+p(l-1,j)*R(l-j)
        dbot=dbot+p(l-1,j)*R(j)
       END DO
c     ------------------------------------------------------------------
       p(l,l)=(R(l)-dtop)/(1-dbot)
       fkk(l)=p(l,l)
c     ------------------------------------------------------------------
       DO j=1,l-1
        p(l,j)=p(l-1,j)-p(l,l)*p(l-1,l-j)
       END DO
      END DO
c     ------------------------------------------------------------------
      IF(Lprt)THEN
       ncol=Sp
       IF(Sp.eq.1)ncol=10
       IF(Sp.gt.12)ncol=12
c     ------------------------------------------------------------------
       mp1=(Nr-1)/ncol+1
       DO i=1,mp1
        WRITE(thisP,1010)i
        write(Mt1,1000)'pacf',Inpacf,i
        CALL mkTableTag(Mt1,'w80',
     &    'Sample Partial Autocorrelation of Innovations, Part '//thisP)
        CALL mkCaption(Mt1,
     &    'Sample Partial Autocorrelation of Innovations, Part '//thisP)
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        isp=(i-1)*ncol+1
        jsp=min(isp+ncol-1,Nr)
        DO j=isp,jsp
         WRITE(thisP,1010)j
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Lag  '//thisP)
        END DO
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row',
     &                        'Partial Autocorrelation Function','PACF')
        DO j=isp,jsp
         WRITE(Mt1,1020)fkk(j)
        END DO
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row','Standard Error','SE')
        DO j=isp,jsp
         WRITE(Mt1,1020)se(j)
        END DO
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END DO
      END IF
      CALL copy(fkk,Nr,1,R)
c     ------------------------------------------------------------------
 1000 FORMAT('<div id="',a,i3.3,'.',i2.2,'">')
 1010 FORMAT(i2)
 1020 FORMAT(' <td>',F6.2,'</td>')
c     ------------------------------------------------------------------
      RETURN
      END
