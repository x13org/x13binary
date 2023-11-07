C     Last change:  BCM   2 Jun 1998   11:43 am
      SUBROUTINE templs(Lsrun,Rmse,Xpxinv,Prttls,Ldiag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     templs.f, Release 1, Subroutine Version 1.4, Modified 13 Mar 1995.
c-----------------------------------------------------------------------
c     This subroutine computes t-statistics to test for temporary level
c shifts, that is, to test whether a run of successive level shifts
c have coefficient estimates that sum to something close to 0 (in the
c sense that the sum is not statistically significantly different from
c 0).  These t-statistics are computed for all runs of 2 or more
c successive level shifts up to a maximum of LSrun successive level
c shifts.
c  Author:  Bill Bell, SRD, 3/25/93
c
c  Input Arguments:
c  Name   Type  Definition
c   nls   int   number of level shifts
c    b     dp   nls x 1 vector of estimated level shift coefficients,
c                assumed to be arranged sequentially in time
c    V     dp   nls x nls variance-covariance matrix of b
c  nrowV  int   row dimension of the array V
c  title  char  nls x 1 character vector of labels of level shifts
c  LSrun  int   maximum number of successive level shifts used in
c                computing the t-statistics, i.e. if nls > LSrun,
c                then the largest number of level shifts would arise
c                in computing t-statistics for b(1) + ... + b(LSrun),
c                b(2) + ... + b(LSrun+1), ... , b(nls-LSrun+1) + ...
c                + b(nls)
c Sometime in the future might want to put in an option to say that
c only LS's within a certain time span (say 5 years) would be tested.
c
c     7/29/1999 (Matt Kramer) Bug corrected in this subroutine that
c     aborted the program from formatting problems.  The problem 
c     occurred if there was a request for temporary level shift tests,
c     lsrun > 2, and there were a large number of level shifts.  
c     Excessive spacing was programmed.  The fix involves introducing
c     a new variable, mxchr, which holds the length of the longest
c     character string used to identify individual outliers, and 
c     calculating mxtlcr only at the end of the first DO loop.  The
c     maximum number of successive level shifts that can be tested
c     is hardcoded in gtotlr.f (and in this subroutine at line 103
c     for the fixed table width option).
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.prm'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION TWO
      PARAMETER(TWO=2D0)
c-----------------------------------------------------------------------
      LOGICAL cmpstr,locok,Prttls,Ldiag
      CHARACTER fmt*(PSRSCR),str*(PCOLCR),tstttl*(PSRSCR)
      INTEGER baselt,begls,endls,i,icol,ils,ilstp,ipos,ipvt,ipvt2,j,jls,
     &        jpvt,lspvt,Lsrun,lstls,lstp,mxtlcr,nchr,nls,ntest,otlidx,
     &        mxchr,nlsrun
      DOUBLE PRECISION lsxpxi,Rmse,se,sumb,sumvar,tstat,Xpxinv
      DIMENSION lspvt(PB),lstp(PB),lsxpxi(PB*(PB+1)/2),
     &          Xpxinv(Nb*(Nb+1)/2)
      EXTERNAL cmpstr
c-----------------------------------------------------------------------
c     Find all the level shifts in the regression, both user-defined
c and automatically identified.  Then make an index of the columns
c they are in.
c-----------------------------------------------------------------------
      mxchr=0
      nls=0
c     ------------------------------------------------------------------
      DO icol=1,Nb
       CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
       IF(Lfatal)RETURN
       IF(cmpstr(NAME,'ls',str(1:2)))THEN
        CALL rdotlr(str(1:nchr),Begspn,Sp,otlidx,ilstp,endls,locok)
c-----------------------------------------------------------------------
c     Sort the level shifts by time.  This needs to be done because
c they are found in two places.
c-----------------------------------------------------------------------
        IF(locok.and.otlidx.eq.LS.AND.(.not.Regfx(icol)))THEN
         DO ils=1,nls
          IF(ilstp.lt.lstp(ils))THEN
           IF(ils.le.nls)THEN
            CALL cpyint(lspvt(ils),nls-ils+1,-1,lspvt(ils+1))
            CALL cpyint(lstp(ils),nls-ils+1,-1,lstp(ils+1))
           END IF
c-----------------------------------------------------------------------
           GO TO 10
          END IF
         END DO
         ils=nls+1
c-----------------------------------------------------------------------
   10    lspvt(ils)=icol
         lstp(ils)=ilstp
         nls=nls+1
         mxchr = max (nchr, mxchr)
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
c     Check if there are runs of level-shifts to test.
c-----------------------------------------------------------------------
      IF(Ldiag)THEN
       WRITE(Nform,1000)'lsrun: ',Lsrun
       IF(nls.le.1)THEN
        WRITE(Nform,1000)'nlsrun: ',0
       ELSE
        ntest=min(nls,Lsrun)
        nlsrun=0
        DO ntest=2,ntest
         DO begls=1,nls-ntest+1
          nlsrun=nlsrun+1
         END DO
        END DO
        WRITE(Nform,1000)'nlsrun: ',nlsrun
       END IF
      END IF
      IF(nls.le.1)RETURN
 1000 FORMAT(a,i5)
c-----------------------------------------------------------------------
      IF(Prttls)THEN
       IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL genSkip(1208)
       mxtlcr = max (18, (min (Lsrun, nls) * mxchr) - 1)
c     mxtlcr = max (18, (7 * mxchr) - 1)               ! for fixed width
       WRITE(Mt1,1010)Inpmdl,'tls'
       CALL mkTableTag(Mt1,'w70',
     &                 'Tests for Cancellation of Level Shifts')
       CALL mkCaption(Mt1,'Tests for Cancellation of Level Shifts')
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','Dates of LS Sets')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','Span')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','t-value')
       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
c     Creat a sub matrix of inv(X'X) for i,j=1,nls
c lsxpxi(i,j)=Xpxinv(ipvt,jpvt)
c-----------------------------------------------------------------------
      DO i=1,nls
       ipvt=lspvt(i)
c-----------------------------------------------------------------------
       DO j=1,i
        jpvt=lspvt(j)
        ipvt2=max(ipvt,jpvt)
        baselt=(ipvt2-1)*ipvt2/2
        lsxpxi((i-1)*i/2+j)=Xpxinv(baselt+min(ipvt,jpvt))
       END DO
      END DO
c-----------------------------------------------------------------------
c     Loop over number of level shifts (k), from 2 to kk = min(nls,
c LSrun).  Initialize sumb (sum of the coefficient estimates) and
c sumvar (variance of the sum of the coefficient estimates) to 0.
c Then loop over starting level shift (m), from 1 to nls-k+1.
c-----------------------------------------------------------------------
      ntest=min(nls,Lsrun)
      nlsrun=0
c-----------------------------------------------------------------------
      DO ntest=2,ntest
       DO begls=1,nls-ntest+1
        endls=begls+ntest-1
        ipvt=lspvt(begls)
        CALL getstr(Colttl,Colptr,Ncoltl,ipvt,str,nchr)
        IF(Lfatal)RETURN
        tstttl(1:nchr+3)=str(3:nchr)//'+'
        ipos=nchr-2+1+1
        sumb=B(ipvt)
        baselt=begls*(begls+1)/2
        sumvar=lsxpxi(baselt)
c-----------------------------------------------------------------------
c     Sum the coefficient estimates b(m) + ... + b(m+k-1).  Also
c compute the variance of this sum and the corresponding t-statistic
c (tstat).
c-----------------------------------------------------------------------
        DO lstls=begls+1,endls
         ipvt=lspvt(lstls)
         sumb=sumb+B(ipvt)
         baselt=(lstls-1)*lstls/2
         sumvar=sumvar+lsxpxi(baselt+lstls)
c-----------------------------------------------------------------------
         DO jls=begls,lstls-1
          sumvar=sumvar+TWO*lsxpxi(baselt+jls)
         END DO
c-----------------------------------------------------------------------
c     Concatenate the titles of the level shifts being summed into tprt.
c A maximum of 5 level shift titles can be printed on any one
c line, so tprt is an array with each element used to store the
c concatenated titles for up to 7 successive level shifts.
c-----------------------------------------------------------------------
         CALL getstr(Colttl,Colptr,Ncoltl,ipvt,str,nchr)
         IF(Lfatal)RETURN
         tstttl(ipos:ipos+nchr)=str(3:nchr)//'+'
         ipos=ipos+nchr-1
        END DO
c-----------------------------------------------------------------------
        se=sqrt(sumvar)*Rmse
        tstat=sumb/se
c-----------------------------------------------------------------------
c     Print out the results for this sum of level shifts (sumb,tstat).
c-----------------------------------------------------------------------
        IF(Prttls)THEN
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',tstttl(1:ipos-2))
         WRITE(Mt1,1020)lstp(endls)-lstp(begls),tstat
         CALL writTag(Mt1,'</tr>')
        ENDIF
        IF(Ldiag)THEN
         nlsrun=nlsrun+1
         WRITE(Nform,1030)nlsrun,tstttl(1:ipos-2),' ',
     &                    lstp(endls)-lstp(begls),' ',tstat
        END IF
       END DO
      END DO
      IF(Prttls)CALL writTag(Mt1,'</table></div>')
c-----------------------------------------------------------------------
 1010 FORMAT('<div id="mdl',i3.3,a,'">')
 1020 FORMAT('<td class="center">',i3,'</td><td class="center">',
     &       g21.6,'</td>')
 1030 FORMAT('lsspan',i2.2,': ',a,a,i3,a,e21.14)
      RETURN
      END
