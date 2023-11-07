C     Last change:  BCM  25 Nov 97   11:48 am
**==kwtest.f    processed by SPAG 4.03F  at 15:12 on  1 Aug 1994
      SUBROUTINE kwtest(X,Ib,Ie,Nyr,Lprt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE APPLIES THE KRUSKAL-WALLIS TEST TO X. THE K-W TEST
C --- IS THE NONPARAMETRIC EQUIVALENT OF THE F-TEST.
C --- THE ARRAY X IS DESTROYED IN THE CALCULATION PROCEDURE.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tests.cmn'
c-----------------------------------------------------------------------
      LOGICAL Lprt
*      CHARACTER xb*50
      DOUBLE PRECISION ck,X,chisq,xval
      INTEGER i,Ib,Ie,j,k,kolr,kval,l,n,ndf,ns,Nyr
      DIMENSION X(Ie),ns(PSP),kolr(PSP),k(PLEN)
      EXTERNAL chisq
c-----------------------------------------------------------------------
C --- INITIALIZE.
c-----------------------------------------------------------------------
      DO i=1,Nyr
       kolr(i)=0
       ns(i)=0
      END DO
      DO i=Ib,Ie
       k(i)=i
      END DO
c-----------------------------------------------------------------------
C --- RANK THE ARRAY X.
c-----------------------------------------------------------------------
      DO i=Ib,Ie
       xval=X(i)
       kval=k(i)
       DO j=i,Ie
        IF(xval.gt.X(j))THEN
         X(i)=X(j)
         k(i)=k(j)
         X(j)=xval
         k(j)=kval
         xval=X(i)
         kval=k(i)
        END IF
       END DO
      END DO
c-----------------------------------------------------------------------
C --- CALCULATE THE COLUMN SUM OF RANKS.
c-----------------------------------------------------------------------
      DO i=Ib,Ie
       l=k(i)-(k(i)-1)/Nyr*Nyr
       ns(l)=ns(l)+1
       kolr(l)=i-Ib+1+kolr(l)
      END DO
c-----------------------------------------------------------------------
C --- CALCULATE THE K-W STATISTIC.
c-----------------------------------------------------------------------
      ck=0D0
      DO i=1,Nyr
       ck=ck+kolr(i)*kolr(i)/dble(ns(i))
      END DO
      n=Ie-Ib+1
      Chikw=12D0*ck/(n*(n+1))-3*(n+1)
      ndf=Nyr-1
      P5=chisq(Chikw,ndf)*100D0
      IF(.not.Lprt.or.Lhiddn)RETURN
      IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
      CALL mkTableTag(Mt1,'w70','Nonparametric Test for the Presence '//
     &                'of Seasonality Assuming Stability')
      CALL mkCaption(Mt1,'Nonparametric Test for the Presence of '//
     &               'Seasonality Assuming Stability')
      CALL writTag(Mt1,'<tr>')
      CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                       'Kruskal-Wallis statistic')
      CALL mkHeaderCellScope(Mt1,0,0,'col','@','Degrees of Freedom')
      CALL mkHeaderCellScope(Mt1,0,0,'col','@','Probability level')
      CALL writTag(Mt1,'</tr>')
      CALL writTag(Mt1,'<tr>')
      WRITE(Mt1,1031)Chikw,ndf,P5
 1031 FORMAT('<td class="center">',F11.4,'</td>',/,
     &       '<td class="center">',I3,'</td>',/,
     &       '<td class="center">',F9.3,'%</td>')
      CALL writTag(Mt1,'</tr>')
      CALL writTag(Mt1,'</table>')
      IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
      IF(P5.le.1D0)THEN
       CALL mkPOneLine(Mt1,'center',
     &                 'Seasonality present at the one percent level.')
       RETURN
      END IF
      CALL mkPOneLine(Mt1,'center',
     &           'No evidence of seasonality at the one percent level.')
      RETURN
      END
