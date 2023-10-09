C     Last change:  BCM  12 Mar 98   12:38 pm
      SUBROUTINE chkrt2(Lprmsg,Inverr,Lhiddn)
c-----------------------------------------------------------------------
c     chkrt2.f, Release 1, Subroutine Version 1.1, Modified 07 Dec 1995.
c-----------------------------------------------------------------------
c     Check the roots of theta(B)=0 and makes them invertible if
c their roots are inside the unit circle.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c dotln   c  Local pgrpcr character dotted line under the model title
c degree  i  Maximum lag of phi(B) or theta(B)
c degp1   i  degree + 1
c coef    d  Coefficients of phi(B) or theta(B) in order of increasing
c             powers
c rcoef   d  Coefficients of phi(B) or theta(B) in order of decreasing
c             powers
c zeror   d  Real part of the roots
c zeroi   d  Imaginary part of the roots
c-----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER dotln*(POPRCR+1),tmpttl*(POPRCR),thisId*6,thisHdr*20,
     &          thisVal*16,thisLag*6,thisRoot*7
      LOGICAL allinv,Lprmsg,Lhiddn
      INTEGER beglag,begopr,degree,endlag,endopr,factor,i,ilag,Inverr,
     &        iopr,ntmpcr,imt
      DOUBLE PRECISION coef(PORDER+1),zeror(PORDER),zeroi(PORDER),
     &                 zerom(PORDER),zerof(PORDER)
      DATA dotln/
     &   '  -----------------------------------------------------------'
     &   /
c-----------------------------------------------------------------------
c     Check the roots of theta(B)=0 and phi(B)=0 if using exact AR
c-----------------------------------------------------------------------
      Inverr=0
c     ------------------------------------------------------------------
      IF(Lextar)THEN
       begopr=Mdl(AR-1)
      ELSE
       begopr=Mdl(MA-1)
      END IF
c     ------------------------------------------------------------------
      beglag=Opr(begopr-1)
      endopr=Mdl(MA)-1
c     ------------------------------------------------------------------
      IF(endopr.gt.0)THEN
       endlag=Opr(endopr)-1
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        factor=Oprfac(iopr)
        degree=Arimal(endlag)/factor
        coef(1)=-1.0D0
        CALL setdp(0D0,degree,coef(2))
c     ------------------------------------------------------------------
        DO ilag=beglag,endlag
         coef(Arimal(ilag)/factor+1)=Arimap(ilag)
        END DO
c     ------------------------------------------------------------------
        allinv=F
        CALL roots(coef,degree,allinv,zeror,zeroi,zerom,zerof)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check invertibility and modify the polynomial and sigma_square
c the roots are g(i)=(zeror(i), zeroi(i)), i=1,2,...,degree
c complex roots are g(i) and g(i+1)
c     If all zeros are invertible do nothing; otherwise expand
c polynomials to get new coefficients.
c-----------------------------------------------------------------------
        IF(.not.allinv)THEN
         IF(Lprier)THEN
          CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
          IF(Lfatal)RETURN
          IF(Lprmsg)CALL writln(tmpttl(1:ntmpcr)//
     &   ' roots inside the unit circle.  Will attempt to invert them.',
     &				Mt2,STDERR,T,T)
c     ------------------------------------------------------------------
          imt=Mt1
          IF(Lhiddn)imt=Mt2
*          WRITE(imt,1010)tmpttl(1:ntmpcr),dotln
* 1010     FORMAT(' ',a,' Roots',/,'  Root',t25,'Real',t31,'Imaginary',
*     &           t44,'Modulus',t53,'Frequency',/,a)
          CALL mkTableTag(imt,'w70',tmpttl(1:ntmpcr)//' Roots')
          CALL mkCaption(imt,tmpttl(1:ntmpcr)//' Roots')
          CALL writTag(imt,'<tr>')
          CALL mkTableCell(imt,'@','&nbsp;')
          CALL mkHeaderCellScope(imt,0,0,'col','@','Real')
          CALL mkHeaderCellScope(imt,0,0,'col','@','Imaginary')
          CALL mkHeaderCellScope(imt,0,0,'col','@','Modulus')
          CALL mkHeaderCellScope(imt,0,0,'col','@','Frequency')
          CALL writTag(imt,'</tr>')
c     ------------------------------------------------------------------
          DO i=1,degree
           CALL writTag(imt,'<tr>')
           WRITE(thisRoot,1310)i
           CALL mkHeaderCellScope(imt,0,0,'row','@',thisRoot)
           WRITE(thisVal,1320)zeror(i)
           CALL mkTableCell(imt,'right',thisVal)
           WRITE(thisVal,1320)zeroi(i)
           CALL mkTableCell(imt,'right',thisVal)
           WRITE(thisVal,1320)zerom(i)
           CALL mkTableCell(imt,'right',thisVal)
           WRITE(thisVal,1320)zerof(i)
           CALL mkTableCell(imt,'right',thisVal)
           CALL writTag(imt,'</tr>')
          END DO
          CALL writTag(imt,'</table>')
         END IF
        END IF
       END DO
      END IF
c     ------------------------------------------------------------------
 1310 FORMAT('Root ',i2)
 1320 FORMAT(f16.4)
c     ------------------------------------------------------------------
      RETURN
      END
