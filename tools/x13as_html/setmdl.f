C     Last change:  SRD  31 Jan 100    7:39 am
      SUBROUTINE setmdl(Estprm,Laumts)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     setmdl.f, Release 1, Subroutine Version 1.3, Modified 01 May 1995.
c-----------------------------------------------------------------------
c     Subroutine to calculate exact MA ARIMA filter residuals.
c Setmdl differences the X:y matrix and changes the model to remove
c the differencing.  The the remaining ARMA model is estimated.
c Model information is in ARIMA.cmn common so the variables are saved
c between calls of the routines fcnkf, and arflt. Setmdl also
c constructs a vector of parameters to be estimated in the nonlinear
c routine.  Fcnar calculates ARIMA filter residuals given new estimated
c parameters, estprm, from the nonlinear routine, regression residuals,
c tsrs, from rgcpnt, and the model information that was constructed
c in setmdl.  ARflt filters an extended [X:y] matrix from rgcpnt
c using parameter estimates saved during the last fcnkf call.
c-----------------------------------------------------------------------
c Jan 2000 - Argument added to facilitate testing if initial values
c            generated from HR routine were valid (BCM)
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c begptr  i  Local pointer to the first row in opr of the current
c             difference, AR, or MA filter
c estprm  d  Input nestpm long vector of estimated parameters from the
c             nonlinear routine.
c iflt    i  Local index for the current filter type, DIFF, AR, or MA.
c ilag    i  Local index for the current lag, pointer to the current
c             element in lag,coef, and fix.
c iopr    i  Local index for the current operator, it is the pointer
c             to the current row in the operator specfication matrix,
c             opr.
c beglag  i  Local pointer to the current coefficient and lag in arimap
c             and arimal
c nestpm  i  Input number of parmeters in estprm
c nlag    i  Local number of lags in the current operator of a filter
c nopr    i  Local for the number of operators in a DIFF, AR, or MA
c             filter.
c one     d  Local PARAMETER for a double precision 1
c oprptr  i  Local pointer of the current row of opr, specifying the
c             current operator
c zero    d  Local PARAMETER for double precision 0
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INTEGER beglag,begopr,endlag,endopr,iflt,ilag,iopr
      LOGICAL Laumts
      DOUBLE PRECISION Estprm
      DIMENSION Estprm(PARIMA)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     The following declaractions were added by Bor-Chung Chen for
c the initial checking of invertibility and stationarity on 4/26/1995.
c-----------------------------------------------------------------------
      CHARACTER dotln*(POPRCR+1),tmpttl*(POPRCR),thisVal*16,thisRoot*7
      LOGICAL allinv,allfix,onunit,mains,maon,mainsa,maonua,arona
      INTEGER factor,degree,ntmpcr,i,fh2
      DOUBLE PRECISION coef(PORDER+1),zeror(PORDER),zeroi(PORDER),
     &                 zerom(PORDER),zerof(PORDER)
c-----------------------------------------------------------------------
c     The following declaractions were added by Brian C. Monsell for
c the shrinkage of MA operators when roots are close to the unit circle
C on 1/8/1998.
c-----------------------------------------------------------------------
      DOUBLE PRECISION PT9
      PARAMETER(PT9=0.9D0)
      LOGICAL shrnkp
      INTEGER lagind
      DIMENSION lagind(PORDER)
      LOGICAL first
      SAVE first
      DATA first/.true./
c-----------------------------------------------------------------------
      DATA dotln/
     &   '  -----------------------------------------------------------'
     &   /
c-----------------------------------------------------------------------
      mainsa=F
      maonua=F
      arona=F
      fh2=0
      IF(.not.Laumts)fh2=STDERR
c-----------------------------------------------------------------------
c     For each AR and MA filter find out how many estimated parameters
c and add them to the estimated parameter vector estprm.
c-----------------------------------------------------------------------
      Nestpm=0
      shrnkp=F
      DO iflt=DIFF,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        DO ilag=beglag,endlag
         IF(.not.Arimaf(ilag))THEN
          Nestpm=Nestpm+1
          Estprm(Nestpm)=Arimap(ilag)
c-----------------------------------------------------------------------
c       CODE ADDED BY Brian Monsell Jan. 1998
c       Save lag corresponding to the ith estimated operator, in case
c       this operator is to be shrunk later.
c-----------------------------------------------------------------------
          IF(.not.first)lagind(ilag)=Nestpm
c-----------------------------------------------------------------------
         END IF
        END DO
       END DO
      END DO
c-----------------------------------------------------------------------
c     Compute the roots of initial theta(B)=0
c-----------------------------------------------------------------------
      begopr=Mdl(MA-1)
      beglag=Opr(begopr-1)
      endopr=Mdl(MA)-1
c     ------------------------------------------------------------------
      IF(endopr.gt.0)THEN
       endlag=Opr(endopr)-1
       mainsa=F
       maonua=F
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
        CALL setdp(0D0,PORDER,zeror)
        CALL setdp(0D0,PORDER,zeroi)
        CALL setdp(0D0,PORDER,zerom)
c     ------------------------------------------------------------------
        DO ilag=beglag,endlag
         coef(Arimal(ilag)/factor+1)=Arimap(ilag)
        END DO
c     ------------------------------------------------------------------
        CALL roots(coef,degree,allinv,zeror,zeroi,zerom,zerof)
c-----------------------------------------------------------------------
c     Check invertibility
c the roots are g(i)=(zeror(i), zeroi(i)), i=1,2,...,degree
c complex roots are g(i) and g(i+1)
c     If all zeros are invertible do nothing; otherwise print an error
c message and STOP execution of the program.
c-----------------------------------------------------------------------
        mains=F
        IF(.not.allinv)THEN
c-----------------------------------------------------------------------
         CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
         IF(Lfatal)RETURN
         CALL eWritln(tmpttl(1:ntmpcr)//
     &                ' polynomial with initial parameters is'//
     &                ' noninvertible',fh2,Mt2,T,F)
         CALL writln('        with root(s) inside the unit circle. '//
     &               'RESPECIFY model with',fh2,Mt2,F,F)
         CALL writln('        different initial parameters.',
     &               fh2,Mt2,F,T)
c     ------------------------------------------------------------------
         mains=T
         mainsa=T
c     ------------------------------------------------------------------
        END IF
        onunit=F
c-----------------------------------------------------------------------
c       CODE ADDED BY Brian Monsell Jan. 1998
c       Initialize shrnkp, which indicates whether the operator should
c       be shrunk.
c-----------------------------------------------------------------------
        shrnkp=F
        i=0
        DO WHILE (i.lt.degree)
         i=i+1
c-----------------------------------------------------------------------
c       CODE ADDED BY Brian Monsell Jan. 1998
c       If first entry, to see if initial parameters are noninvertible
c       with roots on the unit circle.  Else, see if operator has roots
c       too close to the unit circle and should be shrunk.
c-----------------------------------------------------------------------
         IF(first)THEN
          IF(dpeq(zerom(i),1D0).AND.(.not.onunit))onunit=T
         ELSE
          IF((zerom(i).le.1.06D0).AND.(.not.shrnkp))shrnkp=T
         END IF
        END DO
        allfix=T
        DO ilag=beglag,endlag
         IF((.not.Arimaf(ilag)).and.allfix)allfix=F
        END DO
c     ------------------------------------------------------------------
        maon=F
        IF(onunit.and.(.not.allfix))THEN
         CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
         IF(Lfatal)RETURN
         IF(.not.Laumts)WRITE(STDERR,1020)tmpttl(1:ntmpcr)
         WRITE(Mt2,1021)tmpttl(1:ntmpcr)
         IF(.not.Laumts)WRITE(Mt1,1021)tmpttl(1:ntmpcr)
 1020    FORMAT(/,' ERROR: ',a,' polynomial with initial parameters',
     &          ' is noninvertible',/,'        with root(s) on the ',
     &          'unit circle. RESPECIFY model with',/,
     &          '        different initial parameters.',/)
 1021    FORMAT(/,'<p><strong>ERROR:</strong> ',a,' polynomial with ',
     &            'initial parameters is noninvertible',/,
     &          /,' with root(s) on the unit circle. RESPECIFY ',
     &            'model with',
     &          /,' different initial parameters.</p>',/)
         maon=T
         maonua=T
        END IF
c-----------------------------------------------------------------------
c       CODE ADDED BY Brian Monsell Jan. 1998
c       Multiply model parameter estimates for this operator by a
c       constant, and update the entries in the estimated parameter
c       vector.
c     ------------------------------------------------------------------
        IF(shrnkp.AND.(.not.allfix))THEN
c        CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
c        IF(Lfatal)RETURN
c        WRITE(STDERR,1021)tmpttl(1:ntmpcr)
c        WRITE(Mt2,1021)tmpttl(1:ntmpcr)
c        WRITE(Mt1,1021)tmpttl(1:ntmpcr)
c1021    FORMAT(/,' WARNING:  ',a,' polynomial from a previous estimation',
c    &          ' has root(s)',/,'         on or near the unit circle.')
         DO ilag=beglag,endlag
          IF(.not.Arimaf(ilag))THEN
           Arimap(ilag)=Arimap(ilag)*(PT9**Arimal(ilag))
           Estprm(lagind(ilag))=Arimap(ilag)
          END IF
         END DO
        END IF
c     ------------------------------------------------------------------
        IF((mains.or.maon).and.(.not.Laumts))THEN
*         WRITE(Mt1,1030)tmpttl(1:ntmpcr),dotln
* 1030    FORMAT(' ',a,' Roots',/,'  Root',t25,'Real',t31,'Imaginary',
*     &          t44,'Modulus',t53,'Frequency',/,a)
         CALL mkTableTag(Mt1,'w70',tmpttl(1:ntmpcr)//' Roots')
         CALL mkCaption(Mt1,tmpttl(1:ntmpcr)//' Roots')
         CALL writTag(Mt1,'<tr>')
         CALL mkTableCell(Mt1,'head','&nbsp;')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Real')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Imaginary')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Modulus')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Frequency')
         CALL writTag(Mt1,'</tr>')
c     ------------------------------------------------------------------
         DO i=1,degree
          CALL writTag(Mt1,'<tr>')
          WRITE(thisRoot,1310)i
          CALL mkHeaderCellScope(Mt1,0,0,'row','@',thisRoot)
          WRITE(thisVal,1320)zeror(i)
          CALL mkTableCell(Mt1,'right',thisVal)
          WRITE(thisVal,1320)zeroi(i)
          CALL mkTableCell(Mt1,'right',thisVal)
          WRITE(thisVal,1320)zerom(i)
          CALL mkTableCell(Mt1,'right',thisVal)
          WRITE(thisVal,1320)zerof(i)
          CALL mkTableCell(Mt1,'right',thisVal)
          CALL writTag(Mt1,'</tr>')
         END DO
         CALL writTag(Mt1,'</table>')
        END IF
c-----------------------------------------------------------------------
       END DO
      END IF
 1310 FORMAT('Root ',i2)
 1320 FORMAT(f16.4)
c-----------------------------------------------------------------------
c     Compute the roots of initial phi(B)=0
c-----------------------------------------------------------------------
      begopr=Mdl(AR-1)
      beglag=Opr(begopr-1)
      endopr=Mdl(AR)-1
c     ------------------------------------------------------------------
      IF(endopr.gt.0)THEN
       endlag=Opr(endopr)-1
       arona=F
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        factor=Oprfac(iopr)
        degree=Arimal(endlag)/factor
        coef(1)=-1.0D0
c     ------------------------------------------------------------------
        CALL setdp(0D0,PORDER,zeror)
        CALL setdp(0D0,PORDER,zeroi)
        CALL setdp(0D0,PORDER,zerom)
c     ------------------------------------------------------------------
        CALL setdp(0D0,degree,coef(2))
c     ------------------------------------------------------------------
        DO ilag=beglag,endlag
         coef(Arimal(ilag)/factor+1)=Arimap(ilag)
        END DO
c     ------------------------------------------------------------------
        CALL roots(coef,degree,allinv,zeror,zeroi,zerom,zerof)
c-----------------------------------------------------------------------
c     Check stationarity the roots are g(i)=(zeror(i), zeroi(i)),
c i=1,2,...,degree and the complex roots are g(i) and g(i+1).
c If all zeros are stationary do nothing; otherwise print a warning
c message.  The program may bomb later if the exact AR is used.
c-----------------------------------------------------------------------
        onunit=F
        i=0
        DO WHILE (i.lt.degree)
         i=i+1
         IF(dpeq(zerom(i),1D0).and..not.onunit)onunit=T
        END DO
        IF((.not.allinv).or.onunit)THEN
         CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
         IF(Lfatal)RETURN
         IF(Lar)THEN
          IF(.not.Laumts)WRITE(STDERR,1050)tmpttl(1:ntmpcr)
          WRITE(Mt2,1051)tmpttl(1:ntmpcr)
          IF(.not.Laumts)WRITE(Mt1,1051)tmpttl(1:ntmpcr)
 1050     FORMAT(/,' ERROR: ',a,' polynomial with initial parameters',
     &           ' is nonstationary',/,'          with root(s) on or',
     &           ' inside the unit circle.  RESPECIFY the',/,
     &           '          model with different initial parameters.',
     &           /)
 1051     FORMAT(/,'<p><strong>ERROR:</strong> ',a,' polynomial with ',
     &             'initial parameters is nonstationary',
     &           /,' with root(s) on or inside the unit circle.  ',
     &             'RESPECIFY the',
     &           /,' model with different initial parameters.</p>',/)
          arona=T
c     ------------------------------------------------------------------
         ELSE
          IF(.not.(Laumts.or.Lquiet))WRITE(STDERR,1060)tmpttl(1:ntmpcr)
          WRITE(Mt2,1061)tmpttl(1:ntmpcr)
          IF(.not.Laumts)WRITE(Mt1,1061)tmpttl(1:ntmpcr)
 1060     FORMAT(/,' WARNING: ',a,' polynomial with initial parameters',
     &             ' is nonstationary',
     &           /,'          with root(s) on or inside the unit ',
     &             'circle.  RESPECIFY the model',
     &           /,'          with different initial parameters.',/)
 1061     FORMAT(/,'<p><strong>WARNING:</strong> ',a,' polynomial ',
     &             'with initial parameters is nonstationary',
     &           /,' with root(s) on or inside the unit circle.  ',
     &             'RESPECIFY the model',
     &           /,' with different initial parameters.</p>',/)
         END IF
c     ------------------------------------------------------------------
         IF(.not.Laumts)THEN
          CALL mkTableTag(Mt1,'w70',tmpttl(1:ntmpcr)//' Roots')
          CALL mkCaption(Mt1,tmpttl(1:ntmpcr)//' Roots')
          CALL writTag(Mt1,'<tr>')
          CALL mkTableCell(Mt1,'head','&nbsp;')
          CALL mkHeaderCellScope(Mt1,0,0,'col','@','Real')
          CALL mkHeaderCellScope(Mt1,0,0,'col','@','Imaginary')
          CALL mkHeaderCellScope(Mt1,0,0,'col','@','Modulus')
          CALL mkHeaderCellScope(Mt1,0,0,'col','@','Frequency')
          CALL writTag(Mt1,'</tr>')
c     ------------------------------------------------------------------
          DO i=1,degree
           CALL writTag(Mt1,'<tr>')
           WRITE(thisRoot,1310)i
           CALL mkHeaderCellScope(Mt1,0,0,'row','@',thisRoot)
           WRITE(thisVal,1320)zeror(i)
           CALL mkTableCell(Mt1,'right',thisVal)
           WRITE(thisVal,1320)zeroi(i)
           CALL mkTableCell(Mt1,'right',thisVal)
           WRITE(thisVal,1320)zerom(i)
           CALL mkTableCell(Mt1,'right',thisVal)
           WRITE(thisVal,1320)zerof(i)
           CALL mkTableCell(Mt1,'right',thisVal)
           CALL writTag(Mt1,'</tr>')
          END DO
          CALL writTag(Mt1,'</table>')
         END IF
c     ------------------------------------------------------------------
c         IF(.not.arona)arona=T
c     ------------------------------------------------------------------
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
c       CODE ADDED BY Brian Monsell Jan. 1998
c     ------------------------------------------------------------------
      first=.false.
c     ------------------------------------------------------------------
      IF(mainsa.or.maonua.or.arona)THEN
       IF(Laumts)THEN
        Laumts=F
       ELSE
        CALL abend
       END IF
      END IF
c     ------------------------------------------------------------------
 1000 FORMAT(a)
c     ------------------------------------------------------------------
      RETURN
      END
