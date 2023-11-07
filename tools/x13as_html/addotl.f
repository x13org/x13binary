C     Last change:  BCM  16 Jul 2003    5:07 pm
      SUBROUTINE addotl(Bgdtxy,Nrxy,Iymx,Begcol,Endcol)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Add outlier effect variables from begcol:endcol outliers in an
c nrxy by ncxy Xy matrix.  Outliers are defined by date (yr,mo) and
c type, AO or LS in the notlr rows of otlr.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c begcol  i  Local begining column of the x matrix that holds the
c             outlier effects
c begotl  i  Local index for t0 of an AO or LS outlier,
c             or the begining of the ramp.
c bgdtXy  i  Input date array of the starting date of the observations
c             in the extended Xy matrix (yr,mo)
c drmp    d  Local number of point on the ramp, the inverse
c             is the increment the ramp must climb
c             each time point
c endcol  i  Local index for last column of x used for outlier effects
c endotl  i  Local end of the ramp.
c icol    i  Local index for the current Xy matrix column
c irow    i  Local index for the current Xy matrix row
c iymx    i  Input difference in the number of time points between
c             the start dates of the series, y, and the regression
c             variables, x.
c j       i  Local do loop index
c mone    d  Local PARAMETER double precision -1.  Used to set
c             the LS effects
c notlr   i  Local number of outliers,  also is the number of columns
c             used for outlier effects and the number of rows of otlr
c one     d  Local PARAMETER double precision 1
c otldat  i  Local date (yr,mo) that the outlier occured on or the dates
c             (begyr,begmo,endyr,endmo) the ramp occured on.
c otltyp  c  Local 2 character outlier type specifier, AO, LS, or RP
c predat  i  Local date one period before the start of X.  Used as a
c             displacement
c zero    d  Local PARAMETER for a double precision 0d0
c-----------------------------------------------------------------------
c     Date types and initialization
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE,MONE
      LOGICAL F,T
      PARAMETER(ZERO=0.0D0,ONE=1.0D0,MONE=-1.0D0,F=.false.,T=.true.)
c     ------------------------------------------------------------------
      CHARACTER str*(19)
      LOGICAL locok
      INTEGER Bgdtxy,begotl,endotl,icol,irow,nchr,Nrxy,otltyp,Begcol,
     &        Endcol,Iymx,imod,fh2
      DOUBLE PRECISION drmp,drow
      DIMENSION Bgdtxy(2)
c-----------------------------------------------------------------------
c     Check that the begining and ending columns are between 1 and nb.
c-----------------------------------------------------------------------
      IF(Begcol.lt.1.or.Endcol.gt.Ncxy-1.or.Endcol.lt.Begcol)THEN
       IF(.not.Lhiddn)WRITE(STDERR,1010)Begcol,Endcol,Ncxy-1
       CALL errhdr
       WRITE(Mt2,1011)Cbr,Begcol,Endcol,Ncxy-1
 1010  FORMAT(/,' ERROR: Column,  1<=begcol<=endcol<=    nb',
     &        /,25x,3I8,'.')
 1011  FORMAT(/,'<p><strong>ERROR:</strong> Column,  1<=begcol<=endcol',
     &        '<=    nb',a,/,1x,'1<=',I8,'<=',I8,'<=',I8,'.</p>')
       CALL abend
       RETURN
      END IF
      fh2=0
      IF(.not.(Lquiet.or.Lhiddn))fh2=STDERR
c-----------------------------------------------------------------------
c     Check and add the outliers
c-----------------------------------------------------------------------
      icol=Begcol
      DO WHILE (icol.le.Endcol)
       CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
       IF(.not.Lfatal)THEN
        CALL rdotlr(str(1:nchr),Bgdtxy,Sp,otltyp,begotl,endotl,locok)
        IF(.not.locok)CALL abend
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     AO outlier
c-----------------------------------------------------------------------
       IF(otltyp.eq.AO.or.otltyp.eq.MV)THEN
        IF(begotl.gt.Iymx+Nspobs.or.begotl.lt.Iymx+1)THEN
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Removing '//str(1:nchr)//
     &                ' from the regression because it is not within',
     &                fh2,Mt2,T,F)
         CALL writln('       the span of the data.',fh2,Mt2,F,T)
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         Endcol=Endcol-1
c     ------------------------------------------------------------------
        ELSE
         DO irow=1,Nrxy
          Xy(Ncxy*(irow-1)+icol)=ZERO
         END DO
         Xy(Ncxy*(begotl-1)+icol)=ONE
         icol=icol+1
        END IF
c-----------------------------------------------------------------------
c     LS outlier
c-----------------------------------------------------------------------
       ELSE IF(otltyp.eq.LS)THEN
        IF(begotl.gt.Iymx+Nspobs.or.begotl.lt.Iymx+2)THEN
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(Lfatal)RETURN
         IF(begotl.eq.Iymx+Nspobs+1)THEN
          CALL nWritln('Removing '//str(1:nchr)//
     &                 'from the regression because it occurs at the',
     &                 fh2,Mt2,T,F)
          CALL writln('       last data point of the span of the data.',
     &                fh2,Mt2,F,T)
         ELSE IF(begotl.eq.Iymx+1)THEN
          CALL nWritln('Removing '//str(1:nchr)//
     &                 'from the regression because it occurs at the',
     &                 fh2,Mt2,T,F)
          CALL writln(
     &               '       first data point of the span of the data.',
     &                fh2,Mt2,F,T)
         ELSE
          CALL nWritln('Removing '//str(1:nchr)//
     &                 ' from the regression because it is not within',
     &                 fh2,Mt2,T,F)
          CALL writln('       the span of the data.',fh2,Mt2,F,T)
         END IF
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         Endcol=Endcol-1
c     ------------------------------------------------------------------
        ELSE
         DO irow=1,begotl-1
          Xy(Ncxy*(irow-1)+icol)=MONE
         END DO
c     ------------------------------------------------------------------
         DO irow=begotl,Nrxy
          Xy(Ncxy*(irow-1)+icol)=ZERO
         END DO
         icol=icol+1
        END IF
c-----------------------------------------------------------------------
c     Ramp outlier, If both end points are outside the span, drop
c the ramp because either a constant or a slope will be the variable.
c-----------------------------------------------------------------------
       ELSE IF(otltyp.eq.RP)THEN
        IF((endotl.le.begotl).or.(begotl.ge.Iymx+Nspobs).or.
     &     (endotl.le.Iymx+1).or.
     &     (begotl.le.Iymx+1.and.endotl.ge.Iymx+Nspobs))THEN
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Removing '//str(1:nchr)//
     &                ' from the regression because it is not within',
     &                fh2,Mt2,T,F)
         CALL writln('       the span of the data.',fh2,Mt2,F,T)
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         Endcol=Endcol-1
c     ------------------------------------------------------------------
        ELSE
         drmp=dble(endotl-begotl)
         DO irow=1,begotl
          Xy(Ncxy*(irow-1)+icol)=-drmp
c          Xy(Ncxy*(irow-1)+icol)=MONE
         END DO
c     ------------------------------------------------------------------
         DO irow=max(1,begotl+1),min(endotl-1,Nrxy)
          Xy(Ncxy*(irow-1)+icol)=(irow-max(1,begotl+1)+1)-drmp
c          Xy(Ncxy*(irow-1)+icol)=dble(irow-endotl)/drmp
         END DO
c     ------------------------------------------------------------------
         DO irow=endotl,Nrxy
          Xy(Ncxy*(irow-1)+icol)=ZERO
         END DO
         icol=icol+1
        END IF
c-----------------------------------------------------------------------
c     temporary level shift outlier,
c If both end points are outside the span, drop the temporary LS
c because a constant will be the variable.
c-----------------------------------------------------------------------
       ELSE IF(otltyp.eq.TLS)THEN
        IF((endotl.le.begotl).or.(begotl.ge.Iymx+Nspobs).or.
     &     (endotl.le.Iymx+1).or.
     &     (begotl.le.Iymx+1.and.endotl.ge.Iymx+Nspobs))THEN
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Removing '//str(1:nchr)//
     &                ' from the regression because it is not within',
     &                fh2,Mt2,T,F)
         CALL writln('       the span of the data.',fh2,Mt2,F,T)
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         Endcol=Endcol-1
c     ------------------------------------------------------------------
        ELSE
         IF((begotl-1).gt.1)THEN
          DO irow=1,begotl-1
           Xy(Ncxy*(irow-1)+icol)=ZERO
c           Xy(Ncxy*(irow-1)+icol)=MONE
          END DO
         END IF
c     ------------------------------------------------------------------
         DO irow=max(1,begotl),min(endotl,Nrxy)
          Xy(Ncxy*(irow-1)+icol)=ONE
c          Xy(Ncxy*(irow-1)+icol)=dble(irow-endotl)/drmp
         END DO
c     ------------------------------------------------------------------
         IF((endotl+1).lt.Nrxy)THEN
          DO irow=endotl+1,Nrxy
           Xy(Ncxy*(irow-1)+icol)=ZERO
          END DO
         END IF
         icol=icol+1
        END IF
c-----------------------------------------------------------------------
c     TC outlier
c-----------------------------------------------------------------------
       ELSE IF(otltyp.eq.TC)THEN
        IF(begotl.gt.Iymx+Nspobs.or.begotl.lt.Iymx+1)THEN
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Removing '//str(1:nchr)//
     &                ' from the regression because it is not within',
     &                fh2,Mt2,T,F)
         CALL writln('       the span of the data.',fh2,Mt2,F,T)
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         Endcol=Endcol-1
c     ------------------------------------------------------------------
        ELSE
         DO irow=1,begotl-1
          Xy(Ncxy*(irow-1)+icol)=ZERO
         END DO
c     ------------------------------------------------------------------
         Xy(Ncxy*(begotl-1)+icol)=ONE
         DO irow=begotl+1,Nrxy
          Xy(Ncxy*(irow-1)+icol)=Xy(Ncxy*(irow-2)+icol)*Tcalfa
         END DO
         icol=icol+1
        END IF
c-----------------------------------------------------------------------
c     SO outlier
c-----------------------------------------------------------------------
       ELSE IF(otltyp.eq.SO)THEN
        IF(begotl.gt.Iymx+Nspobs.or.begotl.lt.Iymx+1)THEN
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Removing '//str(1:nchr)//
     &                ' from the regression because it is not within',
     &                fh2,Mt2,T,F)
         CALL writln('       the span of the data.',fh2,Mt2,F,T)
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         Endcol=Endcol-1
c     ------------------------------------------------------------------
        ELSE
         DO irow=begotl,Nrxy
          Xy(Ncxy*(irow-1)+icol)=ZERO
         END DO
c     ------------------------------------------------------------------
         imod=mod(begotl,Sp)
         drmp=ONE/DBLE(Sp-1)
         DO irow=begotl-1,1,-1
          IF(mod(irow,Sp).eq.imod)THEN
           Xy(Ncxy*(irow-1)+icol)=MONE
          ELSE
           Xy(Ncxy*(irow-1)+icol)=drmp
          END IF
         END DO
         icol=icol+1
        END IF
c-----------------------------------------------------------------------
c     Quadratic Ramp outlier, increasing rate of change
c If both end points are outside the span, drop
c the ramp because either a constant or a slope will be the variable.
c-----------------------------------------------------------------------
       ELSE IF(otltyp.eq.QI)THEN
        IF((endotl.le.begotl).or.(begotl.ge.Iymx+Nspobs).or.
     &     (endotl.le.Iymx+1).or.
     &     (begotl.le.Iymx+1.and.endotl.ge.Iymx+Nspobs))THEN
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Removing '//str(1:nchr)//
     &                ' from the regression because it is not within'//
     &                ' the span of the data.',STDERR,Mt2,T,T)
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         Endcol=Endcol-1
c     ------------------------------------------------------------------
        ELSE
         drmp=dble(endotl-begotl)
         DO irow=1,begotl
          Xy(Ncxy*(irow-1)+icol)=-(drmp*drmp)
c          Xy(Ncxy*(irow-1)+icol)=MONE
         END DO
c     ------------------------------------------------------------------
         DO irow=max(1,begotl+1),min(endotl-1,Nrxy)
          drow=dble(irow-max(1,begotl+1)+1)
          Xy(Ncxy*(irow-1)+icol)=(drow*drow)-(drmp*drmp)
c          Xy(Ncxy*(irow-1)+icol)=dble(irow-endotl)/drmp
         END DO
c     ------------------------------------------------------------------
         DO irow=endotl,Nrxy
          Xy(Ncxy*(irow-1)+icol)=ZERO
         END DO
         icol=icol+1
        END IF
c-----------------------------------------------------------------------
c Quadratic Ramp outlier, decreasing rate of change
c If both end points are outside the span, drop
c the ramp because either a constant or a slope will be the variable.
c-----------------------------------------------------------------------
       ELSE IF(otltyp.eq.QD)THEN
        IF((endotl.le.begotl).or.(begotl.ge.Iymx+Nspobs).or.
     &     (endotl.le.Iymx+1).or.
     &     (begotl.le.Iymx+1.and.endotl.ge.Iymx+Nspobs))THEN
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Removing '//str(1:nchr)//
     &                ' from the regression because it is not within'//
     &                ' the span of the data.',STDERR,Mt2,T,T)
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         Endcol=Endcol-1
c     ------------------------------------------------------------------
        ELSE
         drmp=dble(endotl-begotl)
         DO irow=1,begotl
          Xy(Ncxy*(irow-1)+icol)=-(drmp*drmp)
c          Xy(Ncxy*(irow-1)+icol)=MONE
         END DO
c     ------------------------------------------------------------------
         DO irow=max(1,begotl+1),min(endotl-1,Nrxy)
          drow=dble(irow-max(1,begotl+1)+1)
          Xy(Ncxy*(irow-1)+icol)=-((drmp-drow)*(drmp-drow))
c          Xy(Ncxy*(irow-1)+icol)=dble(irow-endotl)/drmp
         END DO
c     ------------------------------------------------------------------
         DO irow=endotl,Nrxy
          Xy(Ncxy*(irow-1)+icol)=ZERO
         END DO
         icol=icol+1
        END IF
       ELSE
c-----------------------------------------------------------------------
c     Not an outlier
c-----------------------------------------------------------------------
        CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
        IF(Lfatal)RETURN
        CALL eWritln(str(1:nchr)//
     &            ' not an AO, LS, TC, TL, SO, QD, QI or ramp outlier.',
     &               STDERR,Mt2,T,T)
        CALL abend
        RETURN
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
