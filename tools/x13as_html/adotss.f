C     Last change:  BCM   7 Oct 2003    3:53 pm
      SUBROUTINE adotss(Botr,Otrptr,Notrtl,Fixotr,Otrttl,Lastsy,Otlfix)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER Otrttl*(PCOLCR*PB),str*(PCOLCR)
      LOGICAL Fixotr,locok,Otlfix,fx,lastLS
      DOUBLE PRECISION Botr
      INTEGER Otrptr,Notrtl,icol,nch,endcol,otltyp,begotl,endotl,rtype,
     &        Lastsy,otypvc,otlind,nlast,ltype,ilast,lcol,opref
      DIMENSION Otrptr(0:PB),Botr(PB),Fixotr(PB),otypvc(9),opref(7)
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
      DATA otypvc/PRGTAO,PRGTLS,PRGTTC,PRGTRP,PRGTAO,PRGTTL,PRGTSO,
     &            PRGTQI,PRGTQD/
      DATA opref/1,4,2,0,0,0,3/
c-----------------------------------------------------------------------
      endcol=Notrtl
      icol=1
      nlast=0
      lastLS=F
      DO WHILE(icol.le.endcol)
       CALL getstr(Otrttl,Otrptr,Notrtl,icol,str,nch)
       IF(Lfatal)RETURN
       otlind=strinx(T,Grpttl,Grpptr,1,Ngrptl,str(1:nch))
       IF(otlind.eq.0)THEN
        CALL rdotlr(str(1:nch),Begsrs,Sp,otltyp,begotl,endotl,locok)
c-----------------------------------------------------------------------
c     Check to see if outlier is defined. If so, then add outlier
c     to regression.
c-----------------------------------------------------------------------
        IF(((otltyp.eq.RP.or.otltyp.eq.TLS.or.otltyp.eq.QI.or.
     &       otltyp.eq.QD).and.(begotl.ge.Frstsy.and.
     &     endotl.le.Lastsy)).or.
     &    ((otltyp.eq.SO.or.otltyp.eq.LS).and.
     &     (begotl.gt.Frstsy.and.begotl.le.Lastsy)).or.
     &    ((otltyp.eq.AO.or.otltyp.eq.TC).and.
     &     (begotl.ge.Frstsy.and.begotl.le.Lastsy)))
     &     THEN
         fx=Fixotr(icol).or.Otlfix
         CALL adrgef(Botr(icol),str(1:nch),str(1:nch),otypvc(otltyp),
     &               fx,F)
         IF(Iregfx.eq.3.and.(.not.fx))Iregfx=2
         IF((otltyp.ne.RP.and.otltyp.ne.TLS.or.otltyp.ne.QI.or.
     &       otltyp.ne.QD).and.(begotl.eq.Lastsy))THEN
          nlast=nlast+1
          IF(.not.lastLS)lastLS=otltyp.eq.LS
         END IF
        END IF
        IF(Lfatal)RETURN
       END IF
       icol=icol+1
      END DO
c-----------------------------------------------------------------------
c   if more than one outlier appears on the final observation, delete
c   outliers that will cause singularities in the regression matrix.
c-----------------------------------------------------------------------
      IF(nlast.gt.1)THEN
       icol=Nb
       ltype=0
       ilast=0
       lcol=0
       DO WHILE(icol.ge.1)
        rtype=Rgvrtp(icol)
        IF(rtype.eq.PRGTAO.or.rtype.eq.PRGTAA.or.rtype.eq.PRGTLS.or.
     &     rtype.eq.PRGTAL.or.rtype.eq.PRGTTC.or.rtype.eq.PRGTAT.or.
*     &     rtype.eq.PRGTSO.or.rtype.eq.PRGTAS)THEN
     &     rtype.eq.PRGTSO)THEN
         CALL getstr(Colttl,Colptr,Nb,icol,str,nch)
         IF(Lfatal)RETURN
         CALL rdotlr(str(1:nch),Begsrs,Sp,otltyp,begotl,endotl,locok)
         IF((otltyp.ne.RP.and.otltyp.ne.TLS.or.otltyp.ne.QI.or.
     &       otltyp.ne.QD).and.begotl.eq.Lastsy)THEN
          ilast=ilast+1
          IF(ilast.eq.1)THEN
           ltype=otltyp
           lcol=icol
          ELSE
           IF(opref(ltype).lt.opref(otltyp))THEN
            CALL dlrgef(icol,Nrxy,1)
            IF(Lfatal)RETURN
           ELSE
            CALL dlrgef(lcol,Nrxy,1)
            IF(Lfatal)RETURN
            ltype=otltyp
            lcol=icol
           END IF
          END IF
         END IF
        END IF
        icol=icol-1
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
