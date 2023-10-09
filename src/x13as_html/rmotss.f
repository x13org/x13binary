C     Last change:  BCM   3 Sep 2003    3:59 pm
      SUBROUTINE rmotss(Icol,Begxy,Nrxy,Strtss,Starta,Enda,Botr,Otrptr,
     &                  Notrtl,Fixotr,Otrttl,Otlfix,Revchg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Remove and store outliers before a sliding spans run.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER str*(PCOLCR),Otrttl*(PCOLCR*PB)
      DOUBLE PRECISION Botr
      LOGICAL Otlfix,locok,Revchg,Fixotr
      INTEGER nchr,Icol,otltyp,begotl,endotl,Strtss,Starta,Enda,sspos,
     &        sspos1,sspos2,nreg,Begxy,Nrxy,Otrptr,Notrtl
      DIMENSION Otrptr(0:PB),Botr(PB),Fixotr(PB),Strtss(2),Starta(2),
     &          Enda(2),Begxy(2)
c-----------------------------------------------------------------------
      nreg=Notrtl+1
      CALL getstr(Colttl,Colptr,Ncoltl,Icol,str,nchr)
      IF(Lfatal)RETURN
      CALL rdotlr(str(1:nchr),Begxy,Sp,otltyp,begotl,endotl,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Delete the outlier if it falls before the start of the first 
c     sliding span.
c-----------------------------------------------------------------------
      CALL dfdate(Strtss,Begxy,Sp,sspos)
      sspos=sspos+1
      IF(begotl.lt.sspos)THEN
       IF(.not.Revchg)Revchg=.true.
       CALL dlrgef(Icol,Nrxy,1)
       RETURN
      END IF
c-----------------------------------------------------------------------
c     If outlier will be undefined in one or more spans, save the 
c     outlier and parameter estimate so it can be added back to the
c     regression matrix later, then delete the outlier variable.
c-----------------------------------------------------------------------
      CALL dfdate(Starta,Begxy,Sp,sspos1)
      sspos1=sspos1+1
      CALL dfdate(Enda,Begxy,Sp,sspos2)
      sspos2=sspos2+1
      IF(
     & (otltyp.eq.AO.and.((begotl.lt.sspos1).or.(begotl.gt.sspos2))).or.
     & (otltyp.eq.LS.and.((begotl.le.sspos1).or.(begotl.ge.sspos2))).or.
     & (otltyp.eq.SO.and.((begotl.le.sspos1).or.(begotl.ge.sspos2))).or.
     & (otltyp.eq.TC.and.((begotl.lt.sspos1).or.(begotl.gt.sspos2)))
     & .or.
     & (otltyp.eq.RP.and.(.not.(begotl.gt.sspos1.and.begotl.lt.sspos2)))
     &   )THEN
       Revchg=.true.
       CALL insstr(str(1:nchr),nreg,PB,Otrttl,Otrptr,Notrtl)
       IF(Lfatal)RETURN
       Botr(Notrtl)=B(Icol)
       Fixotr(Notrtl)=Regfx(Icol).or.Otlfix
       CALL dlrgef(Icol,Nrxy,1)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
