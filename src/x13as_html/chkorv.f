C     Last change:  BCM  22 Sep 2003    7:05 am
      SUBROUTINE chkorv(Begxy,Endrev,Botr,Otrptr,Notrtl,Fixotr,Otrttl,
     &                  Othndl,Otlfix,Nrxy,Lprt,Lsav,Lhdr,Lmdl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Routine that checks the outliers saved by rmaootl to see if they
c     can be reentered into the regression matrix.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'ssprep.cmn'
      INCLUDE 'cchars.i'
c-----------------------------------------------------------------------
      CHARACTER CPLUS*(1)
      LOGICAL T,F
      PARAMETER(CPLUS='+',T=.true.,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER str*(PCOLCR),outstr*(PCOLCR*PB),datstr*(10),
     &          str2*(PCOLCR),lstr*(PCOLCR),Otrttl*(PCOLCR*PB)
      LOGICAL locok,Lprt,Lsav,Lhdr,update,Fixotr,Lmdl,Otlfix,fx,lastLS
      DOUBLE PRECISION Botr
      INTEGER Begxy,otltyp,begotl,endotl,endcol,icol,Endrev,i,nchr,nout,
     &        otypvc,curdat,nchr2,Othndl,nchdat,Otrptr,Notrtl,nlast,
     &        Nrxy,rtype,delcol,ltype,ilast,lcol,opref,lchr,jchr
      DIMENSION otypvc(9),Begxy(2),curdat(2),Otrptr(0:PB),opref(7),
     &          Botr(PB),Fixotr(PB)
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
C-----------------------------------------------------------------------
      DATA otypvc/PRGTAO,PRGTLS,PRGTTC,PRGTRP,PRGTAO,PRGTTL,PRGTSO,
     &            PRGTQI,PRGTQD/
      DATA opref/1,4,2,0,0,0,3/
c-----------------------------------------------------------------------
      endcol=Notrtl
      nout=1
      icol=1
      update=F
      endotl=PLEN+1
      nlast=0
      lastLS=F
      DO WHILE (icol.le.endcol)
       CALL getstr(Otrttl,Otrptr,Notrtl,icol,str,nchr)
       IF(Lfatal)RETURN
       CALL rdotlr(str(1:nchr),Begxy,Sp,otltyp,begotl,endotl,locok)
c-----------------------------------------------------------------------
c     Check to see if AO outlier is defined. If so, then add outlier
c     to regression
c-----------------------------------------------------------------------
       IF(((otltyp.eq.RP.or.otltyp.eq.TLS.or.otltyp.eq.QI.or.
     &       otltyp.eq.QD).and.(begotl.le.Endrev.and.
     &    endotl.le.Endrev)).or.((otltyp.ne.RP.and.otltyp.ne.TLS.or.
     &       otltyp.ne.QI.or.otltyp.ne.QD).and.
     &    begotl.le.Endrev))THEN
        fx=Fixotr(icol).or.Otlfix
        CALL adrgef(Botr(icol),str(1:nchr),str(1:nchr),otypvc(otltyp),
     &              fx,F)
        IF(Lfatal)RETURN
        IF(.not.update)update=T
        IF(Iregfx.eq.3.and.(.not.fx))Iregfx=2
c-----------------------------------------------------------------------
c  check to see if outlier being added is in last observation.
c  update lastLS and nlast if this is so.
c-----------------------------------------------------------------------
        IF((otltyp.ne.RP.and.otltyp.ne.TLS.and.otltyp.ne.QI.and.
     &      otltyp.ne.QD).and.(begotl.eq.Endrev))THEN
         nlast=nlast+1
         IF(.not.lastLS)lastLS=otltyp.eq.LS
         icol=icol+1
        ELSE
c-----------------------------------------------------------------------
c     delete outlier from the AO data dictionary
c-----------------------------------------------------------------------
         CALL delstr(icol,Otrttl,Otrptr,Notrtl,PB)
         IF(Lfatal)RETURN
c----------------------------------------------------------------------
c     Update AO beta and fix vector, endcol
c----------------------------------------------------------------------
         IF(icol.lt.endcol)THEN
          DO i=icol+1,endcol
           Botr(i-1)=Botr(i)
           Fixotr(i-1)=Fixotr(i)
          END DO
         END IF
         endcol=endcol-1
c-----------------------------------------------------------------------
c     Store outlier being added to regression. Initialize outstr.
c-----------------------------------------------------------------------
         IF(nout.eq.1)THEN
          CALL addate(Begxy,Sp,Endrev-1,curdat)
          CALL wrtdat(curdat,Sp,datstr,nchdat)
          IF(Lfatal)RETURN
          nchr2=1
         ELSE
          outstr(nchr2:nchr2)=CPLUS
          nchr2=nchr2+1
         END IF
         outstr(nchr2:(nchr+nchr2-1))=str(1:nchr)
         nchr2=nchr+nchr2
         nout=nout+1
        END IF
       ELSE
c----------------------------------------------------------------------
c     Else, update counter
c----------------------------------------------------------------------
        icol=icol+1
       END IF
c----------------------------------------------------------------------
      END DO
c-----------------------------------------------------------------------
c   if more than one outlier appears on the final observation, delete
c   outliers that will cause singularities in the regression matrix.
c-----------------------------------------------------------------------
      IF(nlast.gt.0)THEN
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
         CALL getstr(Colttl,Colptr,Nb,icol,str2,jchr)
         IF(Lfatal)RETURN
         CALL rdotlr(str2(1:jchr),Begxy,Sp,otltyp,begotl,endotl,locok)
         IF(otltyp.ne.RP.and.begotl.eq.Endrev)THEN
          ilast=ilast+1
          IF(ilast.eq.1)THEN
           ltype=otltyp
           lcol=icol
           lstr(1:jchr)=str2(1:jchr)
           lchr=jchr
          ELSE
           IF(opref(ltype).lt.opref(otltyp))THEN
            CALL dlrgef(icol,Nrxy,1)
            IF(Lfatal)RETURN
            IF(ilast.lt.nlast)lcol=lcol-1
           ELSE
            CALL dlrgef(lcol,Nrxy,1)
            IF(Lfatal)RETURN
            ltype=otltyp
            lcol=icol
            lstr(1:jchr)=str2(1:jchr)
            lchr=jchr
           END IF
          END IF
         END IF
        END IF
        icol=icol-1
       END DO
c-----------------------------------------------------------------------
c     Remove outlier from AO data dictionary
c-----------------------------------------------------------------------
       delcol=strinx(T,Otrttl,Otrptr,1,Notrtl,lstr(1:lchr))
       IF(delcol.lt.Notrtl)THEN
        DO i=delcol+1,Notrtl
         Botr(i-1)=Botr(i)
         Fixotr(i-1)=Fixotr(i)
        END DO
       END IF
       CALL delstr(delcol,Otrttl,Otrptr,Notrtl,PB)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Store outlier being added to regression. Initialize outstr.
c-----------------------------------------------------------------------
       IF(nout.eq.1)THEN
        CALL addate(Begxy,Sp,Endrev-1,curdat)
        CALL wrtdat(curdat,Sp,datstr,nchdat)
        IF(Lfatal)RETURN
        nchr2=1
       ELSE
        outstr(nchr2:nchr2)=CPLUS
        nchr2=nchr2+1
       END IF
       outstr(nchr2:(lchr+nchr2-1))=lstr(1:lchr)
       nchr2=lchr+nchr2
       nout=nout+1
      END IF
c-----------------------------------------------------------------------
c     Update stored model parameters with newly stored regressor
c-----------------------------------------------------------------------
      IF(update.and.Lmdl)THEN
       Ngr2=Ngrp
       Ngrt2=Ngrptl
       Ncxy2=Ncxy
       Nbb=Nb
       Nct2=Ncoltl
       Cttl=Colttl
       Gttl=Grpttl
       CALL cpyint(Colptr(0),PB+1,1,Clptr(0))
       CALL cpyint(Grp(0),PGRP+1,1,G2(0))
       CALL cpyint(Grpptr(0),PGRP+1,1,Gptr(0))
       CALL copy(B,PB,1,Bb)
       CALL cpyint(Rgvrtp,PB,1,Rgv2)
      END IF
c-----------------------------------------------------------------------
      IF(nout.gt.1.and.(Lsav.or.Lprt))THEN
       IF(Lhdr)THEN
        CALL rvrghd(Othndl,Mt1,Lsav,Lprt)
        IF(Lfatal)RETURN
        Lhdr=F
       END IF
       IF(Lprt.and.nchr.gt.0)THEN
        CALL writTag(Mt1,'<tr>')
        WRITE(Mt1,1030)datstr(1:nchdat),'added',outstr(1:nchr2-1)
        CALL writTag(Mt1,'</tr>')
       END IF
       IF(Lsav)WRITE(Othndl,1010)datstr(1:nchdat),TABCHR,'added',
     &                           TABCHR,outstr(1:nchr2-1)
      END IF
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(a,a,a,a,a)
 1030 FORMAT('<th scope="row">',a,'</th><td class="center">',a,
     &       '</td><td class="left">',a,'</td>')
      END
