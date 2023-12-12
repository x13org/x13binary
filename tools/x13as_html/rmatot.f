C     Last change:  BCM  30 Jun 1998    8:46 am
      SUBROUTINE rmatot(Nrxy,Otlrev,Otlwin,Begrev,Begxy,Othndl,Otlfix,
     &                  Lprt,Lsav,Lhdr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Outliers automatically identified in a previous run will be 
c     removed from the regression model if they fall within the 
c     "window" given by Otlwin
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'ssprep.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'cchars.i'
c-----------------------------------------------------------------------
      CHARACTER CPLUS*(1)
      LOGICAL T,F
      PARAMETER(CPLUS='+',T=.true.,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER atrttl*(PCOLCR*PB),datstr*(10),rmrttl*(PCOLCR*PB),
     &          outstr*(PCOLCR*PB),str*(PCOLCR)
      LOGICAL locok,Lsav,Lprt,Lhdr,update,vfix,Otlfix
      DOUBLE PRECISION batr
      INTEGER atrptr,natrtl,icol,Othndl,Nrxy,Otlrev,Otlwin,Begrev,vtype,
     &        rmrptr,nrmrtl,end2,nreg,nchr,nchr2,otltyp,i,nauto,begotl,
     &        endotl,Begxy,curdat,nrm,nchdat,rtype,iotlr,limchr,n1,n2,n0
      DIMENSION atrptr(0:PB),rmrptr(0:PB),batr(PB),Begxy(2),curdat(2),
     &          vtype(PB),vfix(PB)
c-----------------------------------------------------------------------
c     Initialize outlier dictionaries and pointer variables.
c-----------------------------------------------------------------------
      CALL intlst(PB,atrptr,natrtl)
      CALL intlst(PB,rmrptr,nrmrtl)
      nreg=natrtl+1
      nrm=nrmrtl+1
      nauto=0
c-----------------------------------------------------------------------
      DO i=Nb,1,-1
       rtype=Rgvrtp(i)
c-----------------------------------------------------------------------
c     If automatic outlier detection is to be done, get outlier
c     information.
c-----------------------------------------------------------------------
*       IF(rtype.eq.PRGTAA.or.rtype.eq.PRGTAL.or.rtype.eq.PRGTAT.or.
*     &    rtype.eq.PRGTAS)THEN
       IF(rtype.eq.PRGTAA.or.rtype.eq.PRGTAL.or.rtype.eq.PRGTAT)THEN
        CALL getstr(Colttl,Colptr,Ncoltl,i,str,nchr)
        IF(Lfatal)RETURN
        CALL rdotlr(str(1:nchr),Begxy,Sp,otltyp,begotl,endotl,locok)
        IF(.not.locok)THEN
         CALL abend
         RETURN
        END IF
        IF(Otlrev.ge.2)THEN
c-----------------------------------------------------------------------
c     Determine if the outlier is to be saved, ie, does it occur before 
c     the start of the revision history analysis.
c-----------------------------------------------------------------------
         end2=Begrev-Otlwin
         IF(begotl.le.end2)THEN
          CALL insstr(str(1:nchr),nreg,PB,atrttl,atrptr,natrtl)
          IF(Lfatal)RETURN
          batr(natrtl)=B(i)
          IF(otltyp.eq.TC.or.otltyp.eq.SO)THEN
           vtype(natrtl)=rtype-1
          ELSE
           vtype(natrtl)=rtype-3
          END IF
          vfix(natrtl)=Regfx(i).or.Otlfix
          nreg=nreg+1
         ELSE
          CALL insstr(str(1:nchr),nrm,PB,rmrttl,rmrptr,nrmrtl)
          IF(Lfatal)RETURN
          nrm=nrm+1
         END IF
        ELSE
         CALL insstr(str(1:nchr),nrm,PB,rmrttl,rmrptr,nrmrtl)
         IF(Lfatal)RETURN
         nrm=nrm+1
        END IF
c-----------------------------------------------------------------------
c     Delete automatic outlier 
c-----------------------------------------------------------------------
        iotlr=i
        CALL dlrgef(iotlr,Nrxy,1)
        IF(Lfatal)RETURN
        nauto=nauto+1
       END IF
      END DO
c-----------------------------------------------------------------------
c     If automatically identified outliers have been saved, reenter 
c     them as regular outliers.
c-----------------------------------------------------------------------
      IF(nauto.eq.0)RETURN
      IF(Otlrev.ge.2.and.natrtl.gt.0)THEN
       update=F
       DO icol=1,natrtl
        CALL getstr(atrttl,atrptr,natrtl,icol,str,nchr)
        IF(.not.Lfatal)CALL adrgef(batr(icol),str(1:nchr),str(1:nchr),
     &                             vtype(icol),vfix(icol),F)
        IF(Lfatal)RETURN
        IF(.not.update)update=T
       END DO
       IF(update)THEN
c-----------------------------------------------------------------------
c     Update stored model parameters with newly stored regressor
c-----------------------------------------------------------------------
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
        CALL copylg(Regfx,PB,1,Regfx2)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Store outlier variables in special file.  First, determine date.
c-----------------------------------------------------------------------
      CALL addate(Begxy,Sp,Begrev-1,curdat)
      CALL wrtdat(curdat,Sp,datstr,nchdat)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print out headers, if necessary
c-----------------------------------------------------------------------
      IF(Lhdr.and.((nrmrtl.gt.0.AND.Otlrev.eq.1.or.Otlrev.eq.3).or.
     &   natrtl.gt.0))THEN
       CALL rvrghd(Othndl,Mt1,Lsav,Lprt)
       IF(Lfatal)RETURN
       Lhdr=F
      END IF
c-----------------------------------------------------------------------
c     Print out outliers that were kept, if any
c-----------------------------------------------------------------------
      IF(Lprt.or.Lsav)THEN
       limchr=132
       nchr=1
       IF(natrtl.gt.0)THEN
        DO i=1,natrtl
         CALL getstr(atrttl,atrptr,natrtl,i,str,nchr2)
         IF(Lfatal)RETURN
         outstr(nchr:(nchr+nchr2-1))=str(1:nchr2)
         nchr=nchr+nchr2
         IF(i.lt.natrtl)THEN
          outstr(nchr:nchr)=CPLUS
          nchr=nchr+1
         END IF
        END DO
       END IF
       nchr=nchr-1
       IF(Lprt.and.nchr.gt.0)THEN
        WRITE(Mt1,1020)datstr(1:nchdat),'kept'
        if(nchr.lt.limchr)THEN
         WRITE(Mt1,1030)outstr(1:nchr)
        ELSE
         n1=1
         n2=limchr
         DO WHILE (n2.lt.nchr)
          DO WHILE (outstr(n2:n2).ne."+")
           n2=n2-1
          END DO
          WRITE(Mt1,1040)outstr(1:n2)
          n1=n2+1
          n2=n2+limchr
         END DO
         WRITE(Mt1,1050)outstr(n1:nchr)
        END IF
       END IF
       IF(Lsav)THEN
        IF(nchr.eq.0)THEN
         outstr(1:4)='none'
         nchr=nchr+4
        END IF
        WRITE(Othndl,1010)datstr(1:nchdat),TABCHR,'kept',TABCHR,
     &                    outstr(1:nchr)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print out outliers that were deleted, if any
c-----------------------------------------------------------------------
      IF(Lsav.or.Lprt)THEN
       nchr=1
       IF(nrmrtl.gt.0)THEN
        DO i=1,nrmrtl
         CALL getstr(rmrttl,rmrptr,nrmrtl,i,str,nchr2)
         IF(Lfatal)RETURN
         outstr(nchr:(nchr+nchr2-1))=str(1:nchr2)
         nchr=nchr+nchr2
         IF(i.lt.nrmrtl)THEN
          outstr(nchr:nchr)=CPLUS
          nchr=nchr+1
         END IF
        END DO
       END IF
       nchr=nchr-1
       IF(Lprt.and.nchr.gt.0)THEN
        WRITE(Mt1,1020)datstr(1:nchdat),'deleted(auto)'
        if(nchr.lt.limchr)THEN
         WRITE(Mt1,1030)outstr(1:nchr)
        ELSE
         n1=1
         n2=limchr
         DO WHILE (n2.lt.nchr)
          DO WHILE (outstr(n2:n2).ne."+")
           n2=n2-1
          END DO
          WRITE(Mt1,1040)outstr(1:n2)
          n1=n2+1
          n2=n2+limchr
         END DO
         WRITE(Mt1,1050)outstr(n1:nchr)
        END IF
       END IF
       IF(Lsav)THEN
        IF(nchr.eq.0)THEN
         outstr(1:4)='none'
         nchr=nchr+4
         IF(Lhdr)THEN
          CALL rvrghd(Othndl,Mt1,Lsav,Lprt)
          IF(Lfatal)RETURN
          Lhdr=F
         END IF
        END IF
        WRITE(Othndl,1010)datstr(1:nchdat),TABCHR,'deleted(auto)',
     &                    TABCHR,outstr(1:nchr)
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(a,a,a,a,a)
 1020 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',a,
     &       '</td>')
 1030 FORMAT('<td class="left">',a,'</td></tr>')
 1040 FORMAT('<td class="left">',a)
 1050 FORMAT(a,'</td></tr>')
      END
      