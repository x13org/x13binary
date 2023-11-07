C     Last change:  BCM  19 Jun 2002    5:41 pm
      SUBROUTINE rmotrv(Begxy,Begrev,Nrxy,Botr,Otrptr,Notrtl,Fixotr,
     &                  Otrttl,Lotlrv,Othndl,Lprt,Lsav,Lhdr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine removes additive outliers from the regression 
c     variables if they occur after the starting date of the revision
c     history analysis.  These outliers will be saved so that they can
c     be reentered into the regression matrix when there is enough data.
c-----------------------------------------------------------------------
      CHARACTER CPLUS*1
      PARAMETER(CPLUS='+')
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'cchars.i'
c-----------------------------------------------------------------------
      CHARACTER outstr*(PCOLCR*PB),datstr*(10),str*(PCOLCR),
     &          Otrttl*(PCOLCR*PB)
      LOGICAL locok,Lprt,Lsav,Lhdr,Fixotr,Lotlrv
      INTEGER Begxy,otltyp,begotl,endotl,icol,Nrxy,Begrev,nchr,nreg,
     &        Othndl,curdat,notlr,rtype,nchdat,nstr,Otrptr,Notrtl,n0,n1,
     &        n2,limchr
      DOUBLE PRECISION Botr
      DIMENSION Otrptr(0:PB),Botr(PB),Fixotr(PB),Begxy(2),curdat(2)
c-----------------------------------------------------------------------
      nreg=Notrtl+1
      notlr=0
      nchr=1
c-----------------------------------------------------------------------
c     Get character string for date of current observation.
c-----------------------------------------------------------------------
      IF(Lsav.or.Lprt)THEN
       CALL addate(Begxy,Sp,Begrev-1,curdat)
       CALL wrtdat(curdat,Sp,datstr,nchdat)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Check outliers to see if any fall after the start of the
c     revisions history analysis
c-----------------------------------------------------------------------
      icol=Nb
      endotl=0
      DO WHILE (icol.ge.1)
       rtype=Rgvrtp(icol)
       IF(rtype.eq.PRGTAO.or.rtype.eq.PRGTLS.or.rtype.eq.PRGTRP.or.
     &    rtype.eq.PRGTAA.or.rtype.eq.PRGTAL.or.rtype.eq.PRGTTC.or.
     &    rtype.eq.PRGTQD.or.rtype.eq.PRGTQI.or.
     &    rtype.eq.PRGTAT.or.rtype.eq.PRGTSO.or.rtype.eq.PRGTTL)THEN
        CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nstr)
        IF(Lfatal)RETURN
        CALL rdotlr(str(1:nstr),Begxy,Sp,otltyp,begotl,endotl,locok)
        IF(.not.locok)THEN
         CALL abend
         RETURN
        END IF
c-----------------------------------------------------------------------
        IF(((otltyp.eq.RP.or.otltyp.eq.TLS).and.(begotl.gt.Begrev.or.
     &       endotl.gt.Begrev)).or.
     &     ((otltyp.ne.RP.and.otltyp.ne.TLS).and.begotl.gt.Begrev))THEN
c-----------------------------------------------------------------------
c     Save the outlier and parameter estimate so that it can be 
c     added back to the regression matrix later.
c-----------------------------------------------------------------------
         IF(Lotlrv)THEN
          CALL insstr(str(1:nstr),nreg,PB,Otrttl,Otrptr,Notrtl)
          IF(Lfatal)RETURN
          Botr(Notrtl)=B(icol)
          Fixotr(Notrtl)=Regfx(icol)
          nreg=nreg+1
         END IF
c-----------------------------------------------------------------------
c     Delete outlier if it occurs after the start of the revision
c     history analysis
c-----------------------------------------------------------------------
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
         notlr=notlr+1
c-----------------------------------------------------------------------
c     update character string of outliers deleted.  First, print header,
c     if necessary
c-----------------------------------------------------------------------
         IF(Lhdr)THEN
          CALL rvrghd(Othndl,Mt1,Lsav,Lprt)
          IF(Lfatal)RETURN
          Lhdr=.false.
         END IF
c-----------------------------------------------------------------------
         IF(Lprt.or.Lsav)THEN
          IF(nchr.gt.1)THEN
           outstr(nchr:nchr)=CPLUS
           nchr=nchr+1
          END IF
c-----------------------------------------------------------------------
          outstr(nchr:(nchr+nstr-1))=str(1:nstr)
          nchr=nchr+nstr
         END IF
        END IF
       END IF
       icol=icol-1
c-----------------------------------------------------------------------
      END DO
c-----------------------------------------------------------------------
c     Print out and/or save outliers that were deleted, if any
c-----------------------------------------------------------------------
      IF(Lsav.or.Lprt)THEN
       nchr=nchr-1
       IF(Lprt.and.nchr.gt.0)THEN
        limchr=132
        IF(nchr.le.limchr)THEN
         WRITE(Mt1,1020)datstr(1:nchdat),'deleted',outstr(1:nchr)
        ELSE
         n0=1
         n1=1
         n2=1
         DO WHILE (n2.gt.0)
          n2=index(outstr(n1:nchr),'+')
          IF((n2+n1-n0+35).gt.limchr)THEN
           IF(n0.eq.1)THEN
            WRITE(Mt1,1030)datstr(1:nchdat),'deleted',outstr(n0:n1-1)
           ELSE
            WRITE(Mt1,1040)outstr(n0:n1-1)
           END IF
           n0=n1
          END IF
          n1=n2+n1
         END DO
         WRITE(Mt1,1040)outstr(n0:nchr)
         WRITE(Mt1,1040)'</td></tr>'
        END IF
       END IF
       IF(Lsav)THEN
        IF(nchr.eq.0)THEN
         outstr(1:4)='none'
         nchr=nchr+3
         IF(Lhdr)THEN
          CALL rvrghd(Othndl,Mt1,Lsav,Lprt)
          IF(Lfatal)RETURN
          Lhdr=.false.
         END IF
        END IF
        WRITE(Othndl,1010)datstr(1:nchdat),TABCHR,'deleted',TABCHR,
     &                    outstr(1:nchr)
       END IF
      END IF
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(a,a,a,a,a)
 1020 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',a,
     &       '</td><td class="left">',a,'</td></tr>')
 1030 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',a,
     &       '</td><td class="left">',a)
 1040 FORMAT(a)
      END
