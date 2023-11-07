      SUBROUTINE mkotky(Ibeg,Iend,Otlidx,Ttst)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c    Create key of observations for whom the t-statistic in the
c    outlier tables have been set to zero (BCM March 2008)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'fxreg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c-----------------------------------------------------------------------
      CHARACTER tmpttl*(PCOLCR),thisot*(2),thisdt*(10),outstr*(80)
      DOUBLE PRECISION Ttst
      LOGICAL locok
      INTEGER Ibeg,Iend,Otlidx,ityp1,ityp2,ndates,icol,ntmpcr,
     &        otltyp,t0,itmp,zrodat,idate,i,i1,i2,i3,ndt,nout
      DIMENSION Ttst(PLEN,POTLR),zrodat(PB),idate(2)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      IF(Nb.eq.0)RETURN
      ndates=0
c-----------------------------------------------------------------------
      IF(Otlidx.eq.AO)THEN
       ityp1=PRGTAO
       ityp2=PRGTAA
       thisot='AO'
      ELSE IF(Otlidx.eq.LS)THEN
       ityp1=PRGTLS
       ityp2=PRGTAL
       thisot='LS'
      ELSE IF(Otlidx.eq.TC)THEN
       ityp1=PRGTTC
       ityp2=PRGTAT
       thisot='TC'
      END IF
c-----------------------------------------------------------------------
      DO icol=1,Nb
       IF(Rgvrtp(icol).eq.ityp1.or.Rgvrtp(icol).eq.ityp2)THEN
        CALL getstr(Colttl,Colptr,Ncoltl,icol,tmpttl,ntmpcr)
        IF(.not.Lfatal)THEN
         CALL rdotlr(tmpttl(1:ntmpcr),Begspn,Sp,otltyp,t0,itmp,locok)
         IF(.not.locok)CALL abend()
        END IF
        IF(Lfatal)RETURN
        IF(dpeq(Ttst(t0,Otlidx),ZERO).and.(t0.ge.Ibeg.and.
     &     t0.le.Iend))THEN
         ndates=ndates+1
         zrodat(ndates)=t0
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
c     If some outliers are being held fixed, need to search fixed
c     outliers to determine if they are within testing range
c-----------------------------------------------------------------------
      IF(Iregfx.ge.2)THEN
       DO icol=1,Nfxttl
        IF(Fxtype(icol).eq.ityp1.or.Fxtype(icol).eq.ityp2)THEN
         CALL getstr(Cfxttl,Cfxptr,Nfxttl,icol,tmpttl,ntmpcr)
         IF(.not.Lfatal)THEN
          CALL rdotlr(tmpttl(1:ntmpcr),Begspn,Sp,otltyp,t0,itmp,locok)
          IF(.not.locok)CALL abend()
         END IF
         IF(Lfatal)RETURN
         IF(t0.ge.Ibeg.and.t0.le.Iend)THEN
          ndates=ndates+1
          zrodat(ndates)=t0
         END IF
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
c     Print out dates
c-----------------------------------------------------------------------
      IF(ndates.gt.0)THEN
       CALL mkPOneLine(Mt1,'@',
     &                 thisot//' Outlier t-values have been set to '//
     &                 'zero for the following observations:')
       CALL mkPClass(Mt1,'indent')
       i1=1
       i2=min(ndates,7)
       DO WHILE (i2.le.ndates)
        nout=6
        CALL setchr(' ',80,outstr)
        do i=i1,i2
         CALL setchr(' ',10,thisdt)
         CALL addate(Begspn,Sp,zrodat(i)-1,idate)
         CALL wrtdat(idate,Sp,thisdt,ndt)
         i3=((i-i1)*10)+8
         outstr(i3:(i3+ndt-1))=thisdt(1:ndt)
         nout=nout+10
        END DO
        write(Mt1,1020)outstr(1:nout)
        if (i2.eq.ndates) then
         i2=i2+1
        ELSE
         i1=i2+1
         i2=min(i2+7,ndates)
         nout=6
        END IF
       END DO
       CALL writTag(Mt1,'</p>')
      END IF
c-----------------------------------------------------------------------
 1020 FORMAT(a)
c-----------------------------------------------------------------------
      RETURN
      END
