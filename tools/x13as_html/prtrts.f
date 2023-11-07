C     Last change:  BCM  25 Jun 1998   10:10 am
      SUBROUTINE prtrts(Lprt,Lsav,Lsvlg,Ldiag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Prints out the roots of phi(B)=0 and theta(B)=0; each root has
c four components: Real, Imaginary, Module, and Frequency
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c degree  i  Maximum lag of phi(B) or theta(B)
c degp1   i  degree + 1
c coeff   d  Coefficients of phi(B) or theta(B) in order of increasing
c             powers
c rcoef   d  Coefficients of phi(B) or theta(B) in order of decreasing
c             powers
c zeror   d  Real part of the roots
c zeroi   d  Imaginary part of the roots
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      CHARACTER lower1*(1),lower2*(4),tmpttl*(POPRCR),outstr*(100)
      LOGICAL allinv,Lprt,Lsav,Lsvlg,Ldiag,fcnok
      INTEGER i,i2,k,beglag,begopr,endlag,endopr,factor,iflt,ilag,iopr,
     &        ntmpcr,degree,spchr,fh,ipos
      DOUBLE PRECISION coeff(PORDER+1),zeror(PORDER),zeroi(PORDER),
     &                 zerom(PORDER),zerof(PORDER)
c-----------------------------------------------------------------------
c     Print out the roots of phi(B)=0 and theta(B)=0 with AR part first
c-----------------------------------------------------------------------
      lower1=' '
      lower2=' '
      begopr=Mdl(AR-1)
      beglag=Opr(begopr-1)
      endopr=Mdl(MA)-1
c     ------------------------------------------------------------------
c     Nov 2005 BCM - add statement to avoid printing out root table
c                    when no ARMA operators are in the model
      IF(endopr.gt.0.and.begopr.le.endopr)THEN
       endlag=Opr(endopr)-1
c     ------------------------------------------------------------------
       IF(Lprt)CALL rtshdr(Mt1,F)
       IF(Lsvlg)CALL rtshdr(Ng,Lsvlg)
       IF(Lsav)THEN
        CALL opnfil(T,F,LESTRT,fh,fcnok)
        IF(.not.fcnok)THEN
         CALL abend
         RETURN
        END IF
        WRITE(fh,1011)TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,
     &                TABCHR,TABCHR,TABCHR,TABCHR,TABCHR,TABCHR
 1011   FORMAT('Operator',a,'Factor',a,'Root',a,'Real',a,'Imaginary',a,
     &         'Modulus',a,'Frequency',/,'--------',a,'------',a,
     &         '----',a,'----',a,'---------',a,'-------',a,'---------')
       END IF
c     ------------------------------------------------------------------
       DO iflt=AR,MA
        begopr=Mdl(iflt-1)
        endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
        DO iopr=begopr,endopr
         beglag=Opr(iopr-1)
         endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
         CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
         IF(Lfatal)RETURN
         IF(Lprt)CALL rtsrowhdr(Mt1,tmpttl,ntmpcr)
         IF(Lsvlg)CALL rtsrowhdr(Ng,tmpttl,ntmpcr)
         IF(Lsav.or.Ldiag)THEN
          DO spchr=ntmpcr,1,-1
           IF(tmpttl(spchr:spchr).eq.' ')GO TO 10
          END DO
          spchr=1
         END IF
c     ------------------------------------------------------------------
   10    factor=Oprfac(iopr)
         degree=Arimal(endlag)/factor
         coeff(1)=-1.0D0
         CALL setdp(0D0,degree,coeff(2))
c         DO i=2,degree+1
c          coeff(i)=0.d0
c         END DO
c     ------------------------------------------------------------------
         DO ilag=beglag,endlag
          coeff(Arimal(ilag)/factor+1)=Arimap(ilag)
         END DO
         CALL roots(coeff,degree,allinv,zeror,zeroi,zerom,zerof)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         DO i=1,degree
          IF(Lprt)CALL rtsrow(Mt1,i,Zeror,Zeroi,Zerom,Zerof)
          IF(Lsvlg)CALL rtsrow(Ng,i,Zeror,Zeroi,Zerom,Zerof)
* 1030     FORMAT('   Root',i3,t18,4F11.4)
          IF(Lsav.or.Ldiag)THEN
           ipos=1
           CALL dtoc(zeror(i),outstr,ipos)
           IF(Lfatal)RETURN
           outstr(ipos:ipos)=TABCHR
           ipos=ipos+1
           CALL dtoc(zeroi(i),outstr,ipos)
           IF(Lfatal)RETURN
           outstr(ipos:ipos)=TABCHR
           ipos=ipos+1
           CALL dtoc(zerom(i),outstr,ipos)
           IF(Lfatal)RETURN
           outstr(ipos:ipos)=TABCHR
           ipos=ipos+1
           CALL dtoc(zerof(i),outstr,ipos)
           IF(Lfatal)RETURN
           IF(Lsav)THEN
            WRITE(fh,1031)tmpttl(spchr+1:ntmpcr),TABCHR,
     &                    tmpttl(1:spchr-1),TABCHR,i,TABCHR,
     &                    outstr(1:(ipos-1))
           END IF
           IF(Ldiag)THEN
            lower1=CHAR(ICHAR(tmpttl(1:1))+32)
            DO k=spchr+1,ntmpcr
             i2=k-spchr
             lower2(i2:i2)=CHAR(ICHAR(tmpttl(k:k))+32)
            END DO
            WRITE(Nform,1031)'roots.'//lower2(1:i2),'.',
     &                       lower1//tmpttl(2:spchr-1),'.',i,': ',
     &                       outstr(1:(ipos-1))
           END IF
 1031      FORMAT(a,a,a,a,i2.2,a,a)
          END IF
         END DO
        END DO
       END DO
       IF(Lprt)THEN
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
       IF(Lsvlg)THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       IF(Lsav)CALL fclose(fh)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE rtshdr(Mt,Lsv)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER thisId*6
      INTEGER Mt
      LOGICAL Lsv
c-----------------------------------------------------------------------
c     Prints out the header for the roots output
c-----------------------------------------------------------------------
      IF(Lsv)THEN
       Inlgfl=Inlgfl+1
       WRITE(Mt,1001)Inlgfl
      ELSE
       Inmd=Inmd+1
       WRITE(Mt,1000)Inmd
      END IF
      CALL mkTableTag(Mt,'w70','Roots of '//Mdlttl(1:Nmdlcr))
      CALL mkCaption(Mt,'Roots of '//Mdlttl(1:Nmdlcr))
      CALL writTag(Mt,'<tr>')
      CALL mkTableCell(Mt,'head','&nbsp;')
      Inrl=Inrl+1
      WRITE(thisId,1010)'rl',Inrl
      CALL mkHeaderCellId(Mt,0,0,thisId,'@','@','Real')
      Inim=Inim+1
      WRITE(thisId,1010)'im',Inim
      CALL mkHeaderCellId(Mt,0,0,thisId,'@','@','Imaginary')
      Inmd=Inmd+1
      WRITE(thisId,1010)'md',Inmd
      CALL mkHeaderCellId(Mt,0,0,thisId,'@','@','Modulus')
      Infq=Infq+1
      WRITE(thisId,1010)'fq',Infq
      CALL mkHeaderCellId(Mt,0,0,thisId,'@','@','Frequency')
      CALL writTag(Mt,'</tr>')
c     ------------------------------------------------------------------
 1000 FORMAT('<div id="mdl',i3.3,'">')
 1001 FORMAT('<div id="lgmdl',i6.6,'">')
 1010 FORMAT(a2,i4.4)
c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE rtsrowhdr(Mt,tmpttl,ntmpcr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER tmpttl*(POPRCR),thisId*6
      INTEGER Mt,ntmpcr
c-----------------------------------------------------------------------
      CALL writTag(Mt,'<tr>')
      Ingr=Ingr+1
      WRITE(thisId,1010)'gr',Ingr
      CALL mkHeaderCellId(Mt,0,5,thisId,'left','@',tmpttl(1:ntmpcr))
      CALL writTag(Mt,'</tr>')
c     ------------------------------------------------------------------
 1010 FORMAT(a2,i4.4)
c     ------------------------------------------------------------------
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE rtsrow(Mt,Ind,Zeror,Zeroi,Zerom,Zerof)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER thisId*6,thisHdr*20,thisVal*16,thisLag*6,thisRoot*7
      INTEGER Mt,Ind
      DOUBLE PRECISION zeror(PORDER),zeroi(PORDER),zerom(PORDER),
     &                 zerof(PORDER)
c-----------------------------------------------------------------------
      CALL writTag(Mt,'<tr>')
      Invl=Invl+1
      WRITE(thisId,1010)'vl',Invl
      WRITE(thisRoot,1310)Ind
      CALL mkHeaderCellId(Mt,0,0,thisId,'@','@',thisRoot)
      WRITE(thisHdr,1020)Ingr,Invl,'rl',Inrl
      WRITE(thisVal,1320)zeror(Ind)
      CALL mkTableCellHeader(Mt,thisHdr,'center',thisVal)
      WRITE(thisHdr,1020)Ingr,Invl,'im',Inim
      WRITE(thisVal,1320)zeroi(Ind)
      CALL mkTableCellHeader(Mt,thisHdr,'center',thisVal)
      WRITE(thisHdr,1020)Ingr,Invl,'md',Inmd
      WRITE(thisVal,1320)zerom(Ind)
      CALL mkTableCellHeader(Mt,thisHdr,'center',thisVal)
      WRITE(thisHdr,1020)Ingr,Invl,'fq',Infq
      WRITE(thisVal,1320)zerof(Ind)
      CALL mkTableCellHeader(Mt,thisHdr,'center',thisVal)
      CALL writTag(Mt,'</tr>')
c-----------------------------------------------------------------------
 1010 FORMAT(a2,i4.4)
 1020 FORMAT('gr',i4.4,' vl',i4.4,' ',a2,i4.4)
 1310 FORMAT('Root ',i2)
 1320 FORMAT(G16.6)
c     ------------------------------------------------------------------
      RETURN
      END
