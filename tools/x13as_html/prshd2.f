C     Last change:Jan. 2021, change blnk to lenghth 79
C     Last change:  BCM  11 Jun 1998    4:19 pm
**==prshd2.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE prshd2(Ttlhdr,Ntbttl,Subhdr,Nsbhdr,Begdat,Sp,Nobs)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Prints the header information for a time series.
c-----------------------------------------------------------------------
      INCLUDE 'tbltitle.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      INTEGER PSRSCR
      PARAMETER(PSRSCR=79)
c     ------------------------------------------------------------------
      CHARACTER Ttlhdr*(PTTLEN),Subhdr*(80),bdtstr*(10),blnk*(PSRSCR),
     &          edtstr*(10)
      INTEGER Begdat,idate,nchr1,nchr2,Nobs,Sp,Ntbttl,Nsbhdr
      DIMENSION Begdat(2),idate(2)
c     ------------------------------------------------------------------
      DATA blnk/
     &'                                                                 
     &              '/
c     ------------------------------------------------------------------
*      IF(Lpage.and.Locpag)THEN
*       WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*       Kpage=Kpage+1
*      END IF
      CALL addate(Begdat,Sp,Nobs-1,idate)
      CALL wrtdat(Begdat,Sp,bdtstr,nchr1)
      IF(.not.Lfatal)CALL wrtdat(idate,Sp,edtstr,nchr2)
      IF(Lfatal)RETURN
      IF(Ntbttl.gt.0)THEN
       IF(Nsbhdr.gt.0)THEN
        CALL writTagOneLine(Mt1,'h3','@',
     &                      Ttlhdr(1:Ntbttl)//Cbr//Subhdr(1:Nsbhdr))
       ELSE
        CALL writTagOneLine(Mt1,'h3','@',Ttlhdr(1:Ntbttl))
       END IF
      END IF
 1020 FORMAT(' ',a)
      IF(Nobs.gt.0)THEN
       WRITE(Mt1,1030)blnk(1:17-nchr1-nchr2),bdtstr(1:nchr1),
     &                edtstr(1:nchr2),Cbr,Nobs
 1030  FORMAT(' <p>From ',a,a,' to ',a,a,/,
     &        '  Observations     ',i6,'</p>')
      END IF
c     ------------------------------------------------------------------
      RETURN
c     ------------------------------------------------------------------
      END
