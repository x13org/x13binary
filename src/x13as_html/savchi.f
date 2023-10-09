      SUBROUTINE savchi(Lsvchi,Lsvlch,Lprhdr,Tbwdth,Baselt,Grpstr,Nchr,
     &                  Info,Df,Chi2vl,Pv,Hdrstr,Nhdr,Savkey)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      CHARACTER Grpstr*(PGRPCR),Savkey*(*),Hdrstr*(31)
      LOGICAL Lprhdr,Lsvchi,Lsvlch
      INTEGER Tbwdth,Baselt,Nchr,Nhdr,Info,Df,i
      DOUBLE PRECISION Chi2vl,Pv
c-----------------------------------------------------------------------
      IF(Lsvchi.and.baselt.ne.NOTSET)
     &   WRITE(Nform,1010)Savkey,Grpstr(1:Nchr),Df,Chi2vl,Pv
      IF(Lsvlch)
     &   CALL prtchi(Ng,Lprhdr,Tbwdth,Baselt,Grpstr,Nchr,Info,Df,Chi2vl,
     &               Pv,Hdrstr,Nhdr,Lsvlch)
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(a,a,': ',i4,2(1x,e22.15))
c-----------------------------------------------------------------------
	END
