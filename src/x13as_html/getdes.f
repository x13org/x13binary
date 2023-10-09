C     Last change:  Jan.2021, use appropriate dictionary,
c     DSEDIC -> DS2DIC.
C     Last change:  BCM  23 Aug 2006    6:51 am
      SUBROUTINE getdes(Itbl,Fildes,Ndescr,Label)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Subroutine to extract the description for table Itbl from the data
c     dictionaries into the character scalar Fildes
c     Since X-13ARIMA-SEATS has so many tables, the dictionary for the
c     table descriptions had to be divided into many parts so the length
c     of all of the data dictionaries was less than 2000 characters
c     ------------------------------------------------------------------
c     Written by BCM August 22 2006
c     ------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'tbltitle.prm'
      INCLUDE 'dessrs.prm'
      INCLUDE 'desmdl.prm'
      INCLUDE 'desx11.prm'
      INCLUDE 'desfsa.prm'
      INCLUDE 'desdgn.prm'
      INCLUDE 'desdg2.prm'
      INCLUDE 'descmp.prm'
      INCLUDE 'descm2.prm'
      INCLUDE 'desadj.prm'
      INCLUDE 'desxrg.prm'
      INCLUDE 'desset.prm'
      INCLUDE 'desst2.prm'
      INCLUDE 'desspc.prm'
c     ------------------------------------------------------------------
      CHARACTER Fildes*(PTTLEN)
      INTEGER Itbl,Ndescr
      LOGICAL Label
c     ------------------------------------------------------------------
      INCLUDE 'dessrs.var'
      INCLUDE 'desmdl.var'
      INCLUDE 'desx11.var'
      INCLUDE 'desfsa.var'
      INCLUDE 'desdgn.var'
      INCLUDE 'desdg2.var'
      INCLUDE 'descmp.var'
      INCLUDE 'descm2.var'
      INCLUDE 'desadj.var'
      INCLUDE 'desxrg.var'
      INCLUDE 'desset.var'
      INCLUDE 'desst2.var'
      INCLUDE 'desspc.var'
c     ------------------------------------------------------------------
      IF(Itbl.le.PDSR)THEN
       CALL makttl(DSRDIC,dsrptr,PDSR,Itbl,0,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM1)THEN
       CALL makttl(DSMDIC,dsmptr,PDSM,Itbl,PDSR,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM2)THEN
       CALL makttl(DSPDIC,dspptr,PDSP,Itbl,PDSUM1,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM3)THEN
       CALL makttl(DSXDIC,dsxptr,PDSX,Itbl,PDSUM2,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM4)THEN                
       CALL makttl(DSSDIC,dssptr,PDSS,Itbl,PDSUM3,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM5)THEN                
       CALL makttl(DSADIC,dsaptr,PDSA,Itbl,PDSUM4,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM6)THEN                
       CALL makttl(DSIDIC,dsiptr,PDSI,Itbl,PDSUM5,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM7)THEN                
       CALL makttl(DSDDIC,dsdptr,PDSD,Itbl,PDSUM6,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM8)THEN
       CALL makttl(DD2DIC,dd2ptr,PDD2,Itbl,PDSUM7,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM9)THEN
       CALL makttl(DSCDIC,dscptr,PDSC,Itbl,PDSUM8,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM10)THEN
       CALL makttl(DC2DIC,dc2ptr,PDC2,Itbl,PDSUM9,Fildes,Ndescr,Label,F)
      ELSE IF(Itbl.le.PDSUM11)THEN
       CALL makttl(DSEDIC,dseptr,PDSE,Itbl,PDSUM10,Fildes,Ndescr,Label,
     &             F)
      ELSE
       CALL makttl(DS2DIC,ds2ptr,PDS2,Itbl,PDSUM11,Fildes,Ndescr,Label,
     &             F)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
      
