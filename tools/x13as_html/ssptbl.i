c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c slidingspans   SSP or SS
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c sliding spans header                   SHD
c sliding spans ftests                   FTS
c range analysis                         RNG
c percent flagged                        PCT
c summary tables                         SUM
c seasonal fac spans                     SFS
c changes spans                          CHS
c sa spans                               SAS
c yearly changes spans                   YCS
c td spans                               TDS
c-----------------------------------------------------------------------
      INTEGER LSSSHD,LSSFTS,LSSFMN,LSSPCT,LSSYPC,LSSSUM,LSSYSM,LSSSFS,
     &        LSSCHS,LSSSAS,LSSYCS,LSSTDS
      PARAMETER(
     &          LSSSHD=268,LSSFTS=269,LSSFMN=270,LSSPCT=272,LSSYPC=274,
     &          LSSSUM=276,LSSYSM=278,LSSSFS=280,LSSCHS=282,LSSSAS=284,
     &          LSSYCS=286,LSSTDS=288)
