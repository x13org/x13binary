c-----------------------------------------------------------------------
c	These tables must be consistant with the level variable
c in getprt, in dfttbl in gtinpt, and tbldic in opnfil.
c 	Variables  with LSL<spec> are the displacements for the tables
c found in the specs below, NSL<spec> are the number of tables used in
c each spec. The spec's that have savelog tables are
c changed NSLCMP from 22->20 -- Jan.2021
c
c regression     REG
c automdl        AUM
c estimate       EST
c x11            X11 
c history        REV 
c slidingspans   SSP 
c composite      CMP 
c-----------------------------------------------------------------------
      INTEGER LSLADJ,NSLADJ
      INTEGER LSLAUM,NSLAUM
      INTEGER LSLAXM,NSLAXM
      INTEGER LSLEST,NSLEST
      INTEGER LSLREG,NSLREG
      INTEGER LSLOTL,NSLOTL
      INTEGER LSLCHK,NSLCHK
      INTEGER LSLX11,NSLX11
      INTEGER LSLXRG,NSLXRG
      INTEGER LSLREV,NSLREV
      INTEGER LSLSSP,NSLSSP
      INTEGER LSLSPC,NSLSPC
      INTEGER LSLCMP,NSLCMP
      INTEGER LSLSET,NSLSET
c-----------------------------------------------------------------------
      PARAMETER (LSLADJ=  0,NSLADJ=  1,
     &           LSLAUM=  1,NSLAUM=  6,
     &           LSLAXM=  7,NSLAXM=  1,
     &           LSLEST=  8,NSLEST=  8,
     &           LSLREG= 16,NSLREG=  2,
     &           LSLOTL= 18,NSLOTL=  1,
     &           LSLCHK= 19,NSLCHK=  9,
     &           LSLX11= 28,NSLX11= 20,
     &           LSLXRG= 48,NSLXRG=  1,
     &           LSLREV= 49,NSLREV=  9,
     &           LSLSSP= 58,NSLSSP=  2,
     &           LSLSPC= 60,NSLSPC= 14,
     &           LSLCMP= 74,NSLCMP= 20,
     &           LSLSET= 94,NSLSET= 15)
