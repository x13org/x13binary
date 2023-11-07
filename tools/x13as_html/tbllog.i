c-----------------------------------------------------------------------
c 	Variables  with LSP<spec> are the displacements for the tables
c found in each spec, NSP<spec> are the number of tables used in each
c spec. The spec's that have print and save tables are
c
c series         SRS
c transform      TRN
c regression     REG
c identify       IDN
c automdl        AUM
c pickmdl        AXM
c estimate       EST
c outlier        OTL
c check          CHK
c forecast       FOR
c spectrum       SPC
c x11            X11
c force          FRC
c x11regression  XRG
c history        REV
c slidingspans   SSP
c composite      CMP
c seats          SET
c-----------------------------------------------------------------------
      INTEGER LSPSRS,NSPSRS
      INTEGER LSPTRN,NSPTRN
      INTEGER LSPREG,NSPREG
      INTEGER LSPIDN,NSPIDN
      INTEGER LSPAUM,NSPAUM
      INTEGER LSPAXM,NSPAXM
      INTEGER LSPEST,NSPEST
      INTEGER LSPOTL,NSPOTL
      INTEGER LSPCHK,NSPCHK
      INTEGER LSPFOR,NSPFOR
      INTEGER LSPSPC,NSPSPC
      INTEGER LSPX11,NSPX11
      INTEGER LSPFRC,NSPFRC
      INTEGER LSPXRG,NSPXRG
      INTEGER LSPREV,NSPREV
      INTEGER LSPSSP,NSPSSP
      INTEGER LSPCMP,NSPCMP
      INTEGER LSPSET,NSPSET
c-----------------------------------------------------------------------
      PARAMETER (LSPSRS=0,NSPSRS=10,
     &           LSPTRN=10,NSPTRN=11,
     &           LSPREG=21,NSPREG=14,
     &           LSPIDN=35,NSPIDN=5,
     &           LSPAUM=40,NSPAUM=13,
     &           LSPAXM=53,NSPAXM=4,
     &           LSPEST=57,NSPEST=14,
     &           LSPOTL=71,NSPOTL=5,
     &           LSPCHK=76,NSPCHK=11,
     &           LSPFOR=87,NSPFOR=5,
     &           LSPSPC=92,NSPSPC=26,
     &           LSPX11=118,NSPX11=90,
     &           LSPFRC=208,NSPFRC=9,
     &           LSPXRG=217,NSPXRG=22,
     &           LSPREV=239,NSPREV=28,
     &           LSPSSP=267,NSPSSP=21,
     &           LSPCMP=288,NSPCMP=60,
     &           LSPSET=348,NSPSET=48)
