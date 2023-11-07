C
C    Growth Rate table 5.x common block
C
c-----------------------------------------------------------------------
c Name   Type Description (Growth Rate common block Variables)
c-----------------------------------------------------------------------
c nSeaGRSE1 i size (rows,columns) of vSeaGRSE1 vector
c nSeaGRSE2 i size (rows,columns) of vSeaGRSE2 vector
c nTreGRSE1 i size (rows,columns) of vTreGRSE1 vector
c nTreGRSE2 i size (rows,columns) of vTreGRSE2 vector
c vSeaGRSE1 d vector of seasonal component growth rate SEs for table 5.2
c vSeaGRSE2 d vector of seasonal component growth rate SEs for table 5.5
c vTbl51    d vector of table 5.1 MSEs
c vTbl53    d vector of table 5.3 SEs
c vTbl54    d vector of table 5.4 MSEs
c vTbl56    d vector of table 5.6 SEs
c vTbl57    d vector of table 5.7 SEs
c vTreGRSE1 d vector of trend component growth rate SEs for table 5.2
c vTreGRSE2 d vector of trend component growth rate SEs for table 5.5
c-----------------------------------------------------------------------
      INTEGER nSeaGRSE1(2), nSeaGRSE2(2), nTreGRSE1(2), nTreGRSE2(2)
      DOUBLE PRECISION vSeaGRSE1(POBS), vTreGRSE1(POBS)
      DOUBLE PRECISION vSeaGRSE2(POBS), vTreGRSE2(POBS)
      DOUBLE PRECISION vTbl51(6), vTbl53(2), vTbl54(6), vTbl56(6,2),
     &                 vTbl57(3,3)
      common / tbl5x / vTbl51, vSeaGRSE1, nSeaGRSE1,
     &                 vTreGRSE1, nTreGRSE1, vTbl53, vTbl54,
     &                 vSeaGRSE2, nSeaGRSE2,
     &				   vTreGRSE2, nTreGRSE2, vTbl56, vTbl57