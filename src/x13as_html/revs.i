C 
C    Created by REG on 03 Jan 2006
C
C... Variables in Common Block /firevs/ 
C    MSEs and revision statistics given finite amount of data
C    in the past and semi-infinite amount of data in the future 
C    where index identifies irregular, seasonal, and trend components.
      DOUBLE PRECISION infMSEs(3), infRevs(3)
C    MSEs and revision statistics given finite amount of data
C    in the past and finite amount of data in the future and  
C    relative revision statistics to infRevs
C    where first index identifies irregular, seasonal, trend components
C    and where second index identifies one, two, three, four, five years
C    of data in the future.
      DOUBLE PRECISION finMSEs(3,5), finRevs(3,5), relRevs(3,5)
C    MSEs given finite anount of data in the past 
C    and no data in the future
      DOUBLE PRECISION curMSEs(3)
C    Standard Error of Revisions for last 5 years of observations
C    for seasonal component and then trend component.
      DOUBLE PRECISION seRevs(60,2)
      common /firevs/ curMSEs, finMSEs, finRevs, infMSEs, infRevs,
     &                relRevs, seRevs