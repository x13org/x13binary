      subroutine htmlfortbl(fo,freg,ftr,fsa,fs,fcyc,fir,tse,siepf,
     $              siepfl,sieaf,sieafl,neff,mq,nouir,noutr,npatd,
     $         neast,nchi,npsi,ncyc,ncycth,lamd,nper,nyer,nz,lfor,
     $         isCloseToTD,varwnc)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
C
C.. Formal Arguments ..
      integer neff(0:7),mq,nouir,noutr,npatd,neast,nchi,npsi,ncyc,
     $        ncycth,lamd,nper,nyer,nz, lfor
      real*8 fo(-kp:kp),freg(-kp:kp),ftr(-kp:kp),fsa(-kp:kp),fs(-kp:kp),
     $       fcyc(-kp:kp),fir(-kp:kp),tse(kl),siepf(kl),siepfl(kl),
     $       sieaf(kl),sieafl(kl),varwnc
      logical isCloseToTD
C
C.. Local Scalars ..
      integer i,j,jnlastper,jnlastyear,ncols,nf,nlastper,nlastyear,nse
C
C.. Local Arrays ..
      character mth(12)*34,srt(11)*34
      real*8 formatrix(kp,14),tmp(kp)
C
C.. External Calls ..
      external USRENTRY
C
C.. Intrinsic Functions ..
      intrinsic MAX, MOD
      include 'stream.i'
C
C.. Data Declarations ..
     
      data mth/
     $     '<abbr title="January">JAN</abbr>  ',
     $     '<abbr title="February">FEB</abbr> ',
     $     '<abbr title="March">MAR</abbr>    ',
     $     '<abbr title="April">APR</abbr>    ',
     $     'MAY                               ',
     $     '<abbr title="June">JUN</abbr>     ',
     $     '<abbr title="July">JUL</abbr>     ',
     $     '<abbr title="August">AUG</abbr>   ',
     $     '<abbr title="September">SEP</abbr>',
     $     '<abbr title="October">OCT</abbr>  ',
     $     '<abbr title="November">NOV</abbr> ',
     $     '<abbr title="December">DEC</abbr> '/
      data srt/
     $     '<abbr title="first">1ST</abbr>    ',
     $     '<abbr title="second">2ND</abbr>   ',
     $     '<abbr title="third">3RD</abbr>    ',
     $     '<abbr title="fourth">4TH</abbr>   ',
     $     '<abbr title="fifth">5TH</abbr>    ',
     $     '<abbr title="sixth">6TH</abbr>    ',
     $     '<abbr title="seventh">7TH</abbr>  ',
     $     '<abbr title="eighth">8TH</abbr>   ',
     $     '<abbr title="ninth">9TH</abbr>    ',
     $     '<abbr title="tenth">10TH</abbr>   ',
     $     '<abbr title="eleventh">11TH</abbr>'/
C
C ... Executable Statements ...
C
      CALL mkTableTag(Nio,'w90','FORECAST OF FINAL COMPONENT')
      CALL writTag(Nio,'<thead>')
      CALL writTag(Nio,'<tr>')
      CALL mkTableCellSpan(Nio,'row',2,'head','&nbsp;')
      ncols = 1
      nse = 1
      nf = MAX(lfor,MAX(8,2*mq))
      do i = 1,nf
       formatrix(i,ncols) = fo(i)
       formatrix(i,ncols+1) = tse(i)
      end do
      ncols = ncols + 1
      CALL mkHeaderCellScope(Nio,0,2,'colgroup','@',
     &                       'ORIGINAL (UNCORRECTED)')
      if ((nchi.gt.1) .or. (noutr.eq.1) .or. (neff(1).eq.1) .or. 
     $     (neff(7).eq.1)) then
       nse = nse + 1
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = ftr(i)
        tmp(i) = ftr(i)
       end do
       ncols = ncols + 1
       if (lamd .eq. 0) then
        do i = 1,nf
         formatrix(i,ncols) = siepfl(i)
        end do
       else
        do i = 1,nf
         formatrix(i,ncols) = siepf(i)
        end do
       end if
       call USRENTRY(tmp,1,nf,1,kp,1410)
       CALL mkHeaderCellScope(Nio,0,2,'colgroup','@','TREND-CYCLE')
      end if
      if ((npsi.gt.1) .or. (neast.eq.1) .or. (neff(2).eq.1) .or.
     $    (npatd.eq.1)) then
       nse = nse + 1
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = fsa(i)
        tmp(i) = fsa(i)
       end do
       ncols = ncols + 1
       if (lamd .eq. 0) then
        do i = 1,nf
         formatrix(i,ncols) = sieafl(i)
        end do
       else
        do i = 1,nf
         formatrix(i,ncols) = sieaf(i)
        end do
       end if
       call USRENTRY(tmp,1,nf,1,kp,1409)
       CALL mkHeaderCellScope(Nio,0,2,'colgroup',
     &                        'SEASONALLY ADJUSTED SERIES','SA SERIES')
      elseif (neff(0) .eq. 1) then
       nse = nse + 1
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = fsa(i)
        tmp(i) = fsa(i)
       end do
       ncols = ncols + 1
       if (lamd .eq. 0) then
        do i = 1,nf
         formatrix(i,ncols) = sieafl(i)
        end do
       else
        do i = 1,nf
         formatrix(i,ncols) = sieaf(i)
        end do
       end if
       call USRENTRY(tmp,1,nf,1,kp,1409)
       CALL mkHeaderCellScope(Nio,0,2,'colgroup',
     &                        'SEASONALLY ADJUSTED SERIES','SA SERIES')
      end if
      if (neff(0) .eq. 1) then
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = freg(i)
       end do
       CALL mkHeaderCellScope(Nio,2,0,'col','@',
     &                        'SEPARATE REGRESSION EFFECT')
      end if
      if ((npsi.gt.1) .or. (neast.eq.1) .or. (neff(2).eq.1) .or.
     $    (npatd.eq.1)) then
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = fs(i)
        tmp(i) = fs(i)
       end do
       call USRENTRY(tmp,1,nf,1,kp,1411)
       if (lamd .eq. 0) then
        CALL mkHeaderCellScope(Nio,2,0,'col','@','SEASONAL FACTORS')
       else
        CALL mkHeaderCellScope(Nio,2,0,'col','@','SEASONAL COMPONENT')
       end if
      end if
      if ((neff(3).eq.1) .or. (nouir.eq.1) .or. 
     $    (varwnc.gt.1.0D-10 .and.(ncycth.eq.1 .or.ncyc.gt.1))
     $     .or. (neff(5).eq.1)) then
       ncols = ncols + 1
       if (lamd .eq. 1) then
        do i = 1,nf
         formatrix(i,ncols) = fir(i) + fcyc(i)
         tmp(i) = fir(i)
        end do
       else
        do i = 1,nf
         formatrix(i,ncols) = (fir(i)*fcyc(i)) / 100.0d0
         tmp(i) = fir(i)
        end do
       end if
       call USRENTRY(tmp,1,nf,1,kp,1412)
       if (IsCloseToTD) then
        if (lamd .eq. 0) then
         CALL mkHeaderCellScope(Nio,2,0,'col',
     &                          'Trading Day final - IRREGULAR FACTORS',
     &                          'TDfinal.-IRREG.')
        else
         CALL mkHeaderCellScope(Nio,2,0,'col',
     &                          'Trading Day final - IRREGULAR',
     &                          'TDfinal.-IRREG.')
        end if
       else
        if (lamd .eq. 0) then
         CALL mkHeaderCellScope(Nio,2,0,'col',
     &                         'TRANSITORY IRREGULAR FACTORS',
     &                         'TRANS.-IRREG. FACTORS')
        else
         CALL mkHeaderCellScope(Nio,2,0,'col','TRANSITORY IRREGULAR',
     &                         'TRANS.-IRREG.')
        end if
       end if
      end if
      CALL writTag(Nio,'</tr>')
      CALL writTag(Nio,'<tr>')
      CALL mkHeaderCellScope(Nio,0,0,'col','@','FORECAST')
      CALL mkHeaderCellScope(Nio,0,0,'col','standard error','SE')
      do i=1,nse-1
       CALL mkHeaderCellScope(Nio,0,0,'col','@','FORECAST')
       CALL mkHeaderCellScope(Nio,0,0,'col',
     &                        'standard error of the revision','SER')
      end do
      CALL writTag(Nio,'</tr>')
      CALL writTag(Nio,'</thead>')
      nlastper = nper
      nlastyear = nyer
      do i = 2,nz
       if (MOD(nlastper,mq) .eq. 0) then
        nlastyear = nlastyear + 1
        nlastper = 0
       end if
       nlastper = nlastper + 1
      end do
      nlastper = nlastper + 1
      if (nlastper .gt. mq) then
       nlastper = 1
       nlastyear = nlastyear + 1
      end if
      jnlastper = nlastper
      jnlastyear = nlastyear
      CALL writTag(Nio,'<tbody>')
      do i = 1,nf
       if (mq .eq. 12) then
        write (Nio,1010) mth(nlastper), nlastyear
       else
        write (Nio,1010) srt(nlastper), nlastyear
       end if
 1010  format('<tr><th scope="row">',a,1x,i4,'</th>')
       do j = 1, nse*2, 2
        write (Nio,1020) formatrix(i,j), formatrix(i,j+1)
 1020   format(2('<td>',f16.4,'</td>'))
       end do 
       if (nlastper .eq. mq) then
        nlastper = 1
        nlastyear = nlastyear + 1
       else
        nlastper = nlastper + 1
       end if
       if (nse*2 .lt. ncols) then
        do j=nse*2+1,ncols
         write (Nio,1030) formatrix(i,j)
 1030   format('<td>',f16.4,'</td>')
        end do
       end if
       CALL writTag(Nio,'</tr>')
      end do
      CALL writTag(Nio,'</tbody>')
      write (Nio,'("</table>")')
      CALL mkPOneLine(Nio,'@','&nbsp;')
      CALL mkPOneLine(Nio,'em','SE  : standard error of the '//
     $                         'observation series forecast.')
      CALL mkPOneLine(Nio,'em','SER : standard error of the revision.')
      CALL mkPOneLine(Nio,'ub','Note 1 :')
      CALL mkPOneLine(Nio,'em',
     &       'Since the component is never observed, '//
     &       'the forecast error is of little applied interest. '//
     &       'What is of interest is the se of the revision the '//
     $       'forecast of the component will undergo (until it '//
     $       'becomes the final or historical estimator).')
      CALL mkPOneLine(Nio,'ub','Note 2 :')
      CALL mkPOneLine(Nio,'em','SER(Seasonal) = SER (SA Series)')
      end
cc
c
cc
