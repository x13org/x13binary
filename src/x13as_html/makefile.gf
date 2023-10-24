# MKMF template makefile for protected mode executables.
LINKER    = $(FC)
PROGRAM         = x13as_html
DEST      = .
EXTHDRS         =
HDRS            =
LDFLAGS   = -s
LDMAP     =
LIBS      =
MAKEFILE  = Makefile
OBJS            = aaamain.o abend.o acf.o acfar.o acfdgn.o \
                  acfhdr.o addadj.o addate.o addeas.o \
                  addfix.o addlom.o addmat.o addmul.o addotl.o \
                  addsef.o addsub.o addtd.o addusr.o adestr.o \
                  adjreg.o adjsrs.o adlabr.o adotss.o adpdrg.o \
                  adrgef.o adrgim.o adsncs.o adthnk.o aggmea.o \
                  agr.o agr1.o agr2.o agr3.o agr3s.o \
                  agrxpt.o altundovrtst.o amdest.o amdfct.o amdid.o \
                  amdid2.o amdprt.o amidot.o analts.o ansub1.o \
                  ansub10.o ansub11.o ansub2.o ansub3.o ansub4.o \
                  ansub5.o ansub7.o ansub8.o ansub9.o antilg.o \
                  apply.o ar30rg.o arfit.o arflt.o arima.o \
                  armacr.o armafl.o armats.o arspc.o autoer.o automd.o \
                  automx.o avedur.o aver.o averag.o bakusr.o \
                  bench.o bestmd.o bkdfmd.o bldcov.o blddif.o \
                  bstget.o bstmdl.o btrit.o calcqs.o calcqs2.o \
                  calcsc.o ceilng.o change.o chisq.o chitst.o \
                  chkadj.o chkchi.o chkcvr.o chkeas.o chkmu.o \
                  chkorv.o chkrt1.o chkrt2.o chkrts.o chksmd.o \
                  chktrn.o chkuhg.o chkurt.o chkzro.o chrt.o \
                  chsppf.o chusrg.o clrotl.o clsgrp.o cmpchi.o \
                  cmpstr.o cncrnt.o cnvfil.o cnvfmt.o cnvmdl.o \
                  coladd.o combft.o compb.o compcrodiag.o compdiag.o \
                  complagdiag.o compmse.o compmsealt.o component.o comprevs.o \
                  constant.o copy.o copycl.o copylg.o cormtx.o \
                  cornom.o corplt.o covar.o cpyint.o cpymat.o \
                  crosco.o ctod.o ctodat.o ctoi.o cumnor.o \
                  cvcmma.o cvdttm.o cvrerr.o daxpy.o dcopy.o \
                  ddot.o decibl.o delstr.o deltst.o desreg.o \
                  devlpl.o dfdate.o dgefa.o dgesl.o difflt.o \
                  dinvnr.o divgud.o divsub.o dlrgef.o dlrgrw.o \
                  dlusrg.o dot.o dpeq.o dpmpar.o dppdi.o \
                  dppfa.o dppsl.o dsarma.o dscal.o dsolve.o \
                  dtoc.o easaic.o easter.o editor.o eltfcn.o \
                  eltlen.o emcomp.o ends.o endsf.o enorm.o \
                  entsch.o errhdr.o estrmu.o euclid.o exctma.o \
                  extend.o extsgnl.o f3cal.o f3gen.o fclose.o \
                  fcnar.o fcstxy.o fdjac2.o fgen.o fis.o \
                  fopen.o forcst.o fouger.o fstop.o ftest.o \
                  fvalue.o fxshfr.o gauss.o gendff.o genfoot.o \
                  genfor.o genindex.o genqs.o genrtt.o genskip.o \
                  genssm.o getadj.o getchk.o getchr.o getcmp.o \
                  getdat.o getdbl.o getdes.o getdiag.o getfcn.o \
                  getfrc.o getgr.o getid.o getidm.o getint.o \
                  getivc.o getmdl.o getmtd.o getopr.o getprt.o \
                  getreg.o getrev.o getrevdec.o getsav.o getsma.o \
                  getsmat.o getsrs.o getssp.o getstr.o getsvec.o \
                  getsvl.o gettpltz.o gettr.o gettrc.o getttl.o \
                  getx11.o getxop.o getxtd.o glbshk.o gnfcrv.o \
                  grzlst.o grzmth.o grzmyr.o gtarg.o gtarma.o \
                  gtauto.o gtautx.o gtdcnm.o gtdcvc.o gtdpvc.o \
                  gtdtvc.o gtedit.o gtestm.o gtfcst.o gtfldt.o \
                  gtfrcm.o gtfree.o gtinpt.o gtinvl.o gtmdfl.o \
                  gtmtdt.o gtmtfl.o gtnmvc.o gtotlr.o gtpdrg.o \
                  gtrgdt.o gtrgpt.o gtrgvl.o gtrvst.o gtseat.o \
                  gtspec.o gttrmo.o gtwacf.o gtx11d.o gtx12s.o \
                  gtxreg.o hender.o hinge.o hist.o histx.o \
                  hndend.o hndtrn.o holday.o holidy.o hrest.o \
                  htmlfortable.o htmlout.o htmlutil.o idamax.o iddiff.o \
                  idmdl.o idotlr.o idpeak.o inbtwn.o indx.o \
                  initdg.o initst.o inpter.o insdbl.o insint.o \
                  inslg.o insopr.o insort.o insptr.o insstr.o \
                  intfmt.o intgpg.o intinp.o intlst.o intrpp.o \
                  intsrt.o invfcn.o invmat.o ipmpar.o iscrfn.o \
                  isdate.o isfals.o isfixd.o ispeak.o ispos.o \
                  issame.o istrue.o itoc.o itrerr.o kfcn.o \
                  kwtest.o lassol.o lendp.o lex.o lgnrmc.o \
                  lkshnk.o lmdif.o lmpar.o loadxr.o locshk.o \
                  logar.o logdet.o lomaic.o lstpth.o makadj.o \
                  makotl.o makttl.o map.o matrix.o maxidx.o \
                  maxlag.o maxvec.o mdlchk.o mdlfix.o mdlinp.o \
                  mdlint.o mdlmch.o mdlset.o mdssln.o meancra.o \
                  medabs.o mflag.o minim2.o mkback.o mkealb.o \
                  mkfreq.o mklnlb.o mkmdsn.o mkmdsx.o mkmetahtmlfile.o \
                  mkoprt.o mkotky.o mkpeak.o mkshdr.o mkspky.o \
                  mksplb.o mkssky.o mkstlb.o mkspst.o mktdlb.o mlist.o \
                  mltpos.o month.o mstest.o mulmat.o mulqmat.o \
                  mulref.o mulsca.o mult.o mult0.o mult1.o \
                  mult2.o mxpeak.o nblank.o newest.o nextk.o \
                  nmlmdl.o nofcst.o nrmtst.o numaff.o numfmt.o \
                  olsreg.o opnfil.o otsort.o outchr.o pacf.o \
                  pass0.o pass2.o pctrit.o phasegain.o polyml.o \
                  polynom.o ppnd.o pracf2.o prafce.o pragr2.o \
                  prfcrv.o pritd.o prlkhd.o procflts.o \
                  prothd.o prprad.o prrvob.o prshd2.o prtacf.o \
                  prtadj.o prtagr.o prtamd.o prtchi.o prtcol.o \
                  prtd8b.o prtd9a.o prtdtb.o prtdwr.o prterr.o \
                  prterx.o prtf2.o prtf2w.o prtfct.o prtft.o \
                  prtitr.o prtlog.o prtmdl.o prtmsp.o prtmsr.o \
                  prtmtx.o prtnfn.o prtopt.o prtref.o prtrev.o \
                  prtrts.o prtrv2.o prtshd.o prttbl.o prttbl1.o \
                  prttd.o prttrn.o prtxrg.o punch.o putbak.o \
                  putrev.o putstr.o qcmmnt.o qcontr.o qdoble.o \
                  qintgr.o qmap.o qmap2.o qname.o qquote.o \
                  qrfac.o qrsolv.o qsdiff.o qtoken.o quad.o \
                  quadit.o quadsd.o ratneg.o ratpos.o rdotlr.o \
                  rdotls.o rdregm.o realit.o regfix.o reglbl.o \
                  regvar.o regx11.o replac.o replyf.o resid.o \
                  resid2.o restor.o revchk.o revdrv.o revhdr.o \
                  revrse.o rgarma.o rgtdhl.o rho2.o rmatot.o \
                  rmfix.o rmlnvr.o rmlpyr.o rmotrv.o rmotss.o \
                  rmpadj.o rmtadj.o rndsa.o rngbuf.o roots.o \
                  round.o rplus.o rpoly.o rv2ss.o rvfixd.o \
                  rvrghd.o sautco.o savacf.o savchi.o savd8b.o \
                  savitr.o savmdc.o savmdl.o savmtx.o savotl.o \
                  savpk.o savspp.o savstp.o savtbl.o savwkf.o \
                  sceast.o scrmlt.o sdev.o sdxtrm.o seatad.o seatdg.o \
                  seatfc.o seatpr.o serates.o setadj.o setamx.o \
                  setapt.o setchr.o setcv.o setcvl.o setdp.o \
                  setint.o setlg.o setmdl.o setmv.o setopr.o \
                  setpt.o setrvp.o setspn.o setssp.o setup.o \
                  setwrt.o setxpt.o sfmax.o sfmsr.o sftest.o \
                  shlsrt.o shrink.o si.o sicp2.o sigex.o \
                  sigsub.o simul.o skparg.o skparm.o skpfcn.o \
                  skplst.o smeadl.o smpeak.o snrasp.o spcdrv.o \
                  spcrsd.o special.o specpeak.o spectrum.o spgrh.o \
                  spgrh2.o spmpar.o ss2rv.o ssap.o ssfnot.o \
                  ssftst.o sshist.o ssmdl.o ssort.o sspdrv.o \
                  ssphdr.o ssprep.o ssrit.o ssrng.o ssx11a.o \
                  ssxmdl.o stpitr.o strinx.o strtvl.o stvaln.o \
                  subset.o sumf.o sumry.o sumsqr.o svaict.o \
                  svamcm.o svchsd.o svdttm.o svf2f3.o svflt.o \
                  svfltd.o svfnrg.o svfreq.o svolit.o svoudg.o \
                  svpeak.o svrgcm.o svrvhd.o svspan.o table.o \
                  taper.o tblhdr.o td6var.o td7var.o tdaic.o \
                  tdftest.o tdlom.o tdset.o tdxtrm.o templs.o \
                  tfmts.o tfmts3.o totals.o transc.o trbias.o \
                  trnaic.o trnfcn.o tstdrv.o tstmd1.o tstmd2.o \
                  ttest.o uconv.o upespm.o usraic.o value.o \
                  varian.o varlog.o vars.o vsfa.o vsfb.o \
                  vsfc.o vtc.o vtest.o weight.o whitsp.o \
                  writln.o wrtdat.o wrtmss.o wrtotl.o \
                  wrttb2.o wrttbl.o wtxtrm.o x11aic.o x11ari.o \
                  x11int.o x11mdl.o x11plt.o x11pt1.o x11pt2.o \
                  x11pt3.o x11pt4.o x11ref.o x12hdr.o x12run.o \
                  xchng.o xpand.o xprmx.o xrgdiv.o xrgdrv.o \
                  xrghol.o xrgtrn.o xrlkhd.o xtrm.o yprmy.o \
                  yrly.o rvarma.o rvtdrg.o prtukp.o svtukp.o \
                  savtpk.o m2q.o chqsea.o npsa.o gennpsa.o prarma.o \
                  testodf.o
SRCS            = aaamain.f abend.f acf.f acfar.f acfdgn.f \
                  acfhdr.f addadj.f addate.f addeas.f \
                  addfix.f addlom.f addmat.f addmul.f addotl.f \
                  addsef.f addsub.f addtd.f addusr.f adestr.f \
                  adjreg.f adjsrs.f adlabr.f adotss.f adpdrg.f \
                  adrgef.f adrgim.f adsncs.f adthnk.f aggmea.f \
                  agr.f agr1.f agr2.f agr3.f agr3s.f \
                  agrxpt.f altundovrtst.f amdest.f amdfct.f amdid.f \
                  amdid2.f amdprt.f amidot.f analts.f ansub1.f \
                  ansub10.f ansub11.f ansub2.f ansub3.f ansub4.f \
                  ansub5.f ansub7.f ansub8.f ansub9.f antilg.f \
                  apply.f ar30rg.f arfit.f arflt.f arima.f \
                  armacr.f armafl.f armats.f arspc.f autoer.f automd.f \
                  automx.f avedur.f aver.f averag.f bakusr.f \
                  bench.f bestmd.f bkdfmd.f bldcov.f blddif.f \
                  bstget.f bstmdl.f btrit.f calcqs.f calcqs2.f \
                  calcsc.f ceilng.f change.f chisq.f chitst.f \
                  chkadj.f chkchi.f chkcvr.f chkeas.f chkmu.f \
                  chkorv.f chkrt1.f chkrt2.f chkrts.f chksmd.f \
                  chktrn.f chkuhg.f chkurt.f chkzro.f chrt.f \
                  chsppf.f chusrg.f clrotl.f clsgrp.f cmpchi.f \
                  cmpstr.f cncrnt.f cnvfil.f cnvfmt.f cnvmdl.f \
                  coladd.f combft.f compb.f compcrodiag.f compdiag.f \
                  complagdiag.f compmse.f compmsealt.f component.f comprevs.f \
                  constant.f copy.f copycl.f copylg.f cormtx.f \
                  cornom.f corplt.f covar.f cpyint.f cpymat.f \
                  crosco.f ctod.f ctodat.f ctoi.f cumnor.f \
                  cvcmma.f cvdttm.f cvrerr.f daxpy.f dcopy.f \
                  ddot.f decibl.f delstr.f deltst.f desreg.f \
                  devlpl.f dfdate.f dgefa.f dgesl.f difflt.f \
                  dinvnr.f divgud.f divsub.f dlrgef.f dlrgrw.f \
                  dlusrg.f dot.f dpeq.f dpmpar.f dppdi.f \
                  dppfa.f dppsl.f dsarma.f dscal.f dsolve.f \
                  dtoc.f easaic.f easter.f editor.f eltfcn.f \
                  eltlen.f emcomp.f ends.f endsf.f enorm.f \
                  entsch.f errhdr.f estrmu.f euclid.f exctma.f \
                  extend.f extsgnl.f f3cal.f f3gen.f fclose.f \
                  fcnar.f fcstxy.f fdjac2.f fgen.f fis.f \
                  fopen.f forcst.f fouger.f fstop.f ftest.f \
                  fvalue.f fxshfr.f gauss.f gendff.f genfoot.f \
                  genfor.f genindex.f genqs.f genrtt.f genskip.f \
                  genssm.f getadj.f getchk.f getchr.f getcmp.f \
                  getdat.f getdbl.f getdes.f getdiag.f getfcn.f \
                  getfrc.f getgr.f getid.f getidm.f getint.f \
                  getivc.f getmdl.f getmtd.f getopr.f getprt.f \
                  getreg.f getrev.f getrevdec.f getsav.f getsma.f \
                  getsmat.f getsrs.f getssp.f getstr.f getsvec.f \
                  getsvl.f gettpltz.f gettr.f gettrc.f getttl.f \
                  getx11.f getxop.f getxtd.f glbshk.f gnfcrv.f \
                  grzlst.f grzmth.f grzmyr.f gtarg.f gtarma.f \
                  gtauto.f gtautx.f gtdcnm.f gtdcvc.f gtdpvc.f \
                  gtdtvc.f gtedit.f gtestm.f gtfcst.f gtfldt.f \
                  gtfrcm.f gtfree.f gtinpt.f gtinvl.f gtmdfl.f \
                  gtmtdt.f gtmtfl.f gtnmvc.f gtotlr.f gtpdrg.f \
                  gtrgdt.f gtrgpt.f gtrgvl.f gtrvst.f gtseat.f \
                  gtspec.f gttrmo.f gtwacf.f gtx11d.f gtx12s.f \
                  gtxreg.f hender.f hinge.f hist.f histx.f \
                  hndend.f hndtrn.f holday.f holidy.f hrest.f \
                  htmlfortable.f htmlout.f htmlutil.f idamax.f iddiff.f \
                  idmdl.f idotlr.f idpeak.f inbtwn.f indx.f \
                  initdg.f initst.f inpter.f insdbl.f insint.f \
                  inslg.f insopr.f insort.f insptr.f insstr.f \
                  intfmt.f intgpg.f intinp.f intlst.f intrpp.f \
                  intsrt.f invfcn.f invmat.f ipmpar.f iscrfn.f \
                  isdate.f isfals.f isfixd.f ispeak.f ispos.f \
                  issame.f istrue.f itoc.f itrerr.f kfcn.f \
                  kwtest.f lassol.f lendp.f lex.f lgnrmc.f \
                  lkshnk.f lmdif.f lmpar.f loadxr.f locshk.f \
                  logar.f logdet.f lomaic.f lstpth.f makadj.f \
                  makotl.f makttl.f map.f matrix.f maxidx.f \
                  maxlag.f maxvec.f mdlchk.f mdlfix.f mdlinp.f \
                  mdlint.f mdlmch.f mdlset.f mdssln.f meancra.f \
                  medabs.f mflag.f minim2.f mkback.f mkealb.f \
                  mkfreq.f mklnlb.f mkmdsn.f mkmdsx.f mkmetahtmlfile.f \
                  mkoprt.f mkotky.f mkpeak.f mkshdr.f mkspky.f \
                  mksplb.f mkstlb.f mkspst.f mkssky.f mktdlb.f mlist.f \
                  mltpos.f month.f mstest.f mulmat.f mulqmat.f \
                  mulref.f mulsca.f mult.f mult0.f mult1.f \
                  mult2.f mxpeak.f nblank.f newest.f nextk.f \
                  nmlmdl.f nofcst.f nrmtst.f numaff.f numfmt.f \
                  olsreg.f opnfil.f otsort.f outchr.f pacf.f \
                  pass0.f pass2.f pctrit.f phasegain.f polyml.f \
                  polynom.f ppnd.f pracf2.f prafce.f pragr2.f \
                  prfcrv.f pritd.f prlkhd.f procflts.f \
                  prothd.f prprad.f prrvob.f prshd2.f prtacf.f \
                  prtadj.f prtagr.f prtamd.f prtchi.f prtcol.f \
                  prtd8b.f prtd9a.f prtdtb.f prtdwr.f prterr.f \
                  prterx.f prtf2.f prtf2w.f prtfct.f prtft.f \
                  prtitr.f prtlog.f prtmdl.f prtmsp.f prtmsr.f \
                  prtmtx.f prtnfn.f prtopt.f prtref.f prtrev.f \
                  prtrts.f prtrv2.f prtshd.f prttbl.f prttbl1.f \
                  prttd.f prttrn.f prtxrg.f punch.f putbak.f \
                  putrev.f putstr.f qcmmnt.f qcontr.f qdoble.f \
                  qintgr.f qmap.f qmap2.f qname.f qquote.f \
                  qrfac.f qrsolv.f qsdiff.f qtoken.f quad.f \
                  quadit.f quadsd.f ratneg.f ratpos.f rdotlr.f \
                  rdotls.f rdregm.f realit.f regfix.f reglbl.f \
                  regvar.f regx11.f replac.f replyf.f resid.f \
                  resid2.f restor.f revchk.f revdrv.f revhdr.f \
                  revrse.f rgarma.f rgtdhl.f rho2.f rmatot.f \
                  rmfix.f rmlnvr.f rmlpyr.f rmotrv.f rmotss.f \
                  rmpadj.f rmtadj.f rndsa.f rngbuf.f roots.f \
                  round.f rplus.f rpoly.f rv2ss.f rvfixd.f \
                  rvrghd.f sautco.f savacf.f savchi.f savd8b.f \
                  savitr.f savmdc.f savmdl.f savmtx.f savotl.f \
                  savpk.f savspp.f savstp.f savtbl.f savwkf.f \
                  sceast.f scrmlt.f sdev.f sdxtrm.f seatad.f seatdg.f \
                  seatfc.f seatpr.f serates.f setadj.f setamx.f \
                  setapt.f setchr.f setcv.f setcvl.f setdp.f \
                  setint.f setlg.f setmdl.f setmv.f setopr.f \
                  setpt.f setrvp.f setspn.f setssp.f setup.f \
                  setwrt.f setxpt.f sfmax.f sfmsr.f sftest.f \
                  shlsrt.f shrink.f si.f sicp2.f sigex.f \
                  sigsub.f simul.f skparg.f skparm.f skpfcn.f \
                  skplst.f smeadl.f smpeak.f snrasp.f spcdrv.f \
                  spcrsd.f special.f specpeak.f spectrum.f spgrh.f \
                  spgrh2.f spmpar.f ss2rv.f ssap.f ssfnot.f \
                  ssftst.f sshist.f ssmdl.f ssort.f sspdrv.f \
                  ssphdr.f ssprep.f ssrit.f ssrng.f ssx11a.f \
                  ssxmdl.f stpitr.f strinx.f strtvl.f stvaln.f \
                  subset.f sumf.f sumry.f sumsqr.f svaict.f \
                  svamcm.f svchsd.f svdttm.f svf2f3.f svflt.f \
                  svfltd.f svfnrg.f svfreq.f svolit.f svoudg.f \
                  svpeak.f svrgcm.f svrvhd.f svspan.f table.f \
                  taper.f tblhdr.f td6var.f td7var.f tdaic.f \
                  tdftest.f tdlom.f tdset.f tdxtrm.f templs.f \
                  tfmts.f tfmts3.f totals.f transc.f trbias.f \
                  trnaic.f trnfcn.f tstdrv.f tstmd1.f tstmd2.f \
                  ttest.f uconv.f upespm.f usraic.f value.f \
                  varian.f varlog.f vars.f vsfa.f vsfb.f \
                  vsfc.f vtc.f vtest.f weight.f whitsp.f \
                  writln.f wrtdat.f wrtmss.f wrtotl.f \
                  wrttb2.f wrttbl.f wtxtrm.f x11aic.f x11ari.f \
                  x11int.f x11mdl.f x11plt.f x11pt1.f x11pt2.f \
                  x11pt3.f x11pt4.f x11ref.f x12hdr.f x12run.f \
                  xchng.f xpand.f xprmx.f xrgdiv.f xrgdrv.f \
                  xrghol.f xrgtrn.f xrlkhd.f xtrm.f yprmy.f \
                  yrly.f rvarma.f rvtdrg.f prtukp.f svtukp.f \
                  savtpk.f m2q.f chqsea.f npsa.f gennpsa.f prarma.f \
                  testodf.f

$(PROGRAM):     $(OBJS) $(LIBS)
	$(LINKER) -o $@ $(OBJS) $(LDMAP) $(LIBS) $(LDFLAGS)

clean:;         @rm -f $(OBJS)

install:   $(PROGRAM)
	@echo Installing $(PROGRAM) in $(DEST)
	@if not $(DEST)x==.x copy $(PROGRAM) $(DEST)
### OPUS MKMF:  Do not remove this line!  Automatic dependencies follow.

aaamain.o:  build.prm cchars.i chrt.cmn error.cmn hiddn.cmn htmlfile.cmn \
         htmlout.cmn htmlout.prm lex.i notset.prm nsums.i seatop.cmn \
         srslen.prm ssap.cmn ssap.prm stdio.i title.cmn units.cmn

abend.o:  dgnsvl.i error.cmn hiddn.cmn stdio.i svllog.cmn svllog.prm \
         units.cmn

acf.o:  autoq.cmn htmlout.cmn notset.prm srslen.prm stdio.i units.cmn

acfar.o:  autoq.cmn srslen.prm

acfdgn.o:  autoq.cmn htmlout.cmn mdldat.cmn mdlsvl.i model.cmn model.prm \
         srslen.prm svllog.cmn svllog.prm units.cmn

acfhdr.o:  htmlout.cmn model.cmn model.prm notset.prm prior.cmn prior.prm \
         srslen.prm

addadj.o:  error.cmn stdio.i units.cmn

addeas.o:  error.cmn model.prm notset.prm srslen.prm

addfix.o:  error.cmn fxreg.cmn mdldat.cmn model.cmn model.prm srslen.prm

addlom.o:  error.cmn model.prm notset.prm srslen.prm

addmul.o:  srslen.prm x11opt.cmn

addotl.o:  error.cmn hiddn.cmn htmlout.cmn mdldat.cmn model.cmn model.prm \
         srslen.prm stdio.i units.cmn

addsef.o:  error.cmn htmlout.cmn model.cmn model.prm notset.prm srslen.prm \
         stdio.i units.cmn

addtd.o:  error.cmn model.prm notset.prm srslen.prm

addusr.o:  arima.cmn error.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         urgbak.cmn usrreg.cmn

adestr.o:  model.prm srslen.prm

adjreg.o:  extend.cmn inpt.cmn model.cmn model.prm orisrs.cmn prior.cmn \
         prior.prm srslen.prm units.cmn x11adj.cmn x11fac.cmn x11log.cmn \
         x11ptr.cmn

adjsrs.o:  adj.cmn error.cmn picktd.cmn priadj.cmn prior.cmn prior.prm \
         priusr.cmn srslen.prm

adlabr.o:  model.prm srslen.prm

adotss.o:  arima.cmn error.cmn model.cmn model.prm srslen.prm

adpdrg.o:  error.cmn lex.i model.cmn model.prm notset.prm picktd.cmn \
         srslen.prm stdio.i units.cmn

adrgef.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm stdio.i \
         units.cmn

adrgim.o:  error.cmn lex.i model.cmn model.prm notset.prm picktd.cmn \
         srslen.prm

adsncs.o:  error.cmn htmlout.cmn model.prm srslen.prm stdio.i units.cmn

adthnk.o:  model.prm srslen.prm

aggmea.o:  srslen.prm

agr.o:  srslen.prm

agr1.o:  agr.cmn agrsrs.cmn notset.prm rev.cmn rev.prm revsrs.cmn \
         srslen.prm ssap.cmn ssap.prm sspdat.cmn

agr2.o:  adxser.cmn agr.cmn agrsrs.cmn cmpsvl.i cmptbl.i extend.cmn \
         htmlout.cmn inpt.cmn lzero.cmn orisrs.cmn priadj.cmn prior.cmn \
         prior.prm priusr.cmn seatcm.cmn seatlg.cmn srslen.prm stdio.i \
         svllog.cmn svllog.prm tbllog.cmn tbllog.prm title.cmn units.cmn \
         x11adj.cmn x11fac.cmn x11opt.cmn x11ptr.cmn x11srs.cmn

agr3.o:  adxser.cmn agr.cmn agrsrs.cmn build.prm cmptbl.i error.cmn \
         extend.cmn force.cmn hiddn.cmn htmlout.cmn inpt.cmn lex.i \
         notset.prm seatcm.cmn srslen.prm stdio.i tbllog.cmn tbllog.prm \
         title.cmn units.cmn x11adj.cmn x11fac.cmn x11msc.cmn x11opt.cmn \
         x11ptr.cmn x11srs.cmn

agr3s.o:  adxser.cmn agr.cmn agrsrs.cmn build.prm cmptbl.i error.cmn \
         extend.cmn force.cmn hiddn.cmn htmlout.cmn inpt.cmn lex.i \
         notset.prm seatcm.cmn srslen.prm stdio.i tbllog.cmn tbllog.prm \
         title.cmn units.cmn x11adj.cmn x11fac.cmn x11msc.cmn x11opt.cmn \
         x11ptr.cmn x11srs.cmn

agrxpt.o:  agr.cmn agrsrs.cmn extend.cmn srslen.prm x11ptr.cmn

amdest.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

amdfct.o:  arima.cmn error.cmn fxreg.cmn mdldat.cmn model.cmn model.prm \
         prior.cmn prior.prm srslen.prm units.cmn usrreg.cmn

amdid.o:  arima.cmn error.cmn extend.cmn htmlout.cmn mdldat.cmn mdlsvl.i \
         mdltbl.i model.cmn model.prm notset.prm prior.cmn prior.prm \
         srslen.prm stdio.i svllog.cmn svllog.prm tbllog.cmn tbllog.prm \
         units.cmn

amdid2.o:  adj.cmn arima.cmn error.cmn mdldat.cmn mdltbl.i model.cmn \
         model.prm srslen.prm stdio.i tbllog.cmn tbllog.prm units.cmn

amdprt.o:  error.cmn htmlout.cmn lkhd.cmn mdldat.cmn model.prm srslen.prm \
         units.cmn

amidot.o:  arima.cmn error.cmn extend.cmn mdltbl.i model.cmn model.prm \
         srslen.prm stdio.i tbllog.cmn tbllog.prm units.cmn

analts.o:  bench.i buffers.i build.i calc.i calfor.i calshr.i count.i \
         date.i dets.i dimensions.i dirs.i eee.i error.cmn hdflag.i \
         hiddn.cmn htmlout.cmn logtrace.i nsums.i peaks.i pinno.i preadtr.i \
         seastest.i seatserr.i sername.i sesfcast.i sfcast.i sform.i sig.i \
         sig1.i srslen.prm stdio.i stream.i strmodel.i titl.i title.cmn \
         unitmak.i units.cmn xarr.i

ansub1.o:  calc.i calfor.i calshr.i count.i cse.i dets.i dimensions.i \
         eee.i htmlout.cmn sesfcast.i srslen.prm stream.i units.cmn

ansub10.o:  calc.i calfor.i date.i dimensions.i dirs.i estb.i htmlout.cmn \
         logtrace.i models.i polynom.i prtous.i revs.i seatop.cmn sesfcast.i \
         sform.i srslen.prm stdio.i stream.i tbl5x.i units.cmn xarr.i

ansub11.o:  calc.i calfor.i dimensions.i estgc.i fft.i models.i srslen.prm \
         units.cmn xarr.i

ansub2.o:  ac02ae.i dimensions.i error.cmn func.i func2.i func3.i func4.i \
         func5.i htmlout.cmn min.i sform.i srslen.prm stdio.i stream.i test.i \
         unitmak.i units.cmn

ansub3.o:  dimensions.i estb.i estgc.i preadtr.i sform.i sig.i srslen.prm  \
         units.cmn

ansub4.o:  acfast.i acfst.i bartlett.i bench.i cross.i cxfinal.i dimensions.i \
         estb.i force.cmn hspect.i htmlout.cmn lzero.cmn models.i preadtr.i \
         priadj.cmn priusr.cmn serrlev.i sesfcast.i sfcast.i sform.i srslen.prm \
         stdio.i stream.i titl.i transcad.i units.cmn

ansub5.o:  bench.i dimensions.i error.cmn fitmod.i func5f1.i hspect.i \
         htmlout.cmn preadtr.i prtous.i rtestm.i sform.i spe.i spectra.i \
         spectrum.i srslen.prm stream.i testf1.i transcad.i units.cmn

ansub7.o:  amic.i dimensions.i error.cmn func.i func2.i func3.i func4.i \
         min.i srslen.prm test.i

ansub8.o:  build.i dirs.i stream.i

ansub9.o:  autoq.cmn dimensions.i error.cmn extend.cmn hiddn.cmn mdldat.cmn \
         model.cmn model.prm notset.prm orisrs.cmn rev.cmn rev.prm \
         seatad.cmn seatcm.cmn seatdg.cmn seatlg.cmn seatmd.cmn seatop.cmn \
         seattb.i srslen.prm sspinp.cmn stdio.i tbllog.cmn tbllog.prm \
         title.cmn units.cmn x11adj.cmn x11fac.cmn x11ptr.cmn

ar30rg.o:  srslen.prm

arima.o:  adj.cmn arima.cmn autoq.cmn error.cmn extend.cmn fxreg.cmn \
         hiddn.cmn htmlout.cmn inpt.cmn lkhd.cmn lzero.cmn mdldat.cmn \
         mdlsvl.i mdltbl.i missng.cmn model.cmn model.prm mq3.cmn notset.prm \
         orisrs.cmn picktd.cmn priadj.cmn prior.cmn prior.prm prittl.cmn \
         priusr.cmn rev.cmn rev.prm rho.cmn seatad.cmn spcsvl.i spctbl.i \
         srslen.prm stdio.i svllog.cmn svllog.prm tbllog.cmn tbllog.i \
         tbllog.prm title.cmn units.cmn usrreg.cmn x11log.cmn x11opt.cmn \
         x11ptr.cmn

armacr.o:  error.cmn htmlout.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         units.cmn

armafl.o:  mdldat.cmn model.cmn model.prm srslen.prm

armats.o:  mdldat.cmn model.cmn model.prm srslen.prm stdio.i units.cmn

autoer.o:  model.prm srslen.prm stdio.i units.cmn

automd.o:  adj.cmn arima.cmn error.cmn extend.cmn htmlout.cmn mdldat.cmn \
         mdlsvl.i mdltbl.i model.cmn model.prm notset.prm picktd.cmn prior.cmn \
         prior.prm srslen.prm stdio.i svllog.cmn svllog.prm tbllog.cmn \
         tbllog.prm title.cmn units.cmn usrreg.cmn

automx.o:  adj.cmn arima.cmn autoq.cmn error.cmn extend.cmn lex.i \
         mdldat.cmn mdltbl.i model.cmn model.prm notset.prm picktd.cmn \
         priadj.cmn prior.cmn prior.prm prittl.cmn priusr.cmn srslen.prm \
         stdio.i tbllog.cmn tbllog.prm title.cmn units.cmn usrreg.cmn \
         x11adj.cmn x11ptr.cmn

aver.o:  chrt.cmn srslen.prm units.cmn

bakusr.o:  model.prm srslen.prm urgbak.cmn

bench.o:  calfor.i dirs.i sform.i titl.i

bestmd.o:  lkhd.cmn notset.prm

bkdfmd.o:  arima.cmn mdldat.cmn model.cmn model.prm picktd.cmn prior.cmn \
         prior.prm srslen.prm ss2rv.cmn tbllog.prm usrreg.cmn x11adj.cmn

bldcov.o:  srslen.prm

bstget.o:  bstmdl.cmn mdldat.cmn model.cmn model.prm notset.prm srslen.prm

bstmdl.o:  bstmdl.cmn mdldat.cmn model.cmn model.prm picktd.cmn srslen.prm

btrit.o:  htmlout.cmn srslen.prm ssap.cmn ssap.prm sspvec.cmn title.cmn units.cmn

calcsc.o:  global.cmn model.prm srslen.prm

change.o:  goodob.cmn notset.prm srslen.prm x11opt.cmn

chitst.o:  mdldat.cmn model.prm notset.prm srslen.prm

chkadj.o:  model.cmn model.prm srslen.prm stdio.i units.cmn usrreg.cmn \
         x11adj.cmn x11log.cmn

chkchi.o:  arima.cmn error.cmn extend.cmn mdldat.cmn model.cmn model.prm \
         notset.prm prior.cmn prior.prm srslen.prm units.cmn usrreg.cmn

chkeas.o:  srslen.prm xeastr.cmn

chkmu.o:  arima.cmn error.cmn extend.cmn mdldat.cmn model.cmn model.prm \
         notset.prm prior.cmn prior.prm srslen.prm stdio.i units.cmn

chkorv.o:  cchars.i error.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         ssprep.cmn units.cmn

chkrt1.o:  error.cmn mdldat.cmn model.cmn model.prm notset.prm srslen.prm

chkrt2.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm stdio.i \
         units.cmn

chkrts.o:  mdldat.cmn model.cmn model.prm srslen.prm

chksmd.o:  error.cmn model.cmn model.prm srslen.prm stdio.i units.cmn

chktrn.o:  extend.cmn hiddn.cmn notset.prm stdio.i units.cmn x11ptr.cmn

chkuhg.o:  model.prm srslen.prm

chkurt.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

chkzro.o:  force.cmn goodob.cmn srslen.prm

chrt.o:  chrt.cmn error.cmn srslen.prm tbltitle.prm x11opt.cmn

chusrg.o:  arima.cmn error.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         usrreg.cmn

clrotl.o:  error.cmn model.cmn model.prm srslen.prm

cmpchi.o:  error.cmn model.cmn model.prm notset.prm picktd.cmn srslen.prm \
         units.cmn usrreg.cmn usrxrg.cmn

cmpstr.o:  lex.i

cncrnt.o:  cchars.i error.cmn notset.prm seattb.i tbllog.cmn tbllog.prm

cnvmdl.o:  error.cmn model.cmn model.prm srslen.prm

coladd.o:  htmlout.cmn stdio.i units.cmn

combft.o:  hiddn.cmn srslen.prm ssap.prm ssft.cmn tests.cmn title.cmn \
         units.cmn

compb.o:  srslen.prm ssap.cmn ssap.prm

compdiag.o:  srslen.prm

compmse.o:  srslen.prm

cormtx.o:  error.cmn model.cmn model.prm notset.prm srslen.prm units.cmn

corplt.o:  stdio.i units.cmn

covar.o:  model.prm notset.prm srslen.prm

ctodat.o:  model.prm srslen.prm

cvrerr.o:  error.cmn stdio.i units.cmn

delstr.o:  error.cmn stdio.i units.cmn

deltst.o:  error.cmn hiddn.cmn mdldat.cmn model.cmn model.prm notset.prm \
         srslen.prm stdio.i units.cmn

desreg.o:  error.cmn htmlout.cmn model.prm srslen.prm title.cmn units.cmn

divgud.o:  goodob.cmn notset.prm srslen.prm

divsub.o:  srslen.prm x11opt.cmn

dlrgef.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm stdio.i \
         units.cmn

dlusrg.o:  arima.cmn model.prm srslen.prm stdio.i units.cmn usrreg.cmn

dot.o:  chrt.cmn srslen.prm

dsarma.o:  htmlout.cmn model.cmn model.prm srslen.prm units.cmn

dtoc.o:  savcmn.cmn stdio.i units.cmn

easaic.o:  adj.cmn arima.cmn error.cmn extend.cmn htmlout.cmn lkhd.cmn \
         mdldat.cmn model.cmn model.prm notset.prm prior.cmn prior.prm \
         srslen.prm units.cmn x11adj.cmn

easter.o:  srslen.prm xeastr.cmn

editor.o:  adj.cmn agr.cmn agrsrs.cmn arima.cmn cmpsvl.i cmptbl.i dgnsvl.i \
         error.cmn extend.cmn filetb.cmn force.cmn frctbl.i goodob.cmn \
         hender.prm hiddn.cmn htmlfile.cmn htmlout.cmn htmlout.prm inpt.cmn \
         lzero.cmn mdldat.cmn mdlsvl.i mdltbl.i metadata.cmn metadata.prm \
         missng.cmn model.cmn model.prm notset.prm picktd.cmn prior.cmn \
         prior.prm priusr.cmn rho.cmn setsvl.i spcsvl.i spctbl.i srslen.prm \
         ssap.cmn ssap.prm sspinp.cmn stdio.i sums.i sumtab.prm sumtab.var \
         svllog.cmn svllog.prm tbllog.cmn tbllog.prm title.cmn units.cmn \
         usrreg.cmn usrxrg.cmn work2.cmn x11adj.cmn x11log.cmn x11msc.cmn \
         x11opt.cmn x11ptr.cmn x11reg.cmn x11svl.i xeastr.cmn xrgfct.cmn \
         xrgmdl.cmn xrgum.cmn xtrm.cmn

eltlen.o:  stdio.i units.cmn

ends.o:  hender.prm

errhdr.o:  hiddn.cmn htmlout.cmn notset.prm rev.cmn rev.prm srslen.prm \
         ssap.prm ssft.cmn units.cmn

estrmu.o:  srslen.prm

exctma.o:  mdldat.cmn model.cmn model.prm srslen.prm

extend.o:  extend.cmn mdldat.cmn model.prm srslen.prm stdio.i units.cmn \
         x11msc.cmn x11opt.cmn x11ptr.cmn

extsgnl.o:  srslen.prm

f3cal.o:  inpt2.cmn srslen.prm tests.cmn work2.cmn x11opt.cmn x11ptr.cmn

f3gen.o:  mq3.cmn srslen.prm work2.cmn

fclose.o:  stdio.i

fcnar.o:  error.cmn mdldat.cmn model.cmn model.prm notset.prm series.cmn \
         srslen.prm stdio.i units.cmn

fcstxy.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

fgen.o:  mq3.cmn srslen.prm title.cmn x11opt.cmn

fopen.o:  stdio.i units.cmn

fstop.o:  stdio.i

ftest.o:  agr.cmn hiddn.cmn htmlout.cmn srslen.prm ssap.prm ssft.cmn tests.cmn \
         title.cmn units.cmn x11msc.cmn x11opt.cmn

fxshfr.o:  global.cmn model.prm srslen.prm

gendff.o:  srslen.prm

genfoot.o:  htmlout.prm srslen.prm ssap.cmn ssap.prm

genfor.o:  agr.cmn filetb.cmn htmlout.cmn stdio.i title.cmn units.cmn

genindex.o:  error.cmn htmlfile.cmn level.prm model.prm model.cmn spctbl.i \
         srslen.prm ssap.prm stdio.i tbllog.prm tbltitle.prm x11opt.cmn

genqs.o:  adxser.cmn arima.cmn extend.cmn htmlout.cmn inpt.cmn model.cmn \
         model.prm notset.prm orisrs.cmn rho.cmn seatcm.cmn seatlg.cmn \
         srslen.prm tbllog.cmn tbllog.prm units.cmn x11adj.cmn x11fac.cmn \
         x11ptr.cmn x11srs.cmn

genrtt.o:  mdldat.cmn model.cmn model.prm srslen.prm

genskip.o: hiddn.cmn htmlout.cmn srslen.prm stdio.i units.cmn x11opt.cmn

genssm.o:  calc.i dimensions.i notset.prm seatdg.cmn srslen.prm

getadj.o:  error.cmn lex.i notset.prm srslen.prm stdio.i svllog.i tbllog.i \
         units.cmn

getchk.o:  error.cmn hiddn.cmn lex.i mdltbl.i notset.prm stdio.i svllog.i \
         tbllog.cmn tbllog.i tbllog.prm units.cmn

getchr.o:  lex.i

getcmp.o:  error.cmn lex.i notset.prm stdio.i svllog.i tbllog.cmn tbllog.i \
         tbllog.prm units.cmn

getdat.o:  lex.i srslen.prm

getdbl.o:  lex.i

getdes.o:  desadj.prm desadj.var descm2.prm descm2.var descmp.prm descmp.var \
         desdgn.prm desdgn.var desdg2.prm desdg2.var desfsa.prm desfsa.var \
         desmdl.prm desmdl.var desset.prm desset.var desst2.prm desst2.var \
         desspc.prm desspc.var dessrs.prm dessrs.var desx11.prm desx11.var \
         desxrg.prm desxrg.var tbltitle.prm

getdiag.o:  acfast.i across.i hiddn.cmn mdldat.cmn model.prm revs.i \
         srslen.prm stdio.i tbl5x.i units.cmn

getfcn.o:  lex.i notset.prm

getfrc.o:  error.cmn lex.i notset.prm stdio.i tbllog.i units.cmn

getgr.o:  sform.i srslen.prm

getid.o:  error.cmn lex.i model.cmn model.prm notset.prm srslen.prm \
         tbllog.i

getint.o:  lex.i

getivc.o:  lex.i notset.prm

getmdl.o:  error.cmn lex.i model.cmn model.prm srslen.prm

getmtd.o:  extend.cmn mdldat.cmn model.cmn model.prm notset.prm picktd.cmn \
         srslen.prm x11opt.cmn

getopr.o:  error.cmn lex.i model.prm notset.prm srslen.prm

getprt.o:  hiddn.cmn level.prm level.var lex.i stdio.i table.prm table.var \
         tbllog.cmn tbllog.prm units.cmn

getreg.o:  error.cmn lex.i mdldat.cmn model.cmn model.prm notset.prm \
         picktd.cmn srslen.prm stdio.i svllog.i tbllog.i units.cmn \
         usrreg.cmn


getrev.o:  rev.cmn rev.prm revsrs.cmn revtrg.cmn srslen.prm stdio.i \
         units.cmn

getsav.o:  lex.i stable.prm stable.var stdio.i tbllog.cmn tbllog.prm \
         units.cmn

getsma.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

getsrs.o:  error.cmn lex.i notset.prm srslen.prm stdio.i svllog.i \
         tbllog.cmn tbllog.i tbllog.prm units.cmn


getssp.o:  error.cmn lex.i notset.prm srslen.prm ssap.prm svllog.i \
         tbllog.i

getstr.o:  error.cmn stdio.i units.cmn

getsvl.o:  lex.i stdio.i svllog.cmn svllog.prm svltbl.prm svltbl.var \
         units.cmn

getttl.o:  error.cmn lex.i notset.prm

getx11.o:  error.cmn lex.i notset.prm srslen.prm stdio.i svllog.i \
         tbllog.cmn tbllog.i tbllog.prm units.cmn


getxop.o:  error.cmn stdio.i

getxtd.o:  model.cmn model.prm notset.prm picktd.cmn srslen.prm

glbshk.o:  srslen.prm x11ptr.cmn

gnfcrv.o:  rev.cmn rev.prm revsrs.cmn srslen.prm

grzlst.o:  chrt.cmn srslen.prm

grzmth.o:  chrt.cmn srslen.prm

grzmyr.o:  chrt.cmn srslen.prm

gtarg.o:  error.cmn lex.i notset.prm

gtarma.o:  error.cmn lex.i model.cmn model.prm notset.prm srslen.prm

gtauto.o:  error.cmn lex.i mdltbl.i notset.prm stdio.i svllog.i tbllog.cmn \
         tbllog.i tbllog.prm units.cmn

gtautx.o:  error.cmn lex.i model.prm notset.prm srslen.prm svllog.i \
         tbllog.i

gtdcnm.o:  lex.i

gtdcvc.o:  error.cmn lex.i

gtdpvc.o:  error.cmn lex.i notset.prm

gtdtvc.o:  error.cmn lex.i notset.prm

gtedit.o:  stdio.i units.cmn x11msc.cmn

gtestm.o:  error.cmn lex.i mdltbl.i model.cmn model.prm notset.prm \
         srslen.prm stdio.i svllog.i tbllog.cmn tbllog.i tbllog.prm \
         units.cmn


gtfcst.o:  error.cmn lex.i notset.prm srslen.prm tbllog.i

gtfldt.o:  lex.i notset.prm stdio.i units.cmn

gtfrcm.o:  lex.i stdio.i units.cmn

gtfree.o:  stdio.i units.cmn

gtinpt.o:  adj.cmn agr.cmn arima.cmn deftab.prm deftab.var error.cmn \
         extend.cmn force.cmn fxreg.cmn hiddn.cmn lex.i mdldat.cmn \
         metadata.cmn metadata.prm missng.cmn model.cmn model.prm \
         notset.prm picktd.cmn priadj.cmn prior.cmn prior.prm prittl.cmn \
         priusr.cmn rev.cmn rev.prm revtrg.cmn rho.cmn savcmn.cmn \
         seatlg.cmn seatop.cmn srslen.prm sspinp.cmn stdio.i sumtab.prm \
         sumtab.var svllog.cmn svllog.prm tbllog.cmn tbllog.prm title.cmn \
         units.cmn usrreg.cmn usrxrg.cmn x11adj.cmn x11log.cmn x11msc.cmn \
         x11opt.cmn x11reg.cmn xclude.cmn xrgfct.cmn xrgmdl.cmn xrgum.cmn \
         xtrm.cmn

gtinvl.o:  error.cmn lex.i mdldat.cmn model.cmn model.prm srslen.prm

gtmdfl.o:  error.cmn model.cmn model.prm notset.prm srslen.prm stdio.i \
         usrreg.cmn x11adj.cmn

gtmtdt.o:  error.cmn lex.i metadata.cmn metadata.prm notset.prm stdio.i

gtmtfl.o:  notset.prm stdio.i

gtnmvc.o:  error.cmn lex.i notset.prm

gtotlr.o:  error.cmn lex.i notset.prm svllog.i tbllog.i

gtpdrg.o:  error.cmn lex.i

gtrgdt.o:  lex.i

gtrgpt.o:  model.cmn model.prm srslen.prm

gtrgvl.o:  lex.i model.prm srslen.prm

gtrvst.o:  error.cmn lex.i notset.prm rev.prm srslen.prm stdio.i svllog.i \
         tbllog.i units.cmn

gtseat.o:  error.cmn lex.i notset.prm stdio.i svllog.i tbllog.cmn tbllog.i \
         tbllog.prm units.cmn

gtspec.o:  error.cmn lex.i notset.prm srslen.prm stdio.i svllog.i \
         tbllog.cmn tbllog.i tbllog.prm units.cmn

gttrmo.o:  stdio.i units.cmn x11msc.cmn

gtx11d.o:  htmlout.cmn notset.prm stdio.i units.cmn x11msc.cmn

gtx12s.o:  htmlout.cmn stdio.i units.cmn

gtxreg.o:  error.cmn htmlout.cmn lex.i mdldat.cmn model.cmn model.prm \
         notset.prm picktd.cmn srslen.prm stdio.i svllog.i tbllog.i \
         units.cmn usrxrg.cmn

hist.o:  error.cmn htmlout.cmn model.prm srslen.prm units.cmn

histx.o:  error.cmn htmlout.cmn srslen.prm ssap.prm units.cmn

hndend.o:  hender.prm

hndtrn.o:  hender.prm x11msc.cmn

holday.o:  error.cmn extend.cmn hiddn.cmn lzero.cmn srslen.prm tbllog.cmn \
         tbllog.prm x11adj.cmn x11fac.cmn x11opt.cmn x11ptr.cmn x11tbl.i \
         xeastr.cmn

holidy.o:  kdate.prm srslen.prm xeastr.cmn

hrest.o:  autoq.cmn error.cmn model.prm srslen.prm units.cmn

htmlfortable.o:  dimensions.i srslen.prm stream.i

htmlout.o:  build.i dimensions.i dirs.i htmlout.cmn models.i peaks.i \
         polynom.i seatserr.i sername.i sform.i sig.i spectra.i \
         spectrum.i srslen.prm stdio.i stream.i sums.i transcad.i units.cmn

iddiff.o:  arima.cmn error.cmn extend.cmn htmlout.cmn mdldat.cmn mdltbl.i \
         model.cmn model.prm notset.prm prior.cmn prior.prm srslen.prm \
         stdio.i tbllog.cmn tbllog.prm units.cmn

idmdl.o:  acfptr.prm error.cmn mdldat.cmn mdltbl.i model.cmn model.prm \
         srslen.prm stdio.i tbllog.cmn tbllog.i tbllog.prm units.cmn

idotlr.o:  cchars.i error.cmn fxreg.cmn hiddn.cmn htmlout.cmn mdldat.cmn \
         mdltbl.i model.cmn model.prm notset.prm srslen.prm stdio.i \
         tbllog.cmn tbllog.prm units.cmn xrgtbl.i

idpeak.o:  notset.prm units.cmn

initdg.o:  error.cmn notset.prm seatdg.cmn setsvl.i svllog.cmn svllog.prm \
         units.cmn

initst.o:  notset.prm seatlg.cmn seatmd.cmn srslen.prm stcfcm.cmn

inpter.o:  htmlout.cmn lex.i stdio.i title.cmn units.cmn

insdbl.o:  error.cmn

insint.o:  error.cmn

inslg.o:  error.cmn

insopr.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

insptr.o:  error.cmn stdio.i units.cmn

insstr.o:  error.cmn

intgpg.o:  mdldat.cmn model.cmn model.prm srslen.prm

intinp.o:  error.cmn lex.i

invfcn.o:  stdio.i units.cmn

issame.o:  goodob.cmn srslen.prm

itoc.o:  stdio.i units.cmn

itrerr.o:  htmlout.cmn stdio.i units.cmn

kfcn.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

kwtest.o:  hiddn.cmn srslen.prm tests.cmn title.cmn units.cmn

lendp.o:  notset.prm

lex.o:  cchars.i lex.i

lmdif.o:  error.cmn

loadxr.o:  arima.cmn mdldat.cmn model.cmn model.prm picktd.cmn prior.cmn \
         prior.prm srslen.prm usrreg.cmn usrxrg.cmn x11adj.cmn xrgmdl.cmn

locshk.o:  srslen.prm units.cmn x11ptr.cmn

lomaic.o:  adj.cmn arima.cmn error.cmn extend.cmn htmlout.cmn lkhd.cmn \
         mdldat.cmn model.cmn model.prm notset.prm picktd.cmn prior.cmn \
         prior.prm srslen.prm units.cmn

lstpth.o:  lex.i

makadj.o:  adj.cmn priadj.cmn prior.cmn prior.prm priusr.cmn srslen.prm

makotl.o:  model.prm srslen.prm

makttl.o:  cmptbl.i error.cmn fctlbl.prm fctlbl.var force.cmn frctbl.i \
         mq3.cmn stdio.i tbllbl.prm tbllbl.var tbltitle.prm units.cmn \
         x11tbl.i

map.o:  lex.i

matrix.o:  matrix1.i matrix2.i sums.i

mdlchk.o:  autoq.cmn mdldat.cmn model.cmn model.prm srslen.prm

mdlfix.o:  mdldat.cmn model.cmn model.prm notset.prm srslen.prm

mdlinp.o:  error.cmn lex.i

mdlint.o:  mdldat.cmn model.cmn model.prm srslen.prm

mdlmch.o:  notset.prm

mdlset.o:  error.cmn model.cmn model.prm srslen.prm stdio.i units.cmn

medabs.o:  model.prm srslen.prm stdio.i units.cmn

mflag.o:  notset.prm srslen.prm ssap.cmn ssap.prm sspvec.cmn

mkback.o:  adj.cmn arima.cmn cchars.i error.cmn extend.cmn hiddn.cmn \
         htmlout.cmn mdldat.cmn mdltbl.i model.cmn model.prm priusr.cmn \
         savcmn.cmn srslen.prm stdio.i tbllog.cmn tbllog.prm units.cmn \
         x11adj.cmn x11fac.cmn x11log.cmn x11opt.cmn

mkealb.o:  error.cmn notset.prm

mkfreq.o:  notset.prm spcidx.cmn

mklnlb.o:  error.cmn notset.prm

mkmdsn.o:  error.cmn

mkoprt.o:  error.cmn model.prm srslen.prm

mkotky.o:  error.cmn fxreg.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         units.cmn

mkpeak.o:  spcidx.cmn

mkshdr.o:  force.cmn hiddn.cmn picktd.cmn prior.cmn prior.prm srslen.prm \
         x11adj.cmn x11log.cmn x11opt.cmn

mksplb.o:  spctbl.i

mkspst.o:  hiddn.cmn prior.cmn prior.prm srslen.prm x11adj.cmn x11log.cmn \
         x11opt.cmn

mkssky.o:  srslen.prm ssap.cmn ssap.prm title.cmn units.cmn

mkstlb.o:  spctbl.i

mktdlb.o:  error.cmn notset.prm

mlist.o:  htmlout.cmn notset.prm srslen.prm ssap.cmn ssap.prm sspvec.cmn \
         title.cmn units.cmn

mltpos.o:  model.prm srslen.prm

month.o:  chrt.cmn error.cmn srslen.prm

mstest.o:  hiddn.cmn htmlout.cmn srslen.prm ssap.prm ssft.cmn tests.cmn \
         title.cmn units.cmn x11opt.cmn

mulqmat.o:  srslen.prm

mxpeak.o:  notset.prm

newest.o:  global.cmn model.prm srslen.prm

nextk.o:  global.cmn model.prm srslen.prm

nmlmdl.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm stdio.i \
         units.cmn

nofcst.o:  arima.cmn extend.cmn model.prm prior.cmn prior.prm srslen.prm

nrmtst.o:  htmlout.cmn nrmtst.var stdio.i units.cmn

olsreg.o:  stdio.i units.cmn

opnfil.o:  error.cmn filetb.cmn filext.prm filext.var gmeta.prm gmeta.var \
         htmlout.cmn notset.prm stdio.i tbllog.prm tbltitle.prm

otsort.o:  arima.cmn error.cmn mdldat.cmn model.cmn model.prm srslen.prm

outchr.o:  chrt.cmn htmlout.cmn srslen.prm tbltitle.prm units.cmn

pacf.o:  htmlout.cmn srslen.prm units.cmn

pass0.o:  arima.cmn error.cmn extend.cmn model.cmn model.prm picktd.cmn \
         prior.cmn prior.prm srslen.prm units.cmn

pass2.o:  adj.cmn arima.cmn error.cmn extend.cmn inpt.cmn mdldat.cmn \
         model.cmn model.prm picktd.cmn prior.cmn prior.prm priusr.cmn \
         series.cmn srslen.prm units.cmn

pctrit.o:  dgnsvl.i htmlout.cmn notset.prm srslen.prm ssap.prm svllog.cmn \
         svllog.prm units.cmn

polynom.o:  dimensions.i hspect.i models.i polynom.i srslen.prm stream.i

pracf2.o:  error.cmn mdldat.cmn mdltbl.i model.cmn model.prm notset.prm \
         srslen.prm stdio.i tbllog.cmn tbllog.prm units.cmn

prafce.o:  htmlout.cmn title.cmn

pragr2.o:  error.cmn tbllog.cmn tbllog.prm

prfcrv.o:  cchars.i error.cmn htmlout.cmn rev.cmn rev.prm revsrs.cmn \
         srslen.prm svllog.cmn svllog.prm tbllog.cmn tbllog.prm title.cmn \
         units.cmn

pritd.o:  error.cmn model.prm srslen.prm

prlkhd.o:  extend.cmn hiddn.cmn htmlout.cmn lkhd.cmn lzero.cmn mdldat.cmn \
         mdltbl.i model.cmn model.prm notset.prm srslen.prm units.cmn \
         x11adj.cmn x11fac.cmn x11log.cmn x11opt.cmn

procflts.o:  cmpflts.i

prothd.o:  error.cmn htmlout.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         units.cmn

prprad.o:  htmlout.cmn units.cmn

prrvob.o:  srslen.prm tfmts.cmn title.cmn units.cmn

prshd2.o:  error.cmn htmlout.cmn title.cmn units.cmn

prtacf.o:  acfptr.prm error.cmn mdldat.cmn mdlsvl.i model.cmn model.prm \
         notset.prm srslen.prm stdio.i svllog.cmn svllog.prm tbllog.cmn \
         tbllog.prm units.cmn

prtadj.o:  error.cmn lzero.cmn mdltbl.i orisrs.cmn priadj.cmn priusr.cmn \
         srslen.prm tbllog.cmn tbllog.prm

prtagr.o:  error.cmn tbllog.cmn tbllog.prm

prtamd.o:  error.cmn fxreg.cmn htmlout.cmn model.cmn model.prm notset.prm \
         srslen.prm title.cmn units.cmn

prtchi.o:  htmlout.cmn model.prm notset.prm srslen.prm title.cmn

prtcol.o:  srslen.prm

prtd8b.o:  arima.cmn error.cmn extend.cmn hiddn.cmn mdldat.cmn model.cmn \
         model.prm notset.prm srslen.prm stdio.i tbltitle.prm tfmts.cmn \
         tfmts.prm tfmts.var tfmts2.prm tfmts2.var title.cmn units.cmn \
         x11adj.cmn x11opt.cmn

prtd9a.o:  error.cmn hiddn.cmn srslen.prm tfmts.cmn tfmts.prm tfmts.var \
         units.cmn x11opt.cmn

prtdtb.o:  htmlout.cmn mq3.cmn notset.prm picktd.cmn prior.cmn prior.prm \
         srslen.prm title.cmn units.cmn x11opt.cmn

prtdwr.o:  arima.cmn mdldat.cmn model.cmn model.prm srslen.prm units.cmn

prterr.o:  error.cmn hiddn.cmn mdldat.cmn mdltbl.i model.cmn model.prm \
         srslen.prm stdio.i tbllog.cmn tbllog.prm units.cmn

prterx.o:  arima.cmn error.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         stdio.i tbllog.cmn tbllog.prm units.cmn xrgtbl.i

prtf2.o:  htmlout.cmn inpt2.cmn mq3.cmn srslen.prm tests.cmn title.cmn \
         work2.cmn x11opt.cmn

prtf2w.o:  inpt2.cmn mq3.cmn srslen.prm tests.cmn title.cmn work2.cmn \
         x11opt.cmn

prtfct.o:  adj.cmn cchars.i error.cmn hiddn.cmn htmlout.cmn mdldat.cmn \
         mdltbl.i model.cmn model.prm prior.cmn prior.prm priusr.cmn \
         rev.cmn rev.prm revsrs.cmn savcmn.cmn seatad.cmn srslen.prm \
         stdio.i tbllog.cmn tbllog.prm units.cmn x11fac.cmn x11log.cmn

prtft.o:  htmlout.cmn hiddn.cmn model.prm notset.prm srslen.prm title.cmn \
         units.cmn

prtitr.o:  error.cmn htmlout.cmn mdltbl.i model.cmn model.prm notset.prm \
         series.cmn srslen.prm tbllog.cmn tbllog.prm units.cmn

prtlog.o:  htmlout.cmn stdio.i

prtmdl.o:  cchars.i cogreg.prm cogreg.var error.cmn hiddn.cmn htmlout.cmn \
         htmlout.prm mdldat.cmn mdldg.cmn mdlsvl.i mdltbl.i model.cmn \
         model.prm notset.prm picktd.cmn rev.cmn rev.prm srslen.prm \
         sspinp.cmn svllog.cmn svllog.prm title.cmn units.cmn

prtmsp.o:  error.cmn htmlout.cmn title.cmn units.cmn

prtmsr.o:  cchars.i desadj.prm desadj.var error.cmn rev.cmn rev.prm \
         srslen.prm tbllog.cmn tbllog.prm tbltitle.prm tfmts.cmn tfmts.prm \
         tfmts.var units.cmn

prtmtx.o:  error.cmn htmlout.cmn title.cmn units.cmn

prtnfn.o:  htmlout.cmn title.cmn units.cmn

prtopt.o:  htmlout.cmn model.cmn model.prm srslen.prm units.cmn

prtref.o:  error.cmn mdldat.cmn mdltbl.i model.cmn model.prm srslen.prm \
         title.cmn usrreg.cmn

prtrev.o:  cchars.i desdgn.prm desdgn.var dgnsvl.i error.cmn htmlout.cmn \
         notset.prm rev.cmn rev.prm srslen.prm svllog.cmn svllog.prm \
         tbllog.cmn tbllog.prm tbltitle.prm tfmts.cmn units.cmn x11msc.cmn \
         x11opt.cmn

prtrts.o:  cchars.i error.cmn htmlout.cmn mdldat.cmn mdltbl.i model.cmn \
         model.prm srslen.prm units.cmn

prtrv2.o:  cchars.i desdgn.prm desdgn.var dgnsvl.i error.cmn htmlout.cmn \
         notset.prm rev.cmn rev.prm srslen.prm svllog.cmn svllog.prm \
         tbllog.cmn tbllog.prm tbltitle.prm tfmts.cmn units.cmn x11msc.cmn \
         x11opt.cmn

prtshd.o:  error.cmn htmlout.cmn title.cmn units.cmn

prttbl.o:  model.prm srslen.prm title.cmn units.cmn

prttd.o:  htmlout.cmn units.cmn

prttrn.o:  error.cmn hiddn.cmn htmlout.cmn model.prm notset.prm srslen.prm \
         stdio.i tbltitle.prm tfmts.cmn tfmts.prm tfmts.var tfmts2.prm \
         tfmts2.var title.cmn units.cmn x11opt.cmn x11ptr.cmn

prtxrg.o:  cchars.i cogreg.prm cogreg.var error.cmn hiddn.cmn htmlout.cmn \
         htmlout.prm mdldat.cmn model.cmn model.prm notset.prm picktd.cmn \
         srslen.prm units.cmn x11reg.cmn

punch.o:  extend.cmn hiddn.cmn srslen.prm title.cmn x11opt.cmn

putbak.o:  lex.i

putrev.o:  agr.cmn

putstr.o:  error.cmn

qcmmnt.o:  cchars.i lex.i

qcontr.o:  agr.cmn title.cmn units.cmn

qintgr.o:  lex.i

qmap2.o:  cmptbl.i error.cmn force.cmn frctbl.i srslen.prm tbllog.cmn \
         tbllog.prm

qname.o:  lex.i

qquote.o:  cchars.i lex.i

qsdiff.o:  srslen.prm

qtoken.o:  cchars.i lex.i

quadit.o:  global.cmn model.prm srslen.prm

ratneg.o:  model.prm srslen.prm

ratpos.o:  model.prm srslen.prm

rdotlr.o:  lex.i model.prm srslen.prm

rdotls.o:  lex.i model.prm srslen.prm

realit.o:  global.cmn model.prm srslen.prm

regfix.o:  mdldat.cmn model.cmn model.prm notset.prm srslen.prm

reglbl.o:  model.prm srslen.prm

regvar.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm stdio.i \
         units.cmn usrreg.cmn

regx11.o:  error.cmn mdldat.cmn model.cmn model.prm series.cmn srslen.prm \
         stdio.i units.cmn xclude.cmn

replyf.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

resid.o:  htmlout.cmn stdio.i units.cmn

resid2.o:  srslen.prm

restor.o:  arima.cmn lzero.cmn mdldat.cmn model.cmn model.prm picktd.cmn \
         prior.cmn prior.prm srslen.prm ssprep.cmn usrreg.cmn x11adj.cmn \
         x11opt.cmn

revchk.o:  error.cmn extend.cmn model.cmn model.prm rev.cmn rev.prm \
         revtbl.i revtrg.cmn seatlg.cmn srslen.prm stdio.i tbllog.cmn \
         tbllog.prm units.cmn x11adj.cmn x11log.cmn x11reg.cmn

revdrv.o:  arima.cmn cchars.i dgnsvl.i error.cmn extend.cmn hiddn.cmn \
         htmlout.cmn inpt.cmn lkhd.cmn mdldat.cmn mdltbl.i missng.cmn \
         model.cmn model.prm notset.prm orisrs.cmn otlrev.cmn otxrev.cmn \
         picktd.cmn rev.cmn rev.prm revsrs.cmn revtbl.i revtrg.cmn \
         seatdg.cmn srslen.prm stdio.i svllog.cmn svllog.prm tbllog.cmn \
         tbllog.prm title.cmn units.cmn usrreg.cmn usrxrg.cmn x11adj.cmn \
         x11log.cmn x11opt.cmn x11ptr.cmn x11reg.cmn xeastr.cmn xrgmdl.cmn \
         xrgtbl.i

revhdr.o:  error.cmn htmlout.cmn rev.cmn rev.prm revtrg.cmn srslen.prm \
         units.cmn

rgarma.o:  error.cmn hiddn.cmn mdldat.cmn mdltbl.i model.cmn model.prm \
         series.cmn srslen.prm stdio.i tbllog.cmn tbllog.prm units.cmn

rgtdhl.o:  arima.cmn error.cmn extend.cmn mdldat.cmn model.cmn model.prm \
         prior.cmn prior.prm srslen.prm x11log.cmn x11msc.cmn x11opt.cmn \
         x11ptr.cmn x11reg.cmn x11srs.cmn

rmatot.o:  cchars.i error.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         ssprep.cmn title.cmn units.cmn

rmfix.o:  error.cmn fxreg.cmn mdldat.cmn model.cmn model.prm srslen.prm

rmlnvr.o:  error.cmn model.cmn model.prm notset.prm picktd.cmn srslen.prm

rmlpyr.o:  adj.cmn arima.cmn error.cmn inpt.cmn mdldat.cmn model.cmn \
         model.prm picktd.cmn prior.cmn prior.prm priusr.cmn srslen.prm

rmotrv.o:  cchars.i error.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         title.cmn units.cmn

rmotss.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

rmpadj.o:  lzero.cmn priadj.cmn prior.cmn prior.prm priusr.cmn srslen.prm

rmtadj.o:  lzero.cmn priadj.cmn prior.cmn prior.prm priusr.cmn srslen.prm

rndsa.o:  srslen.prm stdio.i units.cmn x11opt.cmn

rngbuf.o:  cchars.i lex.i stdio.i

roots.o:  model.prm srslen.prm stdio.i units.cmn

rplus.o:  notset.prm srslen.prm ssap.cmn ssap.prm sspvec.cmn

rpoly.o:  global.cmn model.prm srslen.prm

rv2ss.o:  adj.cmn agr.cmn arima.cmn extend.cmn hiddn.cmn inpt.cmn \
         lzero.cmn mdldat.cmn model.prm orisrs.cmn seatlg.cmn seatmd.cmn \
         srslen.prm ss2rv.cmn ssprep.cmn stcfcm.cmn tbllog.cmn tbllog.prm \
         usrxrg.cmn x11opt.cmn x11reg.cmn xeastr.cmn xrgmdl.cmn

rvfixd.o:  model.prm srslen.prm

rvrghd.o:  cchars.i revtbl.i title.cmn

savacf.o:  autoq.cmn cchars.i error.cmn mdltbl.i notset.prm srslen.prm

savchi.o:  model.prm notset.prm srslen.prm units.cmn

savd8b.o:  cchars.i error.cmn filext.prm filext.var tbllog.prm

savitr.o:  cchars.i mdldat.cmn mdltbl.i model.cmn model.prm savcmn.cmn \
         srslen.prm

savmdc.o:  notset.prm seatmd.cmn srslen.prm

savmdl.o:  error.cmn mdldat.cmn mdltbl.i model.cmn model.prm picktd.cmn \
         srslen.prm usrreg.cmn x11adj.cmn

savmtx.o:  cchars.i error.cmn model.prm savcmn.cmn srslen.prm

savotl.o:  error.cmn htmlout.cmn mdldat.cmn model.cmn model.prm srslen.prm \
         units.cmn usrreg.cmn

savpk.o:  htmlout.cmn rho.cmn spcsvl.i svllog.cmn svllog.prm units.cmn

savspp.o:  cchars.i error.cmn

savstp.o:  cchars.i error.cmn

savtbl.o:  cchars.i error.cmn filext.prm filext.var tbllog.prm

savwkf.o:  cchars.i error.cmn notset.prm seatmd.cmn srslen.prm

sdev.o:  notset.prm

sdxtrm.o:  srslen.prm xtrm.cmn

seatad.o:  seatcm.cmn seatlg.cmn srslen.prm x11adj.cmn x11fac.cmn \
         x11ptr.cmn

seatdg.o:  calc.i cmpflts.i dimensions.i error.cmn force.cmn htmlout.cmn \
         inpt.cmn mdldat.cmn model.cmn model.prm notset.prm orisrs.cmn \
         rev.cmn rev.prm revtbl.i seatcm.cmn seatdg.cmn seatlg.cmn \
         seattb.i setsvl.i sig.i srslen.prm stdio.i svllog.cmn svllog.prm \
         tbllog.cmn tbllog.prm units.cmn x11ptr.cmn

seatfc.o:  force.cmn inpt.cmn orisrs.cmn seatcm.cmn srslen.prm x11adj.cmn \
         x11fac.cmn x11ptr.cmn

seatpr.o:  adj.cmn calc.i desset.prm desset.var dimensions.i error.cmn \
         extend.cmn force.cmn frctbl.i inpt.cmn mdltbl.i notset.prm \
         priusr.cmn seatcm.cmn seatdg.cmn seatlg.cmn seatmd.cmn seattb.i \
         sig.i spctbl.i srslen.prm tbllog.cmn tbllog.prm tbltitle.prm \
         x11adj.cmn x11fac.cmn x11ptr.cmn

serates.o:  units.cmn

setamx.o:  error.cmn stdio.i units.cmn

setapt.o:  agr.cmn extend.cmn notset.prm x11ptr.cmn


setcv.o:  notset.prm stdio.i units.cmn

setcvl.o:  notset.prm stdio.i units.cmn

setmdl.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm stdio.i \
         units.cmn

setopr.o:  error.cmn model.prm notset.prm srslen.prm stdio.i units.cmn

setpt.o:  error.cmn mdldat.cmn model.cmn model.prm srslen.prm

setrvp.o:  rev.cmn rev.prm revtrg.cmn srslen.prm

setssp.o:  agr.cmn error.cmn extend.cmn lzero.cmn model.prm notset.prm \
         srslen.prm ssap.cmn ssap.prm sspinp.cmn stdio.i units.cmn \
         x11adj.cmn x11log.cmn x11opt.cmn xrgmdl.cmn

setup.o:  chrt.cmn rho.cmn srslen.prm

setxpt.o:  extend.cmn lzero.cmn x11ptr.cmn

sfmax.o:  srslen.prm

sfmsr.o:  htmlout.cmn srslen.prm units.cmn work2.cmn x11opt.cmn

sftest.o:  error.cmn mdldat.cmn mdldg.cmn model.cmn model.prm notset.prm \
         srslen.prm units.cmn usrreg.cmn usrxrg.cmn

shrink.o:  srslen.prm x11ptr.cmn

si.o:          error.cmn srslen.prm tbllog.cmn tbllog.prm x11msc.cmn x11srs.cmn \
         x11tbl.i xtrm.cmn

sigex.o:  acfst.i across.i bench.i buffers.i cmpflts.i cross.i date.i \
         dimensions.i dirs.i error.cmn estb.i force.cmn func2.i func4.i \
         func5.i hdflag.i hiddn.cmn hspect.i htmlout.cmn models.i notset.prm \
         peaks.i pinno.i preadtr.i rtestm.i seastest.i seatop.cmn serrlev.i \
         sesfcast.i sfcast.i sform.i sig.i sig1.i spe.i spectra.i spectrum.i \
         srslen.prm stdio.i stream.i strmodel.i titl.i transcad.i units.cmn

sigsub.o:  dimensions.i htmlout.cmn revs.i seatop.cmn serrlev.i sesfcast.i \
         sfcast.i srslen.prm stream.i transcad.i

simul.o:  srslen.prm

skparg.o:  lex.i

skparm.o:  lex.i

skpfcn.o:  lex.i

skplst.o:  lex.i

smpeak.o:  notset.prm

spcdrv.o:  adxser.cmn error.cmn extend.cmn hiddn.cmn inpt.cmn mdltbl.i \
         model.cmn model.prm notset.prm orisrs.cmn prior.cmn prior.prm \
         rho.cmn seatcm.cmn seatlg.cmn spcidx.cmn spctbl.i srslen.prm \
         stdio.i tbllog.cmn tbllog.prm tbltitle.prm title.cmn units.cmn \
         x11adj.cmn x11fac.cmn x11log.cmn x11ptr.cmn x11srs.cmn

spcrsd.o:  error.cmn model.prm notset.prm rho.cmn spcidx.cmn srslen.prm \
         stdio.i tbllog.cmn tbllog.prm tbltitle.prm units.cmn

special.o:  htmlout.cmn spectrum.i stream.i

specpeak.o:  dimensions.i rho.cmn spectrum.i srslen.prm

spectrum.o:  buffers.i dimensions.i dirs.i error.cmn func.i func2.i \
         func3.i func4.i func5.i hspect.i htmlout.cmn min.i pinno.i \
         polynom.i seattb.i spectra.i spectrum.i srslen.prm stdio.i \
         stream.i strmodel.i tbllog.cmn tbllog.prm test.i transcad.i \
         units.cmn

spgrh.o:  notset.prm srslen.prm

ss2rv.o:  adj.cmn agr.cmn arima.cmn extend.cmn hiddn.cmn lzero.cmn \
         mdldat.cmn model.cmn model.prm picktd.cmn seatlg.cmn seatmd.cmn \
         srslen.prm ss2rv.cmn ssprep.cmn tbllog.cmn tbllog.prm usrreg.cmn \
         usrxrg.cmn x11adj.cmn x11opt.cmn x11reg.cmn xeastr.cmn xrgmdl.cmn

ssap.o:  dgnsvl.i error.cmn force.cmn mq3.cmn notset.prm srslen.prm \
         ssap.cmn ssap.prm ssptbl.i stdio.i svllog.cmn svllog.prm \
         tbllog.cmn tbllog.prm title.cmn units.cmn x11opt.cmn

ssfnot.o:  htmlout.cmn htmlout.prm notset.prm srslen.prm ssap.cmn ssap.prm \
         sspvec.cmn

ssftst.o:  htmlout.cmn srslen.prm ssap.prm ssft.cmn units.cmn

sshist.o:  error.cmn mq3.cmn notset.prm srslen.prm ssap.cmn ssap.prm \
         units.cmn x11opt.cmn

ssmdl.o:  arima.cmn error.cmn mdldat.cmn model.cmn model.prm otlrev.cmn \
         picktd.cmn srslen.prm sspinp.cmn ssprep.cmn stdio.i units.cmn \
         usrreg.cmn x11adj.cmn

sspdrv.o:  arima.cmn dgnsvl.i error.cmn hiddn.cmn mdldat.cmn mdltbl.i \
         model.cmn model.prm notset.prm otlrev.cmn revtbl.i srslen.prm \
         ssap.cmn ssap.prm sspdat.cmn sspinp.cmn ssptbl.i svllog.cmn \
         svllog.prm tbllog.cmn tbllog.prm title.cmn units.cmn usrreg.cmn \
         usrxrg.cmn x11opt.cmn x11ptr.cmn xrgmdl.cmn xrgtbl.i

ssphdr.o:  force.cmn htmlout.cmn srslen.prm ssap.cmn ssap.prm units.cmn \
         x11opt.cmn

ssprep.o:  arima.cmn mdldat.cmn model.cmn model.prm picktd.cmn prior.cmn \
         prior.prm srslen.prm ssprep.cmn usrreg.cmn x11adj.cmn x11opt.cmn

ssrit.o:  agr.cmn agrsrs.cmn lzero.cmn notset.prm srslen.prm ssap.cmn \
         ssap.prm ssft.cmn sspdat.cmn sspinp.cmn x11opt.cmn

ssrng.o:  htmlout.cmn notset.prm srslen.prm ssap.cmn ssap.prm ssptbl.i \
         tbllog.cmn tbllog.prm units.cmn

ssx11a.o:  arima.cmn error.cmn extend.cmn htmlout.cmn inpt.cmn lzero.cmn \
         mdldat.cmn missng.cmn model.cmn model.prm orisrs.cmn otlrev.cmn \
         otxrev.cmn srslen.prm ssap.cmn ssap.prm ssft.cmn stdio.i units.cmn \
         x11opt.cmn x11ptr.cmn x11reg.cmn xeastr.cmn xrgmdl.cmn

ssxmdl.o:  arima.cmn error.cmn model.cmn model.prm otxrev.cmn srslen.prm \
         sspinp.cmn stdio.i units.cmn usrxrg.cmn x11log.cmn x11reg.cmn \
         xrgmdl.cmn

stpitr.o:  model.prm srslen.prm stdio.i units.cmn

strinx.o:  lex.i

strtvl.o:  mdldat.cmn model.cmn model.prm notset.prm srslen.prm

sumry.o:  goodob.cmn notset.prm srslen.prm x11opt.cmn

svaict.o:  arima.cmn error.cmn htmlout.cmn model.cmn model.prm picktd.cmn \
         srslen.prm units.cmn

svamcm.o:  cchars.i error.cmn mdldat.cmn mdltbl.i model.cmn model.prm \
         savcmn.cmn srslen.prm

svchsd.o:  srslen.prm units.cmn

svf2f3.o:  cmpsvl.i htmlout.cmn inpt2.cmn srslen.prm svllog.cmn svllog.prm \
         tests.cmn work2.cmn x11opt.cmn x11svl.i

svflt.o:  cchars.i error.cmn

svfltd.o:  cchars.i error.cmn

svfnrg.o:  error.cmn model.prm srslen.prm units.cmn

svfreq.o:  notset.prm spcidx.cmn units.cmn

svolit.o:  cchars.i error.cmn mdltbl.i model.prm savcmn.cmn srslen.prm \
         units.cmn xrgtbl.i

svoudg.o:  acfast.i across.i htmlout.cmn units.cmn

svpeak.o:  error.cmn notset.prm units.cmn

svrgcm.o:  cchars.i error.cmn mdldat.cmn mdltbl.i model.cmn model.prm \
         notset.prm savcmn.cmn srslen.prm

svrvhd.o:  mq3.cmn rev.cmn rev.prm revtrg.cmn srslen.prm units.cmn

svspan.o:  cchars.i error.cmn srslen.prm ssap.cmn ssap.prm

table.o:  desfct.prm desfct.var desfc2.prm desfc2.var error.cmn extend.cmn \
         force.cmn goodob.cmn hiddn.cmn htmlout.cmn missng.cmn notset.prm \
         srslen.prm tbltitle.prm tfmts.cmn tfmts.prm tfmts.var title.cmn \
         units.cmn x11opt.cmn x11ptr.cmn xtrm.cmn

tblhdr.o:  error.cmn extend.cmn force.cmn htmlout.cmn mq3.cmn notset.prm \
         priusr.cmn srslen.prm title.cmn units.cmn x11adj.cmn x11msc.cmn \
         x11opt.cmn x11reg.cmn

td6var.o:  error.cmn model.cmn model.prm srslen.prm

td7var.o:  model.prm srslen.prm

tdaic.o:  adj.cmn arima.cmn error.cmn extend.cmn htmlout.cmn inpt.cmn \
         lkhd.cmn mdldat.cmn model.cmn model.prm notset.prm picktd.cmn \
         priadj.cmn prior.cmn prior.prm priusr.cmn srslen.prm units.cmn

tdftest.o:  error.cmn mdldat.cmn mdldg.cmn model.cmn model.prm notset.prm \
         picktd.cmn srslen.prm units.cmn usrreg.cmn usrxrg.cmn

tdlom.o:  adj.cmn priadj.cmn prior.cmn prior.prm priusr.cmn srslen.prm

tdset.o:  notset.prm srslen.prm

tdxtrm.o:  notset.prm srslen.prm tbllog.cmn tbllog.prm x11ptr.cmn \
         xclude.cmn

templs.o:  error.cmn htmlout.cmn htmlout.prm lex.i mdldat.cmn model.cmn \
         model.prm srslen.prm units.cmn

tfmts.o:  error.cmn srslen.prm stdio.i tfmts.cmn tfmts.prm tfmts.var \
         units.cmn

tfmts3.o:  error.cmn

totals.o:  notset.prm

trbias.o:  srslen.prm

trnaic.o:  adj.cmn arima.cmn error.cmn extend.cmn hiddn.cmn htmlout.cmn \
         inpt.cmn lkhd.cmn mdldat.cmn mdlsvl.i model.cmn model.prm mq3.cmn \
         notset.prm picktd.cmn prior.cmn prior.prm priusr.cmn srslen.prm \
         stdio.i svllog.cmn svllog.prm title.cmn units.cmn x11adj.cmn \
         x11fac.cmn x11opt.cmn x11ptr.cmn x11srs.cmn

trnfcn.o:  stdio.i units.cmn

tstdrv.o:  mdldat.cmn model.cmn model.prm notset.prm srslen.prm

tstmd1.o:  adj.cmn arima.cmn error.cmn extend.cmn htmlout.cmn inpt.cmn \
         mdldat.cmn model.cmn model.prm notset.prm picktd.cmn prior.cmn \
         prior.prm priusr.cmn srslen.prm units.cmn

tstmd2.o:  arima.cmn error.cmn mdldat.cmn model.cmn model.prm notset.prm \
         srslen.prm

ttest.o:  model.prm srslen.prm

upespm.o:  mdldat.cmn model.cmn model.prm srslen.prm

usraic.o:  adj.cmn arima.cmn error.cmn extend.cmn htmlout.cmn lkhd.cmn \
         mdldat.cmn model.cmn model.prm notset.prm prior.cmn prior.prm \
         srslen.prm units.cmn usrreg.cmn

value.o:  chrt.cmn srslen.prm

varlog.o:  goodob.cmn notset.prm srslen.prm

vsfa.o:  srslen.prm x11msc.cmn x11opt.cmn

vsfb.o:  srslen.prm x11msc.cmn x11opt.cmn

vsfc.o:  srslen.prm

vtc.o:  srslen.prm x11opt.cmn x11ptr.cmn

vtest.o:  srslen.prm x11opt.cmn

weight.o:  srslen.prm

whitsp.o:  cchars.i lex.i

writln.o:  units.cmn

wrtdat.o:  error.cmn model.prm srslen.prm

wrtmss.o:  notset.prm srslen.prm ssap.cmn ssap.prm units.cmn

wrtotl.o:  error.cmn stdio.i units.cmn

wrttb2.o:  error.cmn htmlout.cmn htmlout.prm notset.prm srslen.prm

wrttbl.o:  error.cmn notset.prm srslen.prm

x11aic.o:  arima.cmn error.cmn extend.cmn htmlout.cmn mdldat.cmn model.cmn \
         model.prm notset.prm srslen.prm units.cmn usrreg.cmn usrxrg.cmn \
         x11adj.cmn x11log.cmn x11reg.cmn xclude.cmn xrgmdl.cmn xrgum.cmn

x11ari.o:  agr.cmn arima.cmn error.cmn extend.cmn htmlout.cmn lzero.cmn \
         mdldat.cmn mdltbl.i model.prm nsums.i priusr.cmn rho.cmn spcsvl.i \
         spctbl.i srslen.prm stdio.i svllog.cmn svllog.prm tbllog.cmn \
         tbllog.prm title.cmn units.cmn x11adj.cmn x11log.cmn x11msc.cmn \
         x11opt.cmn

x11int.o:  adj.cmn inpt.cmn srslen.prm x11fac.cmn x11opt.cmn x11srs.cmn \
         xtrm.cmn

x11mdl.o:  adj.cmn arima.cmn desxrg.prm desxrg.var error.cmn extend.cmn \
         hiddn.cmn inpt.cmn mdldat.cmn model.cmn model.prm notset.prm \
         prior.cmn prior.prm rev.cmn rev.prm srslen.prm ssap.cmn ssap.prm \
         sspinp.cmn stdio.i svllog.cmn svllog.prm tbllog.cmn tbllog.prm \
         tbltitle.prm title.cmn units.cmn usrreg.cmn x11adj.cmn x11fac.cmn \
         x11log.cmn x11ptr.cmn x11reg.cmn x11svl.i xrgfct.cmn xrgmdl.cmn \
         xrgtbl.i xrgum.cmn

x11plt.o:  error.cmn hiddn.cmn srslen.prm tbltitle.prm title.cmn units.cmn \
         x11adj.cmn x11log.cmn x11opt.cmn x11tbl.i

x11pt1.o:  adj.cmn agr.cmn arima.cmn cmptbl.i error.cmn extend.cmn \
         hiddn.cmn inpt.cmn mdldat.cmn mdltbl.i missng.cmn model.cmn \
         model.prm notset.prm orisrs.cmn prior.cmn prior.prm priusr.cmn \
         srslen.prm tbllog.cmn tbllog.prm units.cmn x11adj.cmn x11fac.cmn \
         x11log.cmn x11msc.cmn x11opt.cmn x11ptr.cmn x11reg.cmn xrgtbl.i

x11pt2.o:  agr.cmn cmptbl.i error.cmn extend.cmn hiddn.cmn inpt.cmn \
         mdltbl.i orisrs.cmn prior.cmn prior.prm srslen.prm ssap.cmn \
         ssap.prm stdio.i tbllog.cmn tbllog.prm units.cmn x11adj.cmn \
         x11fac.cmn x11log.cmn x11msc.cmn x11opt.cmn x11ptr.cmn x11reg.cmn \
         x11srs.cmn x11tbl.i xrgtbl.i xrgum.cmn xtrm.cmn

x11pt3.o:  adj.cmn adxser.cmn agr.cmn error.cmn extend.cmn force.cmn \
         frctbl.i goodob.cmn hiddn.cmn inpt.cmn notset.prm orisrs.cmn \
         priadj.cmn prior.cmn prior.prm priusr.cmn rev.cmn rev.prm revtbl.i \
         srslen.prm tbllog.cmn tbllog.prm units.cmn x11adj.cmn x11fac.cmn \
         x11log.cmn x11msc.cmn x11opt.cmn x11ptr.cmn x11srs.cmn x11tbl.i \
         xrgum.cmn xtrm.cmn

x11pt4.o:  adj.cmn adxser.cmn agr.cmn agrsrs.cmn cmptbl.i error.cmn \
         force.cmn frctbl.i goodob.cmn hiddn.cmn inpt.cmn inpt2.cmn \
         notset.prm orisrs.cmn prior.cmn prior.prm priusr.cmn srslen.prm \
         stdio.i tbllog.cmn tbllog.prm units.cmn work2.cmn x11adj.cmn \
         x11fac.cmn x11log.cmn x11msc.cmn x11opt.cmn x11ptr.cmn x11srs.cmn \
         x11tbl.i

x11ref.o:  model.prm notset.prm srslen.prm xrgum.cmn

x12hdr.o:  agr.cmn build.prm cmptbl.i error.cmn force.cmn hiddn.cmn htmlout.cmn \
         lex.i mdltbl.i metadata.cmn metadata.prm missng.cmn mq3.cmn notset.prm \
         prior.cmn prior.prm rho.cmn spctbl.i srslen.prm stdio.i tbllog.cmn \
         tbllog.prm title.cmn units.cmn x11adj.cmn x11log.cmn x11msc.cmn \
         x11opt.cmn x11reg.cmn x11tbl.i

x12run.o:  agr.cmn dgnsvl.i hiddn.cmn htmlout.cmn lex.i notset.prm srslen.prm \
         stdio.i svllog.cmn svllog.prm title.cmn units.cmn x11opt.cmn

xchng.o:  notset.prm srslen.prm ssap.prm

xrgdiv.o:  arima.cmn mdldat.cmn model.cmn model.prm srslen.prm usrreg.cmn

xrgdrv.o:  arima.cmn error.cmn extend.cmn hiddn.cmn inpt.cmn lzero.cmn \
         mdldat.cmn model.cmn model.prm picktd.cmn priadj.cmn prior.cmn \
         prior.prm priusr.cmn srslen.prm units.cmn usrreg.cmn x11adj.cmn \
         x11opt.cmn x11ptr.cmn x11reg.cmn x11srs.cmn xrgmdl.cmn

xrghol.o:  arima.cmn mdldat.cmn model.cmn model.prm srslen.prm usrreg.cmn

xrgtrn.o:  srslen.prm

xrlkhd.o:  mdldat.cmn model.cmn model.prm notset.prm srslen.prm

xtrm.o:  lzero.cmn notset.prm srslen.prm x11opt.cmn xtrm.cmn

yrly.o:  chrt.cmn srslen.prm

component.o: component.i

complagdiag.o: srslen.prm

compcrodiag.o: srslen.prm

phasegain.o: notset.prm

altundovrtst.o: acfast.i across.i htmlout.cmn models.i stream.i

rvarma.o: error.cmn mdldat.cmn model.cmn model.prm rev.prm srslen.prm

rvtdrg.o: error.cmn mdldat.cmn model.cmn model.prm picktd.cmn rev.prm \
         srslen.prm usrreg.cmn

prtukp.o: arima.cmn error.cmn htmlout.cmn htmlout.prm model.prm rho.cmn \
         spctbl.i srslen.prm title.cmn tukey.cmn

svtukp.o: notset.prm spctbl.i tukey.cmn units.cmn

savtpk.o: htmlout.cmn spcsvl.i svllog.prm svllog.cmn units.cmn

m2q.o: srslen.prm

chqsea.o: adxser.cmn arima.cmn htmlout.cmn inpt.cmn mdldat.cmn model.cmn \
         model.prm notset.prm orisrs.cmn rho.cmn seatcm.cmn seatlg.cmn \
         srslen.prm units.cmn x11adj.cmn x11fac.cmn x11msc.cmn x11opt.cmn \
         x11ptr.cmn x11srs.cmn

npsa.o: dimensions.i srslen.prm

gennpsa.o: adxser.cmn extend.cmn model.prm model.cmn notset.prm seatcm.cmn \
           seatlg.cmn tbllog.cmn tbllog.prm srslen.prm rho.cmn \
           units.cmn x11adj.cmn x11ptr.cmn x11srs.cmn

prarma.o: mdldat.cmn model.prm model.cmn srslen.prm

testodf.o: arima.cmn error.cmn extend.cmn mdldat.cmn mdldg.cmn mdlsvl.i \
           mdltbl.i model.cmn model.prm notset.prm prior.cmn prior.prm \
           srslen.prm stdio.i svllog.cmn svllog.prm tbllog.cmn tbllog.prm \
           units.cmn
