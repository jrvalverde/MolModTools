#
#   This is the makefile for GNU80 the portable version of Gaussian80
#
#   Note that source is literary programmed using FWEB and FORTRAN77.
#
#   When using FWEB, the following options should be set in you ~/.fweb
#   file or the command line:
#
#   +TD
#   +PL
#   +wfwebmac.sty
#   +ykw1000
#   +i
#   +ybs2000
#   +Tv
#   +#
#
#   If not, the uncomment the conversion lines in the production rules.
#   If the last statement sounds like martian to you, seek assistance.
#
#   (C) Jose R. Valverde, 2009.
#
#   $

FC=/opt/gcc2/bin/g77
FFLAGS=-g
TANGLE=ftangle

.web.o:
	$(TANGLE) $<
	mv `basename $*.f` `dirname $<`
	cat $*.f | sed -e 's/C\* 1: \*//g' > $*.F
	#mv $*.F $*.f
	$(FC) -c $(FFLAGS) $*.f

.web.f:
	$(TANGLE) $<
	mv `basename $*.f` `dirname $<`
	#cat $*.f | sed -e 's/C\* 1: \*//g' > $*.F
	#mv $*.F $*.f

.f.o:
	$(FC) -c $(FFLAGS) $<

basis =   src/basis/d6311.o  src/basis/d95v.o  src/basis/dn21g.o  src/basis/fc6311.o  src/basis/la1bas.o  src/basis/la2nd.o  src/basis/la3rd.o  src/basis/la4th.o  src/basis/la5th.o  src/basis/los2nd.o  src/basis/los3rd.o  src/basis/los4th.o  src/basis/los5th.o  src/basis/lpeone.o  src/basis/lpetwo.o  src/basis/minbas.o  src/basis/n21g.o  src/basis/neon.o  src/basis/s1s.o  src/basis/s2p.o  src/basis/s2sp.o  src/basis/s2s.o  src/basis/s3d.o  src/basis/s3p.o  src/basis/s3spd.o  src/basis/s3sp.o  src/basis/s3s.o  src/basis/s4d.o  src/basis/s4spd.o  src/basis/s4sp.o  src/basis/s5sp.o  src/basis/sto.o

blocks =   src/blocks/uu0001.o  src/blocks/uu0002.o  src/blocks/uu0003.o  src/blocks/uu0301.o  src/blocks/uu0302.o  src/blocks/uu0303.o  src/blocks/uu0310.o  src/blocks/uu0311.o  src/blocks/uu0312.o  src/blocks/uu0314.o  src/blocks/uu0401.o  src/blocks/uu0501.o  src/blocks/uu0502.o  src/blocks/uu0503.o  src/blocks/uu0505.o  src/blocks/uu0601.o  src/blocks/uu0701.o  src/blocks/uu0702.o  src/blocks/uu0703.o  src/blocks/uu0716.o  src/blocks/uu0801.o  src/blocks/uu0802.o  src/blocks/uu0803.o  src/blocks/uu0901.o  src/blocks/uu0909.o  src/blocks/uu0913.o  src/blocks/uu0utl.o  src/blocks/uucids.o  src/blocks/uugpar.o  src/blocks/uuilsw.o  src/blocks/uuints.o  src/blocks/uuntrn.o  src/blocks/uuot2e.o  src/blocks/uuqpar.o  src/blocks/uustv.o  src/blocks/uut2.o  src/blocks/uuw2.o

l001 =   src/l001/arcset.o  src/l001/chain.o  src/l001/croute.o  src/l001/dollar.o  src/l001/eroute.o  src/l001/fillrt.o  src/l001/gamgen.o  src/l001/gparse.o  src/l001/inicom.o  src/l001/jqint.o  src/l001/l001.o  src/l001/l0cmnd.o  src/l001/link1.o  src/l001/nextov.o  src/l001/nondf.o  src/l001/phyfil.o  src/l001/prtrte.o  src/l001/qpambi.o  src/l001/qpany1.o  src/l001/qparse.o  src/l001/qpcstr.o  src/l001/qpdcod.o  src/l001/qpdest.o  src/l001/qpdump.o  src/l001/qperro.o  src/l001/qpinit.o  src/l001/qpmatc.o  src/l001/qppop.o  src/l001/qppush.o  src/l001/qpreal.o  src/l001/qpscan.o  src/l001/qpschr.o  src/l001/qpskey.o  src/l001/qpskip.o  src/l001/qptran.o  src/l001/qputit.o  src/l001/rdrout.o  src/l001/rstart.o  src/l001/rtedef.o

l101 =   src/l101/chgmlt.o  src/l101/isubst.o  src/l101/l101.o  src/l101/lsubst.o  src/l101/nosign.o  src/l101/prmout.o  src/l101/rcoord.o  src/l101/rdchg.o  src/l101/rdgeom.o  src/l101/rdtitl.o  src/l101/zcentr.o  src/l101/zget.o  src/l101/zmatch.o  src/l101/zparm.o  src/l101/zsymb.o

l102 =   src/l102/contt.o  src/l102/deriv.o  src/l102/enter.o  src/l102/fpdump.o  src/l102/fperr.o  src/l102/fpexit.o  src/l102/fpmain.o  src/l102/getmat.o  src/l102/initfp.o  src/l102/l102.o  src/l102/rmsfp.o  src/l102/search.o  src/l102/udfp.o  src/l102/value.o

l103 =   src/l103/d2corr.o  src/l103/dxlinr.o  src/l103/dxquad.o  src/l103/estm.o  src/l103/grdopt.o  src/l103/initbs.o  src/l103/l103.o  src/l103/optmz.o  src/l103/savept.o  src/l103/schmdt.o  src/l103/star.o  src/l103/udbs.o

l105 =   src/l105/analfc.o  src/l105/axmxv.o  src/l105/contst.o  src/l105/ident.o  src/l105/initfc.o  src/l105/initms.o  src/l105/l105.o  src/l105/msopt.o  src/l105/msprnt.o  src/l105/nextx.o  src/l105/norm1.o  src/l105/tests.o  src/l105/udms.o  src/l105/vtxv.o

l202 =   src/l202/c2axes.o  src/l202/center.o  src/l202/cirset.o  src/l202/cnaxis.o  src/l202/corprt.o  src/l202/cram.o  src/l202/equiv.o  src/l202/filchg.o  src/l202/fill.o  src/l202/filrep.o  src/l202/findc2.o  src/l202/findcn.o  src/l202/findv.o  src/l202/fixrep.o  src/l202/fwgrp.o  src/l202/geom.o  src/l202/inirep.o  src/l202/invert.o  src/l202/l202a.o  src/l202/l202b.o  src/l202/l202.o  src/l202/labrep.o  src/l202/mul3x3.o  src/l202/noones.o  src/l202/numdof.o  src/l202/num.o  src/l202/octa.o  src/l202/omega.o  src/l202/oper.o  src/l202/orc2v.o  src/l202/orcn.o  src/l202/ord2h.o  src/l202/ordn.o  src/l202/orkey.o  src/l202/ornax.o  src/l202/orplan.o  src/l202/oryz.o  src/l202/outrep.o  src/l202/printc.o  src/l202/ptgrp.o  src/l202/puttbl.o  src/l202/put.o  src/l202/reflct.o  src/l202/repci.o  src/l202/repcnh.o  src/l202/repcnv.o  src/l202/repcn.o  src/l202/repcst.o  src/l202/repcs.o  src/l202/repdnd.o  src/l202/repdnh.o  src/l202/repdn.o  src/l202/repdst.o  src/l202/repoh.o  src/l202/repo.o  src/l202/reptd.o  src/l202/rept.o  src/l202/secmom.o  src/l202/sighcn.o  src/l202/snaxis.o  src/l202/sphere.o  src/l202/sphset.o  src/l202/sssc.o  src/l202/ssseq.o  src/l202/ssso.o  src/l202/ssssig.o  src/l202/stoich.o  src/l202/subvar.o  src/l202/symm.o  src/l202/tetra.o  src/l202/triang.o  src/l202/tstc3.o  src/l202/tstc4.o  src/l202/tstc5.o  src/l202/vert.o  src/l202/zprint.o  src/l202/ztoc.o

l301 =   src/l301/applab.o  src/l301/bas1.o  src/l301/basprt.o  src/l301/bassym.o  src/l301/berror.o  src/l301/bldmap.o  src/l301/bprint.o  src/l301/eone.o  src/l301/ethree.o  src/l301/etwo.o  src/l301/ezero.o  src/l301/gbasis.o  src/l301/getngr.o  src/l301/ginput.o  src/l301/l301.o  src/l301/leeao.o  src/l301/leevao.o  src/l301/losbas.o  src/l301/lospot.o  src/l301/permap.o  src/l301/pinput.o  src/l301/prgbas.o  src/l301/prnpot.o  src/l301/putlbl.o  src/l301/rdpot.o  src/l301/rotcor.o  src/l301/rotmap.o  src/l301/rotmat.o  src/l301/set2e.o  src/l301/setpot.o  src/l301/sfopti.o  src/l301/sfopt.o  src/l301/shldat.o  src/l301/typcon.o

l302 =   src/l302/cmpltc.o  src/l302/formfn.o  src/l302/fpx.o  src/l302/fx.o  src/l302/l302.o  src/l302/ryspol.o  src/l302/rysset.o  src/l302/smults.o  src/l302/stvint.o  src/l302/zpolyr.o  src/l302/zquadr.o

l303 =   src/l303/dipole.o  src/l303/fermi.o  src/l303/l303.o

l305 =   src/l305/l305.o

l306 =   src/l306/ang1.o  src/l306/ang2.o  src/l306/bessv.o  src/l306/bess.o  src/l306/cntrlp.o  src/l306/facab.o  src/l306/fillp.o  src/l306/glpint.o  src/l306/imove.o  src/l306/l306.o  src/l306/ldata.o  src/l306/lpsctr.o  src/l306/pseud1.o  src/l306/pseud2.o  src/l306/ptprep.o  src/l306/qalt.o  src/l306/qasy.o  src/l306/qcomp.o  src/l306/qpasy.o  src/l306/qpow.o  src/l306/qqrad.o  src/l306/quadr.o  src/l306/recur1.o  src/l306/recur2.o  src/l306/recurf.o  src/l306/ztab.o

l310 =   src/l310/f.o  src/l310/genl2e.o  src/l310/getc.o  src/l310/getf.o  src/l310/gpurdf.o  src/l310/l310.o

l311 =   src/l311/elimij.o  src/l311/elimik.o  src/l311/elimkl.o  src/l311/elim.o  src/l311/filmax.o  src/l311/ishdup.o  src/l311/l311.o  src/l311/pinf.o  src/l311/r30001.o  src/l311/r30011.o  src/l311/r30101.o  src/l311/r30111.o  src/l311/r31111.o  src/l311/rot2.o  src/l311/sgeom.o  src/l311/shell.o  src/l311/sinfo.o  src/l311/sp0000.o  src/l311/sp0001.o  src/l311/sp0011.o  src/l311/sp0101.o  src/l311/sp0111.o  src/l311/sp1111.o  src/l311/tq0011.o  src/l311/tq0101.o  src/l311/tq0111.o  src/l311/tq1111.o

l314 =   src/l314/aosumf.o  src/l314/cntprm.o  src/l314/dfcut.o  src/l314/dfout1.o  src/l314/dfout2.o  src/l314/dfout3.o  src/l314/dfout4.o  src/l314/dfout6.o  src/l314/dfout8.o  src/l314/gaoind.o  src/l314/geta2.o  src/l314/getip2.o  src/l314/l314.o  src/l314/lentqf.o  src/l314/mtget.o  src/l314/phoeni.o  src/l314/purdf2.o  src/l314/qinf.o  src/l314/replct.o  src/l314/rpol2.o  src/l314/sitind.o  src/l314/twod2c.o  src/l314/twod3c.o  src/l314/twod4c.o

l401 =   src/l401/altges.o  src/l401/blockh.o  src/l401/cfill.o  src/l401/chkrd.o  src/l401/cmpden.o  src/l401/coefs.o  src/l401/complt.o  src/l401/corges.o  src/l401/densty.o  src/l401/dform.o  src/l401/filrec.o  src/l401/frmprj.o  src/l401/gcfact.o  src/l401/gdot.o  src/l401/geserr.o  src/l401/gesopt.o  src/l401/gesprt.o  src/l401/gesrwf.o  src/l401/getges.o  src/l401/getmo.o  src/l401/gnorm.o  src/l401/guess.o  src/l401/huckel.o  src/l401/hukges.o  src/l401/ind401.o  src/l401/l401.o  src/l401/normch.o  src/l401/ovlp.o  src/l401/prjhuk.o  src/l401/projec.o  src/l401/rdges.o  src/l401/rootmt.o  src/l401/twreig.o

l501 =   src/l501/conclo.o  src/l501/csymm.o  src/l501/cvu.o  src/l501/cycclo.o  src/l501/dir2e.o  src/l501/dirclo.o  src/l501/fofclo.o  src/l501/formf.o  src/l501/frmp.o  src/l501/frmv.o  src/l501/fvmul.o  src/l501/gofr.o  src/l501/incrd.o  src/l501/l501.o  src/l501/levshf.o  src/l501/locmin.o  src/l501/matmul.o  src/l501/raff1c.o  src/l501/rhfclo.o  src/l501/tst503.o  src/l501/uhftst.o  src/l501/vdagt.o

l502 =   src/l502/annil.o  src/l502/conopn.o  src/l502/cycopn.o  src/l502/fofopn.o  src/l502/formv.o  src/l502/l502.o  src/l502/linear.o  src/l502/paann.o  src/l502/rwfspc.o  src/l502/spin.o  src/l502/traopn.o  src/l502/trasq.o  src/l502/uhfopn.o

l503 =   src/l503/asgsym.o  src/l503/aufbau.o  src/l503/bessrt.o  src/l503/carpol.o  src/l503/cmat.o  src/l503/comat.o  src/l503/conuso.o  src/l503/diablo.o  src/l503/dimens.o  src/l503/first.o  src/l503/l503.o  src/l503/ofix.o  src/l503/polcar.o  src/l503/scfdm.o  src/l503/spindm.o  src/l503/tracab.o

l505 =   src/l505/conphf.o  src/l505/cycphf.o  src/l505/eigen.o  src/l505/fofphf.o  src/l505/l505.o  src/l505/matrc3.o  src/l505/phfchk.o  src/l505/phfprt.o  src/l505/rhfopn.o  src/l505/traphf.o

l601 =   src/l601/atompr.o  src/l601/dq.o  src/l601/ecnfig.o  src/l601/frmpop.o  src/l601/l601.o  src/l601/mulpop.o  src/l601/orbtyp.o  src/l601/rgdvo.o  src/l601/symlbl.o

l602 =   src/l602/l602.o  src/l602/savscf.o  src/l602/utida.o  src/l602/utiti.o

l701 =   src/l701/d1e.o  src/l701/dstvnt.o  src/l701/l701.o

l702 =   src/l702/d2esp.o  src/l702/efill.o  src/l702/fpppp.o  src/l702/l702.o  src/l702/twldrv.o

l703 =   src/l703/d2espd.o  src/l703/dipabc.o  src/l703/dipab.o  src/l703/dipac.o  src/l703/dipa.o  src/l703/dphnix.o  src/l703/efill1.o  src/l703/l703.o

l705 =   src/l705/cntlpd.o  src/l705/dlpint.o  src/l705/l705.o

l716 =   src/l716/bend.o  src/l716/dclose.o  src/l716/dopen.o  src/l716/drvsrd.o  src/l716/fcorpr.o  src/l716/ffrcnn.o  src/l716/formbg.o  src/l716/frcnn.o  src/l716/frcout.o  src/l716/fzprnt.o  src/l716/ibout.o  src/l716/l716.o  src/l716/minv.o  src/l716/mofi.o  src/l716/norout.o  src/l716/putff.o  src/l716/putf.o  src/l716/rmsvec.o  src/l716/rotff.o  src/l716/rotf.o  src/l716/str.o  src/l716/symnum.o  src/l716/thermo.o  src/l716/tors.o  src/l716/tranff.o  src/l716/tranf.o  src/l716/tstcor.o  src/l716/vibfrq.o  src/l716/vibovl.o  src/l716/vibsym.o  src/l716/vibtbl.o  src/l716/zmmod.o

l801 =   src/l801/ciprm.o  src/l801/l801.o  src/l801/window.o  src/l801/writed.o  src/l801/writes.o

l802 =   src/l802/dirtrn.o  src/l802/l802.o  src/l802/trcl80.o  src/l802/trclos.o

l803 =   src/l803/l803.o  src/l803/trop80.o  src/l803/tropen.o

l901 =   src/l901/clean.o  src/l901/doubar.o  src/l901/exchn1.o  src/l901/exchn2.o  src/l901/exchn3.o  src/l901/exchn4.o  src/l901/exchn5.o  src/l901/exchn6.o  src/l901/l901.o

l909 =   src/l909/cids1.o  src/l909/l909.o

l910 =   src/l910/cids2.o  src/l910/ds4n5.o  src/l910/ds4n6.o  src/l910/ds4.o  src/l910/l910.o  src/l910/ss3.o

l911 =   src/l911/aatild.o  src/l911/abtild.o  src/l911/cids3.o  src/l911/dd1sd4.o  src/l911/l911.o  src/l911/sd5ds5.o  src/l911/wia4a.o  src/l911/wia4b.o  src/l911/wtwij.o

l912 =   src/l912/aaclos.o  src/l912/cids4.o  src/l912/comijw.o  src/l912/conddf.o  src/l912/ctwc1.o  src/l912/ctwc2.o  src/l912/dd2.o  src/l912/dd3.o  src/l912/exp78.o  src/l912/expaba.o  src/l912/expabs.o  src/l912/expand.o  src/l912/expija.o  src/l912/expijs.o  src/l912/expijw.o  src/l912/expol.o  src/l912/expsym.o  src/l912/inibuc.o  src/l912/l912.o  src/l912/lsexa.o  src/l912/lsexs.o  src/l912/matca1.o  src/l912/matca2.o  src/l912/matcab.o  src/l912/mattrn.o  src/l912/multvc.o  src/l912/normds.o  src/l912/printp.o  src/l912/santab.o  src/l912/santij.o  src/l912/scalp1.o  src/l912/scalp.o  src/l912/sumant.o  src/l912/sumn.o  src/l912/test.o  src/l912/tewa.o  src/l912/tewb.o  src/l912/tews.o  src/l912/timer.o  src/l912/transp.o  src/l912/trsfr.o  src/l912/tstara.o  src/l912/tstarb.o  src/l912/vewa.o  src/l912/vewb.o  src/l912/vews.o  src/l912/wtilda.o  src/l912/wtildb.o  src/l912/wtoada.o  src/l912/wtoadb.o  src/l912/wtoas.o

l913 =   src/l913/cidens.o  src/l913/cids5.o  src/l913/geta.o  src/l913/getde.o  src/l913/l913.o  src/l913/pform.o  src/l913/scanaa.o  src/l913/scanab.o  src/l913/scanss.o  src/l913/scan.o  src/l913/ump4q.o  src/l913/ump4t.o

l9999 =   src/l9999/alldun.o  src/l9999/g80end.o  src/l9999/l9999.o

main =   src/main/gnu80.o  src/main/shift.o

util1 =   src/util1/aclear.o  src/util1/bdump.o  src/util1/binrd.o  src/util1/binwt.o  src/util1/bldtbl.o  src/util1/diagd.o  src/util1/diag.o  src/util1/dismat.o  src/util1/dorot.o  src/util1/drum.o  src/util1/ehbckd.o  src/util1/ehobkd.o  src/util1/ehoudh.o  src/util1/ehousd.o  src/util1/fdump.o  src/util1/fferr.o  src/util1/ffget.o  src/util1/ffread.o  src/util1/ffset.o  src/util1/fileio.o  src/util1/fillc.o  src/util1/matprt.o  src/util1/ntran.o  src/util1/nwait.o  src/util1/out2e.o  src/util1/permut.o  src/util1/qpaint.o  src/util1/qpcpv.o  src/util1/qpdp.o  src/util1/qpskbl.o  src/util1/qptval.o  src/util1/qpword.o  src/util1/symasg.o  src/util1/trnfrm.o  src/util1/trnrow.o

util2 =   src/util2/aadd.o  src/util2/aoloop.o  src/util2/convgd.o  src/util2/corpr1.o  src/util2/cpulft.o  src/util2/cputim.o  src/util2/crop.o  src/util2/dagtyd.o  src/util2/decchr.o  src/util2/defunt.o  src/util2/dgnmap.o  src/util2/digtst.o  src/util2/dint2e.o  src/util2/dintrp.o  src/util2/dout.o  src/util2/encode.o  src/util2/eqrt2d.o  src/util2/fld.o  src/util2/fmtgen.o  src/util2/fmtset.o  src/util2/frmrot.o  src/util2/fsymm.o  src/util2/gabgst.o  src/util2/gabs.o  src/util2/gacos.o  src/util2/gasin.o  src/util2/gatan2.o  src/util2/gatan.o  src/util2/gcos.o  src/util2/getind.o  src/util2/gexp.o  src/util2/gfloat.o  src/util2/glog10.o  src/util2/glog.o  src/util2/gmax1.o  src/util2/gmin1.o  src/util2/gmod.o  src/util2/gsign.o  src/util2/gsin.o  src/util2/gsqrt.o  src/util2/gtan.o  src/util2/hexchr.o  src/util2/iclear.o  src/util2/ifalph.o  src/util2/ijkpri.o  src/util2/ilsw.o  src/util2/intchr.o  src/util2/iord.o  src/util2/ipt123.o  src/util2/itqry.o  src/util2/killer.o  src/util2/lfld.o  src/util2/lnk1e.o  src/util2/lnk1.o  src/util2/locstr.o  src/util2/matpac.o  src/util2/matrec.o  src/util2/move.o  src/util2/nchrpw.o  src/util2/newlnk.o  src/util2/not.o  src/util2/nrep.o  src/util2/numer.o  src/util2/or3mom.o  src/util2/oraxis.o  src/util2/orptst.o  src/util2/pack2.o  src/util2/pack4.o  src/util2/pad.o  src/util2/pakstr.o  src/util2/prmtbl.o  src/util2/prsfwg.o  src/util2/prtsym.o  src/util2/putdel.o  src/util2/putfp.o  src/util2/puti.o  src/util2/putsmt.o  src/util2/putstr.o  src/util2/rawst.o  src/util2/rotate.o  src/util2/rsetcl.o  src/util2/rsetop.o  src/util2/rtrace.o  src/util2/setrys.o  src/util2/shlatm.o  src/util2/skip.o  src/util2/streqc.o  src/util2/streq.o  src/util2/strout.o  src/util2/tform.o  src/util2/timeo.o  src/util2/tquery.o  src/util2/traceb.o  src/util2/tread.o  src/util2/trspn2.o  src/util2/twrite.o  src/util2/unpcck.o  src/util2/unpck2.o  src/util2/unpck4.o  src/util2/vec.o  src/util2/versn.o  src/util2/vprod.o  src/util2/winc.o

util3 =   src/util3/amove.o  src/util3/ascale.o  src/util3/defbuc.o  src/util3/dmpint.o  src/util3/dot.o  src/util3/drvip1.o  src/util3/dsymm.o  src/util3/fillcp.o  src/util3/filmat.o  src/util3/frmw.o  src/util3/get1cs.o  src/util3/get2cs.o  src/util3/get2c.o  src/util3/get3c.o  src/util3/geta1.o  src/util3/getcc1.o  src/util3/getcc2.o  src/util3/getij.o  src/util3/getnb6.o  src/util3/getrep.o  src/util3/herm.o  src/util3/integi.o  src/util3/intprt.o  src/util3/inv.o  src/util3/isymgo.o  src/util3/labint.o  src/util3/labscf.o  src/util3/linout.o  src/util3/ltoutd.o  src/util3/matout.o  src/util3/nprio.o  src/util3/ordoc.o  src/util3/pmat.o  src/util3/purdf1.o  src/util3/redob.o  src/util3/redop.o  src/util3/renorm.o  src/util3/repuls.o  src/util3/rootn.o  src/util3/rpol1.o  src/util3/scftrc.o  src/util3/setord.o  src/util3/setr1.o  src/util3/sls.o  src/util3/square.o  src/util3/tioc.o  src/util3/track.o  src/util3/virial.o

util4 =   src/util4/captlz.o  src/util4/deltyc.o  src/util4/deltyp.o  src/util4/getbc.o  src/util4/getb.o  src/util4/getchr.o  src/util4/getch.o  src/util4/getlcu.o  src/util4/ilord.o  src/util4/lord.o  src/util4/putbc.o  src/util4/putb.o  src/util4/puticr.o  src/util4/putput.o  src/util4/szprnt.o

src =   NBO/src/angles.o  NBO/src/anlyze.o  NBO/src/aout.o  NBO/src/aprint.o  NBO/src/aread.o  NBO/src/atdiag.o  NBO/src/augmnt.o  NBO/src/awrite.o  NBO/src/bdfind.o  NBO/src/charpn.o  NBO/src/chem.o  NBO/src/choose.o  NBO/src/chsdrv.o  NBO/src/chsinp.o  NBO/src/consol.o  NBO/src/convin.o  NBO/src/convrt.o  NBO/src/copy.o  NBO/src/core.o  NBO/src/corinp.o  NBO/src/cortbl.o  NBO/src/cycles.o  NBO/src/debyte.o  NBO/src/delete.o  NBO/src/delinp.o  NBO/src/delscf.o  NBO/src/deplet.o  NBO/src/dfgorb.o  NBO/src/dipanl.o  NBO/src/dipele.o  NBO/src/dipnuc.o  NBO/src/dlcstr.o  NBO/src/dmnao.o  NBO/src/dmsim.o  NBO/src/equal.o  NBO/src/factor.o  NBO/src/feaoin.o  NBO/src/feaomo.o  NBO/src/febas.o  NBO/src/fecoor.o  NBO/src/fednao.o  NBO/src/fedraw.o  NBO/src/fedxyz.o  NBO/src/fee0.o  NBO/src/fefao.o  NBO/src/fefnbo.o  NBO/src/feinfo.o  NBO/src/fenbo.o  NBO/src/fenewd.o  NBO/src/fenlmo.o  NBO/src/fepnao.o  NBO/src/feppao.o  NBO/src/fesnao.o  NBO/src/fesraw.o  NBO/src/fetitl.o  NBO/src/fetlmo.o  NBO/src/fetnab.o  NBO/src/fetnao.o  NBO/src/fetnbo.o  NBO/src/fetnho.o  NBO/src/fnboan.o  NBO/src/fndfld.o  NBO/src/fndmol.o  NBO/src/fndsol.o  NBO/src/formt.o  NBO/src/frmhyb.o  NBO/src/frmpro.o  NBO/src/frmtmo.o  NBO/src/geninp.o  NBO/src/getdel.o  NBO/src/halt.o  NBO/src/hfld.o  NBO/src/htype.o  NBO/src/hybcmp.o  NBO/src/hybdir.o  NBO/src/idigit.o  NBO/src/ifld.o  NBO/src/ihtyp.o  NBO/src/ioinqr.o  NBO/src/iwprj.o  NBO/src/jacobi.o  NBO/src/keypar.o  NBO/src/lblao.o  NBO/src/lblnao.o  NBO/src/lblnbo.o  NBO/src/lblnho.o  NBO/src/limtrn.o  NBO/src/lineq.o  NBO/src/lmoanl.o  NBO/src/loadav.o  NBO/src/load.o  NBO/src/matml2.o  NBO/src/matmlt.o  NBO/src/mulana.o  NBO/src/nameat.o  NBO/src/naoanl.o  NBO/src/naodrv.o  NBO/src/naosim.o  NBO/src/nao.o  NBO/src/nathyb.o  NBO/src/nbclos.o  NBO/src/nbinqr.o  NBO/src/nbocla.o  NBO/src/nbodel.o  NBO/src/nbodim.o  NBO/src/nbodrv.o  NBO/src/nboean.o  NBO/src/nboinp.o  NBO/src/nbopen.o  NBO/src/nboset.o  NBO/src/nbosum.o  NBO/src/nbo.o  NBO/src/nbread.o  NBO/src/nbwrit.o  NBO/src/newdm.o  NBO/src/newryd.o  NBO/src/newwts.o  NBO/src/nlmo.o  NBO/src/normlz.o  NBO/src/orderr.o  NBO/src/orthyb.o  NBO/src/output.o  NBO/src/pack.o  NBO/src/prjexp.o  NBO/src/rank.o  NBO/src/rdcard.o  NBO/src/rdcore.o  NBO/src/rdppna.o  NBO/src/rdtnab.o  NBO/src/rdtnao.o  NBO/src/redblk.o  NBO/src/rediag.o  NBO/src/repol.o  NBO/src/rfld.o  NBO/src/rnkeig.o  NBO/src/runnbo.o  NBO/src/rydiag.o  NBO/src/rydsel.o  NBO/src/setbas.o  NBO/src/shmdt.o  NBO/src/simltr.o  NBO/src/simtrm.o  NBO/src/simtrn.o  NBO/src/simtrs.o  NBO/src/srtnbo.o  NBO/src/stash.o  NBO/src/strtin.o  NBO/src/subst.o  NBO/src/svdnao.o  NBO/src/sve0.o  NBO/src/svfnbo.o  NBO/src/svnbo.o  NBO/src/svnewd.o  NBO/src/svnlmo.o  NBO/src/svpnao.o  NBO/src/svppao.o  NBO/src/svsnao.o  NBO/src/svtlmo.o  NBO/src/svtnab.o  NBO/src/svtnao.o  NBO/src/svtnho.o  NBO/src/symort.o  NBO/src/symuni.o  NBO/src/trnspo.o  NBO/src/unpack.o  NBO/src/valtbl.o  NBO/src/veclen.o  NBO/src/worth.o  NBO/src/wrarc.o  NBO/src/wrbas.o  NBO/src/wrnlmo.o  NBO/src/wrppna.o  NBO/src/wrtnab.o  NBO/src/wrtnao.o  NBO/src/wrtnbo.o  NBO/src/xcited.o

EVERYTHING =  $(basis) $(blocks) $(l001) $(l101) $(l102) $(l103) $(l105) $(l202) $(l301) $(l302) $(l303) $(l305) $(l306) $(l310) $(l311) $(l314) $(l401) $(l501) $(l502) $(l503) $(l505) $(l601) $(l602) $(l701) $(l702) $(l703) $(l705) $(l716) $(l801) $(l802) $(l803) $(l901) $(l909) $(l910) $(l911) $(l912) $(l913) $(l9999) $(main) $(util1) $(util2) $(util3) $(util4) $(src)

all : $(EVERYTHING) NBO/jobopt.o
	$(FC) $(FFLAGS) -c NBO/jobopt.f
	$(FC) -o g80 $(FFLAGS) $(EVERYTHING) NBO/jobopt.o

clean:
	\rm -f src/*/*.[ofF] NBO/src/*.[ofF] NBO/jobopt.o
