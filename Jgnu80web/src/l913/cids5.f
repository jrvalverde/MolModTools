
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cids5"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cids5.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 36 "cids5.web"
      subroutine cids5(JUMP)
      implicit none
      double precision A00,A0old,Anorm,anorm2,aswitc,Atmchg,C,ccdl,ccdl4
     &,ccdl41,ccdl42,Ccdlin,Cmo,costht,Cuts,Dehf,Delmax,Den,den2w,Dep
      double precision det23,detv2,devar1,devar2,dlccd,dqmp4,dscc,Dv,dva
     &r,E,e2,e2me3,e2pe3,e2t,e3,ec,eccdl4,eccl41,ecisd4,ecorr
      double precision edqmp,Ee,el,Elccd,ener,Energy,enp2,equad4,escc,es
     &dqmp,esdtq,etrip4,evar1,evard,ew1,F42,Filmoc,Four,gabs,gacos
      double precision gcos,gfloat,gsqrt,gtan,Half,One,Onept5,pt5,Qep,ro
     &ot,sdqmp4,sdtqmp,sec2t,small,t,tan2t,Ten,tewa,tewb,tews
      double precision theta2,Three,Two,V,W0,W0save,w1,Zero
      integer i,Iad1,Iad2,Iad3,Ian,Iapr,Ias1,Ias2,Ibckt,ibskp,Icharg,Ici
     &var,Idb1,Idb10,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7
      integer Idb8,Idb9,idens,Idimmy,Idmm,Idummy,Idump,Ieval,Iextrp,Ifla
     &g,Igen,Igeno,Ilincc,im1,im2,In,ind,Inforb,inhexp,Inr
      integer Ioab,Iop,Iopcl,Iout,ipairp,Ipcyc,Iprint,Ipunch,iscan,ische
     &m,Iscr1,Iscr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscr7,Iscr8,Iscr9,Isd
      integer Ispect,iupdat,Iwd1,Iwd2,Iwd3,Iws1,Iws2,j,JUMP,k,l,La0,Lano
     &rm,Lccd,Lcisd4,Lcivar,Ldq4,Lecid,Lecids,Lehf
      integer Lenrgy,Lextrp,Ligen,Lisd,Llccd,Llccd4,Llincc,Lmp2,Lmp3,Lnf
     &orb,Loab,Lscc1,Lsdq4,Lsdtq4,Lspect,Lvar1,Maxbuc,Maxit,Mdv,mdv2
      integer method,Multip,Nae,Natoms,nb1,nb2,Nbasis,Nbe,Ne,Niter,Noa,N
     &oa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobuc
      integer nobuc1,nobuc9,Nobva,Nobvb,Norm,norms,Novaa,Novab,Novbb,np2
     &,Nrorb,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      logical Davail,Savail
      dimension Ibckt(1),Dv(1),E(1),Igen(1)
      dimension ec(2)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/v/V(20000),Mdv
      common/rwfp/Igeno,Ligen,Inforb,Lnforb,Icivar,Lcivar,Iextrp,Lextrp
      common/loclin/Ilincc,Llincc
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      common/extrap/Dep(2),Qep,A0old,Inr,Idummy
      common/moc/Cmo(300),Ee(175),Filmoc(12325)
      common/nobuc/Nobuc
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscr7,Iscr8,Iscr9,Iapr(10)
      common/locgen/Lehf,Lmp2,Lmp3,Ldq4,Lsdq4,Lsdtq4,Lecid,Lecids,Lscc1,
     &Lccd,Llccd,Lvar1,Lcisd4,Llccd4,Lenrgy,Lanorm,La0,Lisd
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/constr/Iopcl
      common/io/In,Iout,Ipunch
      common/print/Iprint
      common/dump/Idump,Idimmy
      common/lccd/Ccdlin(50),W0save,Elccd
      equivalence(Ibckt(1),Idb1)
      equivalence(Dv(1),Igen(1),Cmo(1)),(E(1),Ee(1))
      data ibskp/-103/
      data aswitc/0.1D0/
      data small/1.D-8/
      
      
      
      
      
      
      
      
      
      
      
99001 format(' MP4(R+Q)=',d16.8)
99002 format(' MP4(T)=',d18.8)
99003 format(' E3=',d22.8,8x,'EUMP3=',d24.11)
99004 format('+',39x,d24.11)
99005 format(' E4(DQ)=',d18.8,8x,'UMP4(DQ)=',d21.11)
99006 format('+',43x,d21.11)
99007 format(' E4(SDQ)=',d17.8,8x,'UMP4(SDQ)=',d20.11)
99008 format('+',43x,d20.11)
99009 format(' E4(SDTQ)=',d16.8,8x,'UMP4(SDTQ)=',d19.11)
99010 format('+',44x,d19.11)
99011 format(' VARIATIONAL ENERGIES WITH THE FIRST-ORDER WAVEFUNCTION:')
99012 format(' E(VAR1)=',d20.11,5x,'E(CID,4)=',d21.11)
99013 format(' E(VAR1)=',d20.11,5x,'E(CISD,4)=',d20.11)
99014 format(' NORM(A)=',d17.8)
99015 format(' A(0)=',d20.8)
99016 format(' DE(CORR)',d17.8,8x,'E(CORR)=',d22.11)
99017 format(' DE(CI)=',d18.8,8x,'E(CI)=',d24.11)
99018 format(' SIZE-CONSISTENCY CORRECTION: ')
99019 format(' S.C.C.=',d18.8,8x,'E(CI,SIZE)=',d19.11)
99020 format(' E(CCD.LIN,',i2,')= ',d17.10,' CORR.ENERGY=',d17.10,' TOTA
     &L ENERGY= ',d17.10)
99021 format(' E(CCD.LIN,',i2,')=',d25.10)
99022 format(1x,13(1H*)/1x,'*MAX. CYCLES*'/1x,13(1H*))
99023 format(1x,63(1H*))
      
      
      method=Iop(5)
      iupdat=Iop(7)
      ischem=Iop(18)+1
      inhexp=Iop(19)
      ipairp=Iop(25)
      norms=Iop(26)
      iscan=Iop(28)
      idens=Iop(30)
      Iprint=Iop(33)
      Idump=Iop(34)
      
      call tread(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      call tread(Icivar,A00,Lcivar,1,Lcivar,1,0)
      call tread(Iextrp,Dep,Lextrp,1,Lextrp,1,0)
      
      mdv2=Mdv/2
      
      call ilsw(2,1,Iopcl)
      
      Davail=.TRUE.
      if(method.EQ.0.OR.method.EQ.4.OR.method.EQ.5)Savail=.TRUE.
      
      do 100 i=1,Nobuc
      Ibckt(i)=i
100   continue
      
      nobuc1=Nobuc+1
      nobuc9=Nobuc+19
      do 200 i=nobuc1,nobuc9
      Ibckt(i)=3000+i
200   continue
      
      if(Iopcl.EQ.0)then
      Ias2=Ias1
      Iws2=Iws1
      Iad3=Iad1
      Iwd3=Iwd1
      Idb3=Idb1
      endif
      
      nb1=Nrorb+1
      nb2=2*Nrorb
      call tread(Ieval,E,nb2,1,nb2,1,0)
      
      anorm2=Anorm**2
      den2w=Den+Two*A00*W0
      if(Iopcl.NE.0)then
      
      dvar=den2w+tewa(Iad1,E,Noa,Nva,1)+tewb(Iad2,E,E(nb1),1)+tewa(Iad3,
     &E(nb1),Nob,Nvb,1)+tews(Ias1,E,Noa,Nva,1)+tews(Ias2,E(nb1),Nob,Nvb,
     &1)
      dvar=dvar/anorm2
      ccdl41=-(tewa(Iwd1,E,Noa,Nva,2)+tewb(Iwd2,E,E(nb1),2)+tewa(Iwd3,E(
     &nb1),Nob,Nvb,2))
      ccdl42=-(tews(Iws1,E,Noa,Nva,2)+tews(Iws2,E(nb1),Nob,Nvb,2))
      ccdl4=ccdl41+ccdl42
      else
      
      dvar=den2w+Two*(+tewa(Iad1,E,Noa,Nva,1)+tews(Ias1,E,Noa,Nva,1))+te
     &wb(Iad2,E,E,1)
      dvar=dvar/anorm2
      ccdl41=-Two*(tewa(Iwd1,E,Noa,Nva,2))-tewb(Iwd2,E,E(nb1),2)
      ccdl42=-Two*(tews(Iws1,E,Noa,Nva,2))
      ccdl4=ccdl41+ccdl42
      endif
      
      if(method.GT.2)then
      
      call fileio(6,0,0,0,0)
      call ump4q(equad4)
      if(Niter.EQ.1)write(Iout,99001)equad4
      if(method.EQ.5)call ump4t(etrip4)
      if(method.EQ.5)write(Iout,99002)etrip4
      endif
      
      call geta(E,method,ischem)
      
      call normds
      
      w1=Zero
      if(gabs(A00).GT.1.D-4)w1=W0/A00
      ew1=Dehf+(w1)
      evard=Dehf+(dvar)
      dscc=Zero
      escc=Zero
      if(method.LE.1.AND.dabs(A00).GE.aswitc)then
      costht=A00/Anorm
      theta2=Two*gacos(costht)
      tan2t=gtan(theta2)
      sec2t=One/gcos(theta2)
      el=gfloat(Noa+Nob)
      root=gsqrt(el**2+Two*el*tan2t**2)
      if(sec2t.LT.Zero)root=-root
      if(gabs(sec2t-One).GT.small)then
      escc=dvar*(root-el)/(Two*(sec2t-One))
      dscc=(escc-(dvar))
      endif
      endif
      escc=escc+Dehf
      if(inhexp.NE.0)Inr=0
      
      
      if(ipairp.EQ.2)call printp(Nae)
      if(ipairp.EQ.3.AND.Niter.EQ.Ipcyc)call printp(Nae)
      if(ipairp.EQ.4.AND.Niter.EQ.1)call printp(Nae)
      ccdl=ccdl4
      
      call tread(Igeno,Dv,Ligen,1,Ligen,1,0)
      
      if(Niter.LE.1)then
      e2=(Dv(Lmp2)-Dehf)
      t=(Dv(Lanorm))
      t=t*t
      e2t=e2*(t-One)
      e3=Den
      if(Norm.GT.0)e3=Den*(t)
      if(Norm.GT.0)ccdl4=ccdl4*(t)
      if(Norm.GT.0)equad4=equad4*(t)
      e2pe3=e2+e3
      e2me3=e2-e3
      if(gabs(t-One).GT.small.AND.dabs(e2me3).GT.small)devar2=pt5*e2me3*
     &(gsqrt(One+Four*t*(e2/e2me3)**2)-One)/(t-One)
      det23=Dehf+(e2pe3)
      detv2=Dehf+(devar2)
      devar1=e2pe3/(t)
      evar1=Dehf+(devar1)
      eccdl4=det23+(ccdl4)
      ecisd4=eccdl4-(e2t)
      eccl41=det23+(ccdl41)
      dqmp4=eccl41+equad4
      sdqmp4=dqmp4+ccdl42
      sdtqmp=sdqmp4+etrip4
      edqmp=dqmp4-det23
      esdqmp=sdqmp4-det23
      esdtq=esdqmp+etrip4
      Dv(Lmp3)=det23
      Dv(Lvar1)=evar1
      Dv(Llccd4)=eccdl4
      Dv(Lcisd4)=ecisd4
      if(method.GT.2)Dv(Ldq4)=dqmp4
      if(method.EQ.4.OR.method.EQ.5)Dv(Lsdq4)=sdqmp4
      if(method.EQ.5)Dv(Lsdtq4)=sdtqmp
      if(iupdat.EQ.1)Dv(Lenrgy)=det23
      if(iupdat.EQ.2)Dv(Lenrgy)=sdqmp4
      if(iupdat.EQ.0.AND.method.EQ.2)Dv(Lenrgy)=det23
      if(iupdat.EQ.0.AND.method.EQ.3)Dv(Lenrgy)=dqmp4
      if(iupdat.EQ.0.AND.method.EQ.4)Dv(Lenrgy)=sdqmp4
      if(iupdat.EQ.0.AND.method.EQ.5)Dv(Lenrgy)=sdtqmp
      
      W0save=e2
      Elccd=e2
      
      write(Iout,99003)e3,det23
      if(method.GT.2)write(Iout,99005)edqmp,dqmp4
      if(method.EQ.4.OR.method.EQ.5)write(Iout,99007)esdqmp,sdqmp4
      if(method.EQ.5)write(Iout,99009)esdtq,sdtqmp
      if(method.GE.2)write(Iout,99011)
      if(method.EQ.4.OR.method.EQ.5)write(Iout,99013)evar1,ecisd4
      if(method.EQ.2.OR.method.EQ.3.OR.method.EQ.6)write(Iout,99012)evar
     &1,ecisd4
      endif
      
      if(method.EQ.6)write(Iout,99016)w1,ew1
      if(method.LE.1)write(Iout,99017)dvar,evard
      if(method.LE.1.OR.method.EQ.6)write(Iout,99014)Anorm
      if(method.LE.1)write(Iout,99018)
      if(method.LE.1)write(Iout,99019)dscc,escc
      if(method.LE.1.AND.Norm.GT.1)write(Iout,99015)A00
      
      if(ischem.EQ.3.AND.method.LT.2)then
      
      if(Niter.GT.1)call tread(Ilincc,Ccdlin,Llincc,1,Llincc,1,0)
      enp2=w1-W0save
      W0save=w1
      np2=Niter+2
      if(Iprint.GT.1)write(Iout,99021)np2,enp2
      ec(1)=Den
      ec(2)=ccdl
      i=Niter
      im1=i-1
      im2=i-2
      
      do 250 k=1,2
      ener=Zero
      ind=2*i+k
      if(im1.GT.0)then
      do 210 l=1,i
      ener=ener+Ccdlin(l+k+1)*gfloat(l)
210   continue
      if(im2.GT.0)then
      do 215 l=1,im2
      j=i-l
      ener=ener+Ccdlin(i+l+k+1)*gfloat(j)
215   continue
      endif
      endif
      Ccdlin(ind)=ec(k)-ener
      Elccd=Elccd+Ccdlin(ind)
      dlccd=Dehf+(Elccd)
      write(Iout,99020)ind,Ccdlin(ind),Elccd,dlccd
250   continue
      
      Dv(Llccd)=dlccd
      call twrite(Ilincc,Ccdlin,Llincc,1,Llincc,1,0)
      endif
      
      
      ecorr=dvar
      if(method.GT.1)ecorr=w1
      if(ischem.GE.2)ecorr=w1
      if(ischem.EQ.3.AND.method.LE.1)ecorr=Elccd
      
      Dv(Lscc1)=escc
      if(method.EQ.1.AND.ischem.LE.2)Dv(Lecid)=Dehf+(ecorr)
      if(method.EQ.0.AND.ischem.LE.2)Dv(Lecids)=Dehf+(ecorr)
      if(iupdat.EQ.0)then
      if(method.LE.1)Dv(Lenrgy)=Dehf+dvar
      if(method.EQ.6)Dv(Lenrgy)=Dehf+ecorr
      if(method.LE.1.AND.ischem.EQ.3)Dv(Lenrgy)=Dehf+Elccd
      endif
      Dv(Lanorm)=Anorm
      Dv(La0)=A00
      if(method.EQ.6)Dv(Lccd)=ew1
      if(method.LE.1.AND.ischem.EQ.3)Dv(Llccd)=Dehf+Elccd
      call twrite(Igeno,Dv,Ligen,1,Ligen,1,0)
      
      if(Niter.NE.1)then
      if(inhexp.NE.0.OR.Inr.NE.0)then
      if(gabs(ecorr-Energy).LT.Delmax)goto 300
      endif
      endif
      Energy=ecorr
      
      if(Niter.GE.Maxit)then
      
      
      if(method.LE.1)write(Iout,99022)
      else
      
      if(Idump.GT.1)call fdump
      call twrite(Icivar,A00,Lcivar,1,Lcivar,1,0)
      call twrite(Iextrp,Dep,Lextrp,1,Lextrp,1,0)
      JUMP=ibskp
      goto 400
      endif
300   if(method.LE.1)write(Iout,99023)
      if(ipairp.EQ.1.OR.ipairp.EQ.4)call printp(Nae)
      if(idens.GT.0)call cidens(Nbasis)
      if(iscan.GT.0)call scan(A00,Davail,Savail,Nbasis)
      
      if(Idump.GT.1)call fdump
      call twrite(Icivar,A00,Lcivar,1,Lcivar,1,0)
      call twrite(Iextrp,Dep,Lextrp,1,Lextrp,1,0)
      JUMP=0
400   return
      
      end
C* :1 * 
      
