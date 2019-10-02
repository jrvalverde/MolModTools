
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 purdf2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "purdf2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 80 "purdf2.web"
      subroutine purdf2(INTC,ISMODE,ISET,TQ,TQNEW,INTCP)
      implicit none
      double precision dx2,dy2,dz2,fx2y,fx2z,fx3,fxy2,fxz2,fy2z,fy3,fyz2
     &,fz3,Pt5,R1,R2,R3,R3ov2,R4,Root15,Root3
      double precision Root5,TQ,TQNEW,Z1,Z2,Z3
      integer i,Iend,iendp,iflag,Ifpure,iind,ij,ijk,Imj,Imk,Imkjml,In,in
     &d,indis,indjs,indks,inds,inds1,inds2,indx
      integer indx1,indx2,indx3,inew,INTC,intcnt,INTCP,Iout,Ipunch,Ipurd
     &,Ipure,Ipurf,Irange,irm1,irmi,irngp,irp1,ISET,isj,ISMODE
      integer Istart,Istm,istm1,itemp,itrflg,Itype,j,Jend,jendp,jind,jkl
     &r1,jklr2,jklr3,jklr4,jklr5,jklr6,jklr7,jklr8,jklr9,jlim
      integer Jml,jnd,jnew,Jpure,Jrange,jrngp,jsksl,Jstart,Jstm,jstm1,jt
     &emp,Jtype,k,Kend,kendp,kind,kl,klim,klr1,klr2
      integer klr3,klr4,klr5,klr6,klr7,klr8,klr9,klrp,Kml,knd,knew,Kpure
     &,Krange,krm1,krmk,krngp,krp1,ksl,Kstart,Kstm
      integer kstm1,ktemp,Ktype,l,Lamax,Lbmax,Lbound,Lcmax,Ldmax,Lend,le
     &ndp,Lentq,limij,limijk,llim,lnd,lnew,Lpmax,Lpqmax,Lpure
      integer Lqmax,Lr1,lr2,lr3,lr4,lr5,lr6,lr7,lr8,lr9,Lrange,lrngp,Lst
     &art,Lstm,lstm1,Ltype,lwatq,lwatqn,N10ord,N5ord
      integer N6ord,N7ord,Nfa,Nfb,Nfc,Nfd,Nordr,ntt,Numdf
      integer Ubound,Ulpure
      dimension TQ(*),TQNEW(*)
      dimension iind(20),jind(20),kind(20)
      common/io/In,Iout,Ipunch
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/limit/Imj,Imk,Jml,Kml,Imkjml,Istart,Jstart,Kstart,Lstart,Ie
     &nd,Jend,Kend,Lend,Irange,Jrange,Krange,Lrange,Lentq,Numdf
      common/stypes/Itype,Jtype,Ktype,Ltype
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/ipure/Ipurd,Ipurf
      common/jpure/Ifpure(4,4),Ipure,Jpure,Kpure,Lpure
      common/nf/Nfa,Nfb,Nfc,Nfd,Istm,Jstm,Kstm,Lstm
      equivalence(irp1,krp1),(irm1,krm1),(irmi,krmk)
      equivalence(krmk,llim)
      equivalence(Lr1,Lrange),(klr1,ksl),(jklr1,jsksl)
      
      
      itrflg=iabs(Ipure)+iabs(Jpure)+iabs(Kpure)+iabs(Lpure)
      
      INTCP=INTC
      
      
      if(ISMODE.EQ.0)then
      
      if(itrflg.EQ.0)goto 1000
      endif
      Iend=Ubound(Lamax)
      Jend=Ubound(Lbmax)
      Kend=Ubound(Lcmax)
      Lend=Ubound(Ldmax)
      
      
      ksl=Krange*Lrange
      jsksl=Jrange*ksl
      isj=Irange*Jrange
      istm1=Istart-1
      jstm1=Jstart-1
      kstm1=Kstart-1
      lstm1=Lstart-1
      
      
      itemp=0
      do 100 k=Kstart,Kend
      kind(k)=itemp
      itemp=itemp+Lrange
100   continue
      itemp=0
      do 200 j=Jstart,Jend
      jind(j)=itemp
      itemp=itemp+ksl
200   continue
      itemp=0
      do 300 i=Istart,Iend
      iind(i)=itemp
      itemp=itemp+jsksl
300   continue
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      if(Imj*Kml*Imkjml.EQ.0)then
      
      
      if(Imj+Imkjml.EQ.0)then
      
      
      
      
      indx1=Lentq
      indx2=INTC+1
      ind=Iend+1
      do 340 i=Istart,Iend
      ind=ind-1
      jnd=Jend+1
      do 320 j=Jstart,Jend
      jnd=jnd-1
      knd=Kend+1
      do 310 k=Kstart,Kend
      knd=knd-1
      lnd=Lend+1
      do 308 l=Lstart,Lend
      lnd=lnd-1
      iflag=1
      inew=ind
      jnew=jnd
      knew=knd
      lnew=lnd
      if(inew.LT.jnew)then
      itemp=inew
      inew=jnew
      jnew=itemp
      iflag=0
      endif
      if(knew.LT.lnew)then
      itemp=knew
      knew=lnew
      lnew=itemp
      iflag=0
      endif
      if(inew.LT.knew)then
      elseif(inew.EQ.knew)then
      if(jnew.GE.lnew)goto 302
      else
      goto 302
      endif
      itemp=inew
      inew=knew
      knew=itemp
      itemp=jnew
      jnew=lnew
      lnew=itemp
      goto 304
      
302   if(iflag.NE.0)then
      indx2=indx2-1
      TQNEW(indx1)=TQ(indx2)
      goto 306
      endif
304   indx3=iind(inew)+jind(jnew)+kind(knew)+lnew-lstm1
      TQNEW(indx1)=TQ(indx3)
306   indx1=indx1-1
308   continue
310   continue
320   continue
340   continue
      
      
      elseif(Imkjml.EQ.0)then
      
      
      
      
      
      
      
      
      
      
      
      
      indx1=Lentq
      indx2=INTC+1
      ind=Iend+1
      do 380 i=Istart,Iend
      ind=ind-1
      itemp=kind(ind)-jstm1
      jnd=Jend+1
      do 360 j=Jstart,Jend
      jnd=jnd-1
      jtemp=itemp+jnd
      knd=Kend+1
      do 350 k=Kstart,Kend
      knd=knd-1
      ktemp=jtemp+iind(knd)
      lnd=Lend+1
      do 346 l=Lstart,Lend
      lnd=lnd-1
      if(ind.LT.knd)then
      elseif(ind.EQ.knd)then
      if(jnd.GE.lnd)goto 342
      else
      goto 342
      endif
      indx3=ktemp+jind(lnd)
      TQNEW(indx1)=TQNEW(indx3)
      goto 344
342   indx2=indx2-1
      TQNEW(indx1)=TQ(indx2)
344   indx1=indx1-1
346   continue
350   continue
360   continue
      
      
380   continue
      
      
      elseif(Imj+Kml.NE.0)then
      
      
      if(Imj.NE.0)then
      
      
      
      
      
      
      
      if(Krange.GT.1)then
      ntt=(Krange*(Krange+1))/2
      krp1=Krange+1
      krm1=Krange-1
      lwatq=INTC
      lwatqn=Lentq
      do 390 ij=1,isj
      indx1=lwatqn
      indx2=lwatq
      lwatq=lwatq-ntt
      lwatqn=lwatqn-ksl
      do 384 k=1,Krange
      llim=krp1-k
      do 382 l=1,llim
      TQNEW(indx1)=TQ(indx2)
      indx2=indx2-1
      indx1=indx1-1
382   continue
      indx1=indx1-k
384   continue
      indx1=lwatqn+2
      indx2=lwatqn+krp1
      do 388 k=1,krm1
      inds1=indx1
      inds2=indx2
      krmk=Krange-k
      do 386 l=1,krmk
      TQNEW(indx1)=TQNEW(indx2)
      indx1=indx1+1
      indx2=indx2+Krange
386   continue
      indx1=inds1+krp1
      indx2=inds2+krp1
388   continue
      
      
390   continue
      endif
      
      
      
      
      
      
      
      
      
      elseif(Irange.GT.1)then
      
      
      indx1=Lentq+1
      indx2=INTC+1
      ind=Iend+1
      do 400 i=Istart,Iend
      ind=ind-1
      jnd=Jend+1
      do 395 j=Jstart,Jend
      jnd=jnd-1
      if(ind.LT.jnd)then
      indx3=iind(jnd)+jind(ind)+ksl
      do 392 kl=1,ksl
      indx1=indx1-1
      TQNEW(indx1)=TQNEW(indx3)
      indx3=indx3-1
392   continue
      else
      do 394 kl=1,ksl
      indx1=indx1-1
      indx2=indx2-1
      TQNEW(indx1)=TQ(indx2)
394   continue
      endif
395   continue
      
      
400   continue
      endif
      
      
      
      
      
      
      elseif(Krange.NE.1)then
      lwatq=INTC
      lwatqn=Lentq
      ntt=(Krange*(Krange+1))/2
      krp1=Krange+1
      krm1=Krange-1
      ind=Iend+1
      do 440 i=Istart,Iend
      ind=ind-1
      jnd=Jend+1
      do 420 j=Jstart,Jend
      jnd=jnd-1
      if(ind.LT.jnd)then
      
      
      indx1=lwatqn
      lwatqn=lwatqn-ksl
      indx2=iind(jnd)+jind(ind)+ksl
      do 402 kl=1,ksl
      TQNEW(indx1)=TQNEW(indx2)
      indx1=indx1-1
      indx2=indx2-1
402   continue
      else
      indx2=lwatq
      indx1=lwatqn
      lwatq=lwatq-ntt
      lwatqn=lwatqn-ksl
      do 406 k=1,Krange
      llim=krp1-k
      do 404 l=1,llim
      TQNEW(indx1)=TQ(indx2)
      indx2=indx2-1
      indx1=indx1-1
404   continue
      indx1=indx1-k
406   continue
      indx1=lwatqn+2
      indx2=lwatqn+krp1
      do 410 k=1,krm1
      inds1=indx1
      inds2=indx2
      krmk=Krange-k
      do 408 l=1,krmk
      TQNEW(indx1)=TQNEW(indx2)
      indx1=indx1+1
      indx2=indx2+Krange
408   continue
      indx1=inds1+krp1
      indx2=inds2+krp1
410   continue
      endif
420   continue
      
440   continue
      else
      
      
      
      indx1=Lentq
      indx2=INTC
      irp1=Irange+1
      irm1=Irange-1
      do 460 i=1,Irange
      jlim=irp1-i
      do 450 j=1,jlim
      TQNEW(indx1)=TQ(indx2)
      indx2=indx2-1
      indx1=indx1-1
450   continue
      indx1=indx1-i
460   continue
      indx1=2
      indx2=irp1
      do 480 i=1,irm1
      inds1=indx1
      inds2=indx2
      irmi=Irange-i
      do 470 j=1,irmi
      TQNEW(indx1)=TQNEW(indx2)
      indx1=indx1+1
      indx2=indx2+Jrange
470   continue
      indx1=inds1+irp1
      indx2=inds2+irp1
      
      
480   continue
      endif
      endif
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      if(ISMODE.NE.0)then
      if(itrflg.EQ.0)goto 900
      endif
      lr2=Lr1+Lr1
      lr3=lr2+Lr1
      lr4=lr3+Lr1
      lr5=lr4+Lr1
      klr2=klr1+klr1
      klr3=klr2+klr1
      klr4=klr3+klr1
      klr5=klr4+klr1
      jklr2=jklr1+jklr1
      jklr3=jklr2+jklr1
      jklr4=jklr3+jklr1
      jklr5=jklr4+jklr1
      
      iendp=Iend
      if(Ipure.NE.0)iendp=Ulpure(Lamax)
      jendp=Jend
      if(Jpure.NE.0)jendp=Ulpure(Lbmax)
      kendp=Kend
      if(Kpure.NE.0)kendp=Ulpure(Lcmax)
      lendp=Lend
      if(Lpure.NE.0)lendp=Ulpure(Ldmax)
      irngp=iendp-istm1
      jrngp=jendp-jstm1
      krngp=kendp-kstm1
      lrngp=lendp-lstm1
      
      limij=Irange*Jrange
      limijk=limij*Krange
      klrp=krngp*lrngp
      
      
      if(Lpure.LT.0)then
      
      
      indx=5-lstm1
      
      do 500 ijk=1,limijk
      
      dx2=TQNEW(indx)
      dy2=TQNEW(indx+1)
      dz2=TQNEW(indx+2)
      
      TQNEW(indx)=dz2-Pt5*(dx2+dy2)
      TQNEW(indx+1)=TQNEW(indx+4)
      TQNEW(indx+2)=TQNEW(indx+5)
      TQNEW(indx+4)=TQNEW(indx+3)
      TQNEW(indx+3)=R3ov2*(dx2-dy2)
      
      indx=indx+Lrange
      
500   continue
      elseif(Lpure.NE.0)then
      
      
      
      
      indx=1
      
      do 550 ijk=1,limijk
      
      fx3=TQNEW(indx)
      fy3=TQNEW(indx+1)
      fz3=TQNEW(indx+2)
      fxy2=TQNEW(indx+3)
      fx2y=TQNEW(indx+4)
      fx2z=TQNEW(indx+5)
      fxz2=TQNEW(indx+6)
      fyz2=TQNEW(indx+7)
      fy2z=TQNEW(indx+8)
      
      TQNEW(indx)=fz3-R2*(fx2z+fy2z)
      TQNEW(indx+1)=R4*(Z1*fxz2-fx3-Z2*fxy2)
      TQNEW(indx+2)=R4*(Z1*fyz2-fy3-Z2*fx2y)
      TQNEW(indx+3)=R3*(fx2z-fy2z)
      TQNEW(indx+4)=TQNEW(indx+9)
      TQNEW(indx+5)=R1*(fx3-Z3*fxy2)
      TQNEW(indx+6)=R1*(Z3*fx2y-fy3)
      
      indx=indx+Lrange
550   continue
      endif
      
      
      
      if(Kpure.LT.0)then
      
      
      inds=(5-Kstart)*Lrange+1
      
      do 600 ij=1,limij
      
      indx=inds
      do 560 l=1,lrngp
      
      dx2=TQNEW(indx)
      dy2=TQNEW(indx+Lr1)
      dz2=TQNEW(indx+lr2)
      
      TQNEW(indx)=dz2-Pt5*(dx2+dy2)
      TQNEW(indx+Lr1)=TQNEW(indx+lr4)
      TQNEW(indx+lr2)=TQNEW(indx+lr5)
      TQNEW(indx+lr4)=TQNEW(indx+lr3)
      TQNEW(indx+lr3)=R3ov2*(dx2-dy2)
      
      indx=indx+1
560   continue
      inds=inds+ksl
      
600   continue
      elseif(Kpure.NE.0)then
      
      
      
      lr6=lr5+Lr1
      lr7=lr6+Lr1
      lr8=lr7+Lr1
      lr9=lr8+Lr1
      
      inds=1
      
      do 650 ij=1,limij
      indx=inds
      do 620 l=1,lrngp
      fx3=TQNEW(indx)
      fy3=TQNEW(indx+Lr1)
      fz3=TQNEW(indx+lr2)
      fxy2=TQNEW(indx+lr3)
      fx2y=TQNEW(indx+lr4)
      fx2z=TQNEW(indx+lr5)
      fxz2=TQNEW(indx+lr6)
      fyz2=TQNEW(indx+lr7)
      fy2z=TQNEW(indx+lr8)
      
      TQNEW(indx)=fz3-R2*(fx2z+fy2z)
      TQNEW(indx+Lr1)=R4*(Z1*fxz2-fx3-Z2*fxy2)
      TQNEW(indx+lr2)=R4*(Z1*fyz2-fy3-Z2*fx2y)
      TQNEW(indx+lr3)=R3*(fx2z-fy2z)
      TQNEW(indx+lr4)=TQNEW(indx+lr9)
      TQNEW(indx+lr5)=R1*(fx3-Z3*fxy2)
      TQNEW(indx+lr6)=R1*(Z3*fx2y-fy3)
      
      indx=indx+1
620   continue
      inds=inds+ksl
650   continue
      endif
      
      
      
      if(Jpure.LT.0)then
      
      
      
      indis=(5-Jstart)*ksl+1
      
      do 700 i=1,Irange
      indks=indis
      
      do 680 k=1,krngp
      indx=indks
      do 660 l=1,lrngp
      
      dx2=TQNEW(indx)
      dy2=TQNEW(indx+klr1)
      dz2=TQNEW(indx+klr2)
      
      TQNEW(indx)=dz2-Pt5*(dx2+dy2)
      TQNEW(indx+klr1)=TQNEW(indx+klr4)
      TQNEW(indx+klr2)=TQNEW(indx+klr5)
      TQNEW(indx+klr4)=TQNEW(indx+klr3)
      TQNEW(indx+klr3)=R3ov2*(dx2-dy2)
      
      indx=indx+1
660   continue
      indks=indks+Lrange
680   continue
      indis=indis+jsksl
      
700   continue
      elseif(Jpure.NE.0)then
      
      
      
      klr6=klr5+klr1
      klr7=klr6+klr1
      klr8=klr7+klr1
      klr9=klr8+klr1
      
      
      indis=1
      do 750 i=1,Irange
      indks=indis
      do 720 k=1,krngp
      indx=indks
      do 710 l=1,lrngp
      
      fx3=TQNEW(indx)
      fy3=TQNEW(indx+klr1)
      fz3=TQNEW(indx+klr2)
      fxy2=TQNEW(indx+klr3)
      fx2y=TQNEW(indx+klr4)
      fx2z=TQNEW(indx+klr5)
      fxz2=TQNEW(indx+klr6)
      fyz2=TQNEW(indx+klr7)
      fy2z=TQNEW(indx+klr8)
      
      TQNEW(indx)=fz3-R2*(fx2z+fy2z)
      TQNEW(indx+klr1)=R4*(Z1*fxz2-fx3-Z2*fxy2)
      TQNEW(indx+klr2)=R4*(Z1*fyz2-fy3-Z2*fx2y)
      TQNEW(indx+klr3)=R3*(fx2z-fy2z)
      TQNEW(indx+klr4)=TQNEW(indx+klr9)
      TQNEW(indx+klr5)=R1*(fx3-Z3*fxy2)
      TQNEW(indx+klr6)=R1*(Z3*fx2y-fy3)
      
      indx=indx+1
710   continue
      indks=indks+Lrange
720   continue
      indis=indis+jsksl
750   continue
      endif
      
      
      
      if(Ipure.LT.0)then
      
      
      indjs=(5-Istart)*jsksl+1
      
      do 800 j=1,jrngp
      indks=indjs
      do 780 k=1,krngp
      indx=indks
      do 760 l=1,lrngp
      
      dx2=TQNEW(indx)
      dy2=TQNEW(indx+jklr1)
      dz2=TQNEW(indx+jklr2)
      
      TQNEW(indx)=dz2-Pt5*(dx2+dy2)
      TQNEW(indx+jklr1)=TQNEW(indx+jklr4)
      TQNEW(indx+jklr2)=TQNEW(indx+jklr5)
      TQNEW(indx+jklr4)=TQNEW(indx+jklr3)
      TQNEW(indx+jklr3)=R3ov2*(dx2-dy2)
      
      indx=indx+1
760   continue
      indks=indks+Lrange
780   continue
      indjs=indjs+ksl
      
800   continue
      elseif(Ipure.NE.0)then
      
      
      
      jklr6=jklr5+jklr1
      jklr7=jklr6+jklr1
      jklr8=jklr7+jklr1
      jklr9=jklr8+jklr1
      
      indjs=1
      
      do 850 j=1,jrngp
      indks=indjs
      do 820 k=1,krngp
      indx=indks
      do 810 l=1,lrngp
      
      fx3=TQNEW(indx)
      fy3=TQNEW(indx+jklr1)
      fz3=TQNEW(indx+jklr2)
      fxy2=TQNEW(indx+jklr3)
      fx2y=TQNEW(indx+jklr4)
      fx2z=TQNEW(indx+jklr5)
      fxz2=TQNEW(indx+jklr6)
      fyz2=TQNEW(indx+jklr7)
      fy2z=TQNEW(indx+jklr8)
      
      TQNEW(indx)=fz3-R2*(fx2z+fy2z)
      TQNEW(indx+jklr1)=R4*(Z1*fxz2-fx3-Z2*fxy2)
      TQNEW(indx+jklr2)=R4*(Z1*fyz2-fy3-Z2*fx2y)
      TQNEW(indx+jklr3)=R3*(fx2z-fy2z)
      TQNEW(indx+jklr4)=TQNEW(indx+jklr9)
      TQNEW(indx+jklr5)=R1*(fx3-Z3*fxy2)
      TQNEW(indx+jklr6)=R1*(Z3*fx2y-fy3)
      
      indx=indx+1
810   continue
      indks=indks+Lrange
820   continue
      indjs=indjs+ksl
850   continue
      endif
      
      
      
900   if(ISMODE.EQ.0)then
      intcnt=0
      jlim=jendp
      klim=kendp
      do 950 i=Istart,iendp
      itemp=iind(i)-lstm1
      if(Imj.EQ.0)jlim=i
      if(Imkjml.EQ.0)klim=i
      do 920 j=Jstart,jlim
      jtemp=itemp+jind(j)
      do 910 k=Kstart,klim
      ktemp=jtemp+kind(k)
      llim=lendp
      if(Kml.EQ.0)llim=k
      if(Imkjml+iabs(i-k).EQ.0)llim=j
      do 905 l=Lstart,llim
      intcnt=intcnt+1
      TQNEW(intcnt)=TQNEW(ktemp+l)
905   continue
910   continue
920   continue
950   continue
      INTCP=intcnt
      endif
      
      
      if(itrflg.GT.0)then
      Iend=iendp
      Jend=jendp
      Kend=kendp
      Lend=lendp
      endif
      if(ISET.EQ.1)then
      if(itrflg.GT.0)then
      Nfa=irngp
      Nfb=jrngp
      Nfc=krngp
      Nfd=lrngp
      endif
      endif
      
      
1000  return
      
      end
C* :1 * 
      
