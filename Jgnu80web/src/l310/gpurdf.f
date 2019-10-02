
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gpurdf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gpurdf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 72 "gpurdf.web"
      subroutine gpurdf(INTC)
      implicit none
      double precision Dbuf2e,dx2,dy2,dz2,fx2y,fx2z,fx3,fxy2,fxz2,fy2z,f
     &y3,fyz2,fz3,Pt5,R1,R2,R3,R3ov2,R4,Root15
      double precision Root3,Root5,Tq,Tqnew,Z1,Z2,Z3
      integer i,Iend,iendp,iflag,Ifpure,iind,ij,ijk,Imj,Imk,Imkjml,In,in
     &d,indis,indjs,indks,inds,inds1,inds2,indx
      integer indx1,indx2,indx3,inew,INTC,intcnt,Iout,Ipunch,Ipurd,Ipure
     &,Ipurf,Irange,irm1,irmi,irngp,irp1,isj,Istart,istm1,itemp
      integer Itype,j,Jend,jendp,jind,jklr1,jklr2,jklr3,jklr4,jklr5,jklr
     &6,jklr7,jklr8,jklr9,jlim,Jml,jnd,jnew,Jpure,Jrange
      integer jrngp,jsksl,Jstart,jstm1,jtemp,Jtype,k,Kend,kendp,kind,kl,
     &klim,klr1,klr2,klr3,klr4,klr5,klr6,klr7,klr8
      integer klr9,klrp,Kml,knd,knew,Kpure,Krange,krm1,krmk,krngp,krp1,k
     &sl,Kstart,kstm1,ktemp,Ktype,l,Lamax,Lbmax,Lbound
      integer Lcmax,Ldmax,Lend,lendp,Lentq,limij,limijk,llim,lnd,lnew,Lp
     &max,Lpqmax,Lpure,Lqmax,Lr1,lr2,lr3,lr4,lr5,lr6
      integer lr7,lr8,lr9,Lrange,lrngp,Lstart,lstm1,Ltype,lwatq,lwatqn,N
     &10ord,N5ord,N6ord,N7ord,Nordr,ntt,Numdf
      integer Ubound,Ulpure
      dimension Tqnew(10000)
      dimension iind(20),jind(20),kind(20)
      common/io/In,Iout,Ipunch
      common/twbuf2/Tq(10000),Dbuf2e(4760)
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/limit/Imj,Imk,Jml,Kml,Imkjml,Istart,Jstart,Kstart,Lstart,Ie
     &nd,Jend,Kend,Lend,Irange,Jrange,Krange,Lrange,Lentq,Numdf
      common/stypes/Itype,Jtype,Ktype,Ltype
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/ipure/Ipurd,Ipurf
      common/jpure/Ifpure(4,4),Ipure,Jpure,Kpure,Lpure
      equivalence(Tqnew(1),Tq(1))
      equivalence(irp1,krp1),(irm1,krm1),(irmi,krmk)
      equivalence(krmk,llim)
      equivalence(Lr1,Lrange),(klr1,ksl),(jklr1,jsksl)
      
      
      if(iabs(Ipure)+iabs(Jpure)+iabs(Kpure)+iabs(Lpure).NE.0)then
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
      do 50 k=Kstart,Kend
      kind(k)=itemp
      itemp=itemp+Lrange
50    continue
      itemp=0
      do 100 j=Jstart,Jend
      jind(j)=itemp
      itemp=itemp+ksl
100   continue
      itemp=0
      do 150 i=Istart,Iend
      iind(i)=itemp
      itemp=itemp+jsksl
150   continue
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      if(Imj*Kml*Imkjml.EQ.0)then
      
      
      if(Imj+Imkjml.EQ.0)then
      
      
      
      
      indx1=Lentq
      indx2=INTC+1
      ind=Iend+1
      do 170 i=Istart,Iend
      ind=ind-1
      jnd=Jend+1
      do 165 j=Jstart,Jend
      jnd=jnd-1
      knd=Kend+1
      do 160 k=Kstart,Kend
      knd=knd-1
      lnd=Lend+1
      do 158 l=Lstart,Lend
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
      if(jnew.GE.lnew)goto 152
      else
      goto 152
      endif
      itemp=inew
      inew=knew
      knew=itemp
      itemp=jnew
      jnew=lnew
      lnew=itemp
      goto 154
      
152   if(iflag.NE.0)then
      indx2=indx2-1
      Tqnew(indx1)=Tq(indx2)
      goto 156
      endif
154   indx3=iind(inew)+jind(jnew)+kind(knew)+lnew-lstm1
      Tqnew(indx1)=Tq(indx3)
156   indx1=indx1-1
158   continue
160   continue
165   continue
170   continue
      
      
      elseif(Imkjml.EQ.0)then
      
      
      
      
      
      
      
      
      
      
      
      
      indx1=Lentq
      indx2=INTC+1
      ind=Iend+1
      do 190 i=Istart,Iend
      ind=ind-1
      itemp=kind(ind)-jstm1
      jnd=Jend+1
      do 180 j=Jstart,Jend
      jnd=jnd-1
      jtemp=itemp+jnd
      knd=Kend+1
      do 178 k=Kstart,Kend
      knd=knd-1
      ktemp=jtemp+iind(knd)
      lnd=Lend+1
      do 176 l=Lstart,Lend
      lnd=lnd-1
      if(ind.LT.knd)then
      elseif(ind.EQ.knd)then
      if(jnd.GE.lnd)goto 172
      else
      goto 172
      endif
      indx3=ktemp+jind(lnd)
      Tqnew(indx1)=Tqnew(indx3)
      goto 174
172   indx2=indx2-1
      Tqnew(indx1)=Tq(indx2)
174   indx1=indx1-1
176   continue
178   continue
180   continue
      
      
190   continue
      
      
      elseif(Imj+Kml.NE.0)then
      
      
      if(Imj.NE.0)then
      
      
      
      
      
      
      
      if(Krange.GT.1)then
      ntt=(Krange*(Krange+1))/2
      krp1=Krange+1
      krm1=Krange-1
      lwatq=INTC
      lwatqn=Lentq
      do 200 ij=1,isj
      indx1=lwatqn
      indx2=lwatq
      lwatq=lwatq-ntt
      lwatqn=lwatqn-ksl
      do 194 k=1,Krange
      llim=krp1-k
      do 192 l=1,llim
      Tqnew(indx1)=Tq(indx2)
      indx2=indx2-1
      indx1=indx1-1
192   continue
      indx1=indx1-k
194   continue
      indx1=lwatqn+2
      indx2=lwatqn+krp1
      do 198 k=1,krm1
      inds1=indx1
      inds2=indx2
      krmk=Krange-k
      do 196 l=1,krmk
      Tqnew(indx1)=Tqnew(indx2)
      indx1=indx1+1
      indx2=indx2+Krange
196   continue
      indx1=inds1+krp1
      indx2=inds2+krp1
198   continue
      
      
200   continue
      endif
      
      
      
      
      
      
      
      
      
      elseif(Irange.GT.1)then
      
      
      indx1=Lentq+1
      indx2=INTC+1
      ind=Iend+1
      do 210 i=Istart,Iend
      ind=ind-1
      jnd=Jend+1
      do 206 j=Jstart,Jend
      jnd=jnd-1
      if(ind.LT.jnd)then
      indx3=iind(jnd)+jind(ind)+ksl
      do 202 kl=1,ksl
      indx1=indx1-1
      Tqnew(indx1)=Tqnew(indx3)
      indx3=indx3-1
202   continue
      else
      do 204 kl=1,ksl
      indx1=indx1-1
      indx2=indx2-1
      Tqnew(indx1)=Tq(indx2)
204   continue
      endif
206   continue
      
      
210   continue
      endif
      
      
      
      
      
      
      elseif(Krange.NE.1)then
      lwatq=INTC
      lwatqn=Lentq
      ntt=(Krange*(Krange+1))/2
      krp1=Krange+1
      krm1=Krange-1
      ind=Iend+1
      do 230 i=Istart,Iend
      ind=ind-1
      jnd=Jend+1
      do 225 j=Jstart,Jend
      jnd=jnd-1
      if(ind.LT.jnd)then
      
      
      indx1=lwatqn
      lwatqn=lwatqn-ksl
      indx2=iind(jnd)+jind(ind)+ksl
      do 212 kl=1,ksl
      Tqnew(indx1)=Tqnew(indx2)
      indx1=indx1-1
      indx2=indx2-1
212   continue
      else
      indx2=lwatq
      indx1=lwatqn
      lwatq=lwatq-ntt
      lwatqn=lwatqn-ksl
      do 216 k=1,Krange
      llim=krp1-k
      do 214 l=1,llim
      Tqnew(indx1)=Tq(indx2)
      indx2=indx2-1
      indx1=indx1-1
214   continue
      indx1=indx1-k
216   continue
      indx1=lwatqn+2
      indx2=lwatqn+krp1
      do 220 k=1,krm1
      inds1=indx1
      inds2=indx2
      krmk=Krange-k
      do 218 l=1,krmk
      Tqnew(indx1)=Tqnew(indx2)
      indx1=indx1+1
      indx2=indx2+Krange
218   continue
      indx1=inds1+krp1
      indx2=inds2+krp1
220   continue
      endif
225   continue
      
230   continue
      else
      
      
      
      indx1=Lentq
      indx2=INTC
      irp1=Irange+1
      irm1=Irange-1
      do 240 i=1,Irange
      jlim=irp1-i
      do 235 j=1,jlim
      Tqnew(indx1)=Tq(indx2)
      indx2=indx2-1
      indx1=indx1-1
235   continue
      indx1=indx1-i
240   continue
      indx1=2
      indx2=irp1
      do 250 i=1,irm1
      inds1=indx1
      inds2=indx2
      irmi=Irange-i
      do 245 j=1,irmi
      Tqnew(indx1)=Tqnew(indx2)
      indx1=indx1+1
      indx2=indx2+Jrange
245   continue
      indx1=inds1+irp1
      indx2=inds2+irp1
      
      
250   continue
      endif
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
      
      do 260 ijk=1,limijk
      
      dx2=Tqnew(indx)
      dy2=Tqnew(indx+1)
      dz2=Tqnew(indx+2)
      
      Tqnew(indx)=dz2-Pt5*(dx2+dy2)
      Tqnew(indx+1)=Tqnew(indx+4)
      Tqnew(indx+2)=Tqnew(indx+5)
      Tqnew(indx+4)=Tqnew(indx+3)
      Tqnew(indx+3)=R3ov2*(dx2-dy2)
      
      indx=indx+Lrange
      
260   continue
      elseif(Lpure.NE.0)then
      
      
      
      
      indx=1
      
      do 280 ijk=1,limijk
      
      fx3=Tqnew(indx)
      fy3=Tqnew(indx+1)
      fz3=Tqnew(indx+2)
      fxy2=Tqnew(indx+3)
      fx2y=Tqnew(indx+4)
      fx2z=Tqnew(indx+5)
      fxz2=Tqnew(indx+6)
      fyz2=Tqnew(indx+7)
      fy2z=Tqnew(indx+8)
      
      Tqnew(indx)=fz3-R2*(fx2z+fy2z)
      Tqnew(indx+1)=R4*(Z1*fxz2-fx3-Z2*fxy2)
      Tqnew(indx+2)=R4*(Z1*fyz2-fy3-Z2*fx2y)
      Tqnew(indx+3)=R3*(fx2z-fy2z)
      Tqnew(indx+4)=Tqnew(indx+9)
      Tqnew(indx+5)=R1*(fx3-Z3*fxy2)
      Tqnew(indx+6)=R1*(Z3*fx2y-fy3)
      
      indx=indx+Lrange
280   continue
      endif
      
      
      
      if(Kpure.LT.0)then
      
      
      inds=(5-Kstart)*Lrange+1
      
      do 300 ij=1,limij
      
      indx=inds
      do 290 l=1,lrngp
      
      dx2=Tqnew(indx)
      dy2=Tqnew(indx+Lr1)
      dz2=Tqnew(indx+lr2)
      
      Tqnew(indx)=dz2-Pt5*(dx2+dy2)
      Tqnew(indx+Lr1)=Tqnew(indx+lr4)
      Tqnew(indx+lr2)=Tqnew(indx+lr5)
      Tqnew(indx+lr4)=Tqnew(indx+lr3)
      Tqnew(indx+lr3)=R3ov2*(dx2-dy2)
      
      indx=indx+1
290   continue
      inds=inds+ksl
      
300   continue
      elseif(Kpure.NE.0)then
      
      
      
      lr6=lr5+Lr1
      lr7=lr6+Lr1
      lr8=lr7+Lr1
      lr9=lr8+Lr1
      
      inds=1
      
      do 320 ij=1,limij
      indx=inds
      do 310 l=1,lrngp
      fx3=Tqnew(indx)
      fy3=Tqnew(indx+Lr1)
      fz3=Tqnew(indx+lr2)
      fxy2=Tqnew(indx+lr3)
      fx2y=Tqnew(indx+lr4)
      fx2z=Tqnew(indx+lr5)
      fxz2=Tqnew(indx+lr6)
      fyz2=Tqnew(indx+lr7)
      fy2z=Tqnew(indx+lr8)
      
      Tqnew(indx)=fz3-R2*(fx2z+fy2z)
      Tqnew(indx+Lr1)=R4*(Z1*fxz2-fx3-Z2*fxy2)
      Tqnew(indx+lr2)=R4*(Z1*fyz2-fy3-Z2*fx2y)
      Tqnew(indx+lr3)=R3*(fx2z-fy2z)
      Tqnew(indx+lr4)=Tqnew(indx+lr9)
      Tqnew(indx+lr5)=R1*(fx3-Z3*fxy2)
      Tqnew(indx+lr6)=R1*(Z3*fx2y-fy3)
      
      indx=indx+1
310   continue
      inds=inds+ksl
320   continue
      endif
      
      
      
      if(Jpure.LT.0)then
      
      
      
      indis=(5-Jstart)*ksl+1
      
      do 340 i=1,Irange
      indks=indis
      
      do 330 k=1,krngp
      indx=indks
      do 325 l=1,lrngp
      
      dx2=Tqnew(indx)
      dy2=Tqnew(indx+klr1)
      dz2=Tqnew(indx+klr2)
      
      Tqnew(indx)=dz2-Pt5*(dx2+dy2)
      Tqnew(indx+klr1)=Tqnew(indx+klr4)
      Tqnew(indx+klr2)=Tqnew(indx+klr5)
      Tqnew(indx+klr4)=Tqnew(indx+klr3)
      Tqnew(indx+klr3)=R3ov2*(dx2-dy2)
      
      indx=indx+1
325   continue
      indks=indks+Lrange
330   continue
      indis=indis+jsksl
      
340   continue
      elseif(Jpure.NE.0)then
      
      
      
      klr6=klr5+klr1
      klr7=klr6+klr1
      klr8=klr7+klr1
      klr9=klr8+klr1
      
      
      indis=1
      do 360 i=1,Irange
      indks=indis
      do 350 k=1,krngp
      indx=indks
      do 345 l=1,lrngp
      
      fx3=Tqnew(indx)
      fy3=Tqnew(indx+klr1)
      fz3=Tqnew(indx+klr2)
      fxy2=Tqnew(indx+klr3)
      fx2y=Tqnew(indx+klr4)
      fx2z=Tqnew(indx+klr5)
      fxz2=Tqnew(indx+klr6)
      fyz2=Tqnew(indx+klr7)
      fy2z=Tqnew(indx+klr8)
      
      Tqnew(indx)=fz3-R2*(fx2z+fy2z)
      Tqnew(indx+klr1)=R4*(Z1*fxz2-fx3-Z2*fxy2)
      Tqnew(indx+klr2)=R4*(Z1*fyz2-fy3-Z2*fx2y)
      Tqnew(indx+klr3)=R3*(fx2z-fy2z)
      Tqnew(indx+klr4)=Tqnew(indx+klr9)
      Tqnew(indx+klr5)=R1*(fx3-Z3*fxy2)
      Tqnew(indx+klr6)=R1*(Z3*fx2y-fy3)
      
      indx=indx+1
345   continue
      indks=indks+Lrange
350   continue
      indis=indis+jsksl
360   continue
      endif
      
      
      
      if(Ipure.LT.0)then
      
      
      indjs=(5-Istart)*jsksl+1
      
      do 380 j=1,jrngp
      indks=indjs
      do 370 k=1,krngp
      indx=indks
      do 365 l=1,lrngp
      
      dx2=Tqnew(indx)
      dy2=Tqnew(indx+jklr1)
      dz2=Tqnew(indx+jklr2)
      
      Tqnew(indx)=dz2-Pt5*(dx2+dy2)
      Tqnew(indx+jklr1)=Tqnew(indx+jklr4)
      Tqnew(indx+jklr2)=Tqnew(indx+jklr5)
      Tqnew(indx+jklr4)=Tqnew(indx+jklr3)
      Tqnew(indx+jklr3)=R3ov2*(dx2-dy2)
      
      indx=indx+1
365   continue
      indks=indks+Lrange
370   continue
      indjs=indjs+ksl
      
380   continue
      elseif(Ipure.NE.0)then
      
      
      
      jklr6=jklr5+jklr1
      jklr7=jklr6+jklr1
      jklr8=jklr7+jklr1
      jklr9=jklr8+jklr1
      
      indjs=1
      
      do 400 j=1,jrngp
      indks=indjs
      do 390 k=1,krngp
      indx=indks
      do 385 l=1,lrngp
      
      fx3=Tqnew(indx)
      fy3=Tqnew(indx+jklr1)
      fz3=Tqnew(indx+jklr2)
      fxy2=Tqnew(indx+jklr3)
      fx2y=Tqnew(indx+jklr4)
      fx2z=Tqnew(indx+jklr5)
      fxz2=Tqnew(indx+jklr6)
      fyz2=Tqnew(indx+jklr7)
      fy2z=Tqnew(indx+jklr8)
      
      Tqnew(indx)=fz3-R2*(fx2z+fy2z)
      Tqnew(indx+jklr1)=R4*(Z1*fxz2-fx3-Z2*fxy2)
      Tqnew(indx+jklr2)=R4*(Z1*fyz2-fy3-Z2*fx2y)
      Tqnew(indx+jklr3)=R3*(fx2z-fy2z)
      Tqnew(indx+jklr4)=Tqnew(indx+jklr9)
      Tqnew(indx+jklr5)=R1*(fx3-Z3*fxy2)
      Tqnew(indx+jklr6)=R1*(Z3*fx2y-fy3)
      
      indx=indx+1
385   continue
      indks=indks+Lrange
390   continue
      indjs=indjs+ksl
400   continue
      endif
      
      
      
      intcnt=0
      jlim=jendp
      klim=kendp
      do 450 i=Istart,iendp
      itemp=iind(i)-lstm1
      if(Imj.EQ.0)jlim=i
      if(Imkjml.EQ.0)klim=i
      do 420 j=Jstart,jlim
      jtemp=itemp+jind(j)
      do 410 k=Kstart,klim
      ktemp=jtemp+kind(k)
      llim=lendp
      if(Kml.EQ.0)llim=k
      if(Imkjml+iabs(i-k).EQ.0)llim=j
      do 405 l=Lstart,llim
      intcnt=intcnt+1
      Tqnew(intcnt)=Tqnew(ktemp+l)
405   continue
410   continue
420   continue
450   continue
      
      
      Iend=iendp
      Jend=jendp
      Kend=kendp
      Lend=lendp
      endif
      
      
      return
      
      end
C* :1 * 
      
