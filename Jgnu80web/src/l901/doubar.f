
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 doubar"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "doubar.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 112 "doubar.web"
      subroutine doubar
      implicit none
      double precision anorm1,Atmchg,C,e,E2,ed,eump2,gabs,gsqrt,one,S2,s
     &20,s21,small,T,tsqp1,V,zero
      integer i,Iad1,Iad2,Iad3,Ian,Ias1,Ias2,Ibckt,ibuck,Icharg,Idmm,Idu
     &mmy,Idump,Ieval,Igeno,ign,ii,In,ind,Inforb
      integer Ioab,Iop,Iopcl,Iout,Iprint,Ipunch,isbuc,Isc1,Isc2,Isc3,Isc
     &4,Iscd,Isd1,Isd10,Isd11,Isd12,Isd13,Isd14,Isd15,Isd16
      integer Isd17,Isd18,Isd2,Isd3,Isd4,Isd5,Isd6,Isd7,Isd8,Isd9,Ispect
     &,Iwd1,Iwd2,Iwd3,Iws1,Iws2,La0,Lanorm,lbuck,Lehf
      integer lend12,lend16,lend2,lend7,lengd,lengi,lengs,length,Lenrgy,
     &Ligen,Lisd,Lmp2,Lnforb,Loab,Ls20,Ls21,lscr,lscr1,Lspect,Maxbuc
      integer Mdv,mdv2,Multip,Nae,Natoms,nb2,Nbasis,Nbe,Ne,Noa,Noa2,Noa3
     &,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobuc,nobuc1
      integer nobuc9,Nobva,Nobvb,nom,Novaa,Novab,Novbb,novm,Nrorb,Nva,Nv
     &a2,Nva3,Nvavb,Nvb,Nvb2,Nvb3,nvm
      dimension lengd(50),lengs(50),e(140),Ibckt(50)
      dimension ign(280),ed(140)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/v/V(20000),Mdv
      common/rwfl/Igeno,Ligen,Inforb,Lnforb
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/nobuc/Nobuc
      common/bucknr/Isd1,Isd2,Isd3,Isd4,Isd5,Isd6,Isd7,Isd8,Isd9,Isd10,I
     &sd11,Isd12,Isd13,Isd14,Isd15,Isd16,Isd17,Isd18,Idmm(3),Iad1,Iad2,I
     &ad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Isc1,Isc2,Isc3,Isc4,Iscd(15
     &)
      common/lgen/Lehf,Lmp2,Ls20,Ls21,Lenrgy,Lanorm,La0,Lisd
      common/result/T,E2,S2
      common/io/In,Iout,Ipunch
      common/constr/Iopcl
      common/print/Iprint
      common/dump/Idump,Idummy
      equivalence(Ibckt(1),Isd1)
      equivalence(ign(1),ed(1),e(1))
      data zero,one/0.D0,1.D0/
      data small/1.D-9/
      data isbuc/50/
      
      
      
99001 format(d20.14)
99002 format(' SINGLE-BAR TO DOUBLE-BAR CONVERSION'/1x,35(1H*))
99003 format(' (S**2,0)=',d13.5,11x,'(S**2,1)=',d13.5)
99004 format(' NORM(A1)=',d13.5)
99005 format(' E2=',d22.8,8x,'EUMP2=',d24.11)
99006 format(' ',39x,d24.11)
99007 format(' THE FOLLOWING DOUBLE BAR BUCKETS ARE PRESENT:',21I4)
99008 format(/)
      
      
      Iprint=Iop(33)
      Idump=Iop(34)
      
      if(Iprint.GE.1)write(Iout,99002)
      
      call ilsw(2,1,Iopcl)
      
      call tread(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      
      do 100 i=1,18
      call tquery((i+isbuc),lengs(i))
100   continue
      
      do 200 i=1,Nobuc
      Ibckt(i)=i
200   continue
      nobuc1=Nobuc+1
      nobuc9=Nobuc+9
      do 300 i=nobuc1,nobuc9
      Ibckt(i)=2000+i
300   continue
      
      nb2=2*Nrorb
      call tread(Ieval,e,nb2,1,nb2,1,0)
      
      mdv2=Mdv/2
      
      
      do 400 i=1,Maxbuc
      lengd(i)=0
400   continue
      
      if(lengs(Isd5).NE.0)lengd(Isd1)=Noa3*Nva3
      if(lengs(Isd4).NE.0)lengd(Isd4)=Noa3*(Noa3+1)/2
      if(lengs(Isd5).NE.0.AND.lengs(Isd1).NE.0)lengd(Isd5)=Noava*Noava
      if(lengs(Isd11).NE.0)lengd(Isd11)=Noa3*Noava
      if(lengs(Isd15).NE.0)lengd(Isd15)=Noava*Nvb3
      lengd(Iad1)=lengd(Isd1)
      if(Iopcl.NE.0)then
      
      if(lengs(Isd2).NE.0)lengd(Isd2)=Novab
      if(lengs(Isd10).NE.0)lengd(Isd3)=Nob3*Nvb3
      if(lengs(Isd6).NE.0)lengd(Isd6)=Noa2*Nob2
      if(lengs(Isd7).NE.0)lengd(Isd7)=Noa2*Nvb2
      if(lengs(Isd8).NE.0)lengd(Isd8)=Nob2*Nva2
      if(lengs(Isd9).NE.0)lengd(Isd9)=Nob3*(Nob3+1)/2
      if(lengs(Isd10).NE.0.AND.lengs(Isd3).NE.0)lengd(Isd10)=Nobvb*Nobvb
      if(lengs(Isd12).NE.0)lengd(Isd12)=Noa2*Nobvb
      if(lengs(Isd13).NE.0)lengd(Isd13)=Nob2*Noava
      if(lengs(Isd14).NE.0)lengd(Isd14)=Nob3*Nobvb
      if(lengs(Isd16).NE.0)lengd(Isd16)=Noava*Nvb2
      if(lengs(Isd17).NE.0)lengd(Isd17)=Nobvb*Nva2
      if(lengs(Isd18).NE.0)lengd(Isd18)=Nobvb*Nvb3
      lengd(Iad2)=lengd(Isd2)
      lengd(Iad3)=lengd(Isd3)
      else
      if(lengs(Isd5).NE.0)lengd(Isd2)=Novab
      if(lengs(Isd4).NE.0)lengd(Isd6)=Noa2*Nob2
      if(lengs(Isd1).NE.0)lengd(Isd7)=Noa2*Nvb2
      if(lengs(Isd11).NE.0)lengd(Isd12)=Noa2*Nobvb
      if(lengs(Isd15).NE.0)lengd(Isd16)=Noava*Nvb2
      lengd(Iad2)=lengd(Isd2)
      endif
      
      nom=max0(Noa,Nob)
      nvm=max0(Nva,Nvb)
      novm=max0(nom,nvm)
      lscr=nom**2*novm**2
      lscr1=lscr
      if(lengs(Isd15).GT.0)lscr1=nom*nvm*novm**2
      lengd(Isc1-2000)=lscr1
      lengd(Isc2-2000)=lscr1
      lengd(Isc3-2000)=lscr
      
      do 500 i=1,Nobuc
      ibuck=Ibckt(i)
      if(lengd(ibuck).GT.0)call conddf(ibuck,lengd(ibuck))
500   continue
      
      do 600 i=nobuc1,nobuc9
      ibuck=Ibckt(i)
      lbuck=lengd(ibuck-2000)
      if(lbuck.GT.0)call defbuc(ibuck,lbuck)
600   continue
      
      
      
      
      if(lengs(Isd5).GT.0)call expsym(Noava,(Isd5+isbuc),Isc1)
      
      if(Iopcl.NE.0)then
      
      call trsfr(lengd(Isd2),(Isd2+isbuc),Isd2)
      call trsfr(lengd(Isd6),(Isd6+isbuc),Isd6)
      call trsfr(lengd(Isd7),(Isd7+isbuc),Isd7)
      call trsfr(lengd(Isd8),(Isd8+isbuc),Isd8)
      call trsfr(lengd(Isd12),(Isd12+isbuc),Isd12)
      call trsfr(lengd(Isd13),(Isd13+isbuc),Isd13)
      call trsfr(lengd(Isd16),(Isd16+isbuc),Isd16)
      call trsfr(lengd(Isd17),(Isd17+isbuc),Isd17)
      else
      
      lend2=lengd(Isd2)
      lend7=lengd(Isd7)
      lend12=lengd(Isd12)
      lend16=lengd(Isd16)
      call trsfr(lend2,Isc1,Isd2)
      if(lengd(Isd6).GT.0)call expsym(Noa2,(Isd4+isbuc),Isd6)
      call trsfr(lend7,(Isd1+isbuc),Isd7)
      call trsfr(lend12,(Isd11+isbuc),Isd12)
      call trsfr(lend16,(Isd15+isbuc),Isd16)
      endif
      
      
      
      
      T=zero
      E2=zero
      S2=zero
      
      if(lengs(Isd5).NE.0)then
      call mattrn(Noa,Nva,Noa,Nva,2,Isc1,Isc2,mdv2)
      call exchn1(Noa,Nva,Isc2,Isd1,Iad1,e)
      if(lengs(Isd1).NE.0)then
      call exp78(Noa,Nva,(Isd1+isbuc),Isc2)
      call mattrn(Noa,Noa,Nva,Nva,2,Isc2,Isc3,mdv2)
      call mattrn(Noa,Nva,Noa,Nva,1,Isc1,Isc2,mdv2)
      call exchn2(Noa,Nva,Isc3,Isc2,Isd5)
      endif
      if(Iopcl.EQ.0)then
      E2=E2+E2
      T=T+T
      goto 700
      endif
      endif
      
      if(lengs(Isd10).NE.0)then
      call expsym(Nobvb,(Isd10+isbuc),Isc1)
      call mattrn(Nob,Nvb,Nob,Nvb,2,Isc1,Isc2,mdv2)
      call exchn1(Nob,Nvb,Isc2,Isd3,Iad3,e(Nrorb+1))
      if(lengs(Isd3).NE.0)then
      call exp78(Nob,Nvb,(Isd3+isbuc),Isc2)
      call mattrn(Nob,Nob,Nvb,Nvb,2,Isc2,Isc3,mdv2)
      call mattrn(Nob,Nvb,Nob,Nvb,1,Isc1,Isc2,mdv2)
      call exchn2(Nob,Nvb,Isc3,Isc2,Isd10)
      endif
      endif
      
700   if(lengd(Isd2).NE.0)then
      call mattrn(Noa,Nva,Nob,Nvb,2,Isd2,Isc1,mdv2)
      call exchn3(Isc1,Iad2,e)
      endif
      
      if(lengd(Isd4).GT.0)call exchn4(Noa,(Isd4+isbuc),Isd4,Isc1,Isc2)
      
      if(Iopcl.NE.0.AND.lengd(Isd9).NE.0)call exchn4(Nob,(Isd9+isbuc),Is
     &d9,Isc1,Isc2)
      
      if(lengd(Isd11).GT.0)call exchn5(Noa,Nva,(Isd11+isbuc),Isd11,Isc1,
     &Isc2)
      
      if(lengd(Isd14).GT.0)call exchn5(Nob,Nvb,(Isd14+isbuc),Isd14,Isc1,
     &Isc2)
      
      if(lengd(Isd15).GT.0)call exchn6(Noa,Nva,(Isd15+isbuc),Isd15,Isc1,
     &Isc2)
      
      if(lengd(Isd18).GT.0)call exchn6(Nob,Nvb,(Isd18+isbuc),Isd18,Isc1,
     &Isc2)
      
      call tread(Igeno,ed,Ligen,1,Ligen,1,0)
      eump2=ed(Lehf)+(E2)
      ed(Lmp2)=eump2
      ed(Lenrgy)=eump2
      tsqp1=T+one
      anorm1=gsqrt(tsqp1)
      ign(Lisd)=0
      ed(Lanorm)=(anorm1)
      ed(La0)=one
      
      write(Iout,99004)anorm1
      write(Iout,99005)E2,eump2
      
      if(Iopcl.NE.0)then
      s20=(ed(Ls20))
      if(s20.LT.small)s20=zero
      if(gabs(S2).LT.small)S2=zero
      s21=s20+S2
      ed(Ls21)=ed(Ls20)+(S2)
      write(Iout,99003)s20,s21
      endif
      call twrite(Igeno,ed,Ligen,1,Ligen,1,0)
      
      ind=0
      do 800 ii=1,Nobuc
      i=Ibckt(ii)
      lengd(i)=0
      call tquery(i,lengd(i))
      if(lengd(i).NE.0)then
      ind=ind+1
      lengs(ind)=i
      lengi=lengd(i)
      call clean(i,lengi)
      endif
800   continue
      
      if(Iprint.GE.1)write(Iout,99007)(lengs(i),i=1,ind)
      
      do 900 i=1,18
      length=0
      call tquery((i+isbuc),length)
      if(length.GT.0)call fileio(5,(i+isbuc),0,0,0)
900   continue
      
      if(Idump.GT.1)call fdump
      return
      
      end
C* :1 * 
      
