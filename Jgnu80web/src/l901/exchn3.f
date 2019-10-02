
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 exchn3"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "exchn3.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "exchn3.web"
      subroutine exchn3(IBUC1,IBUC3,E)
      implicit none
      double precision a0,a1,Dv,E,E2,ei,eij,eija,eijab,S2,T,uaj,uib,V
      integer i1,ia,ib,ib1,IBUC1,IBUC3,Ieval,ii,ij,In,Ioab,Iopcl,Iout,Ip
     &rint,Ipunch,Ispect,iva,j1,leng,Loab
      integer Lspect,Maxbuc,Mdv,mdv2,mdv21,mdv3,mdv31,mm,Noa,Noa2,Noa3,N
     &oaob,Noava,Noavb,Nob,Nob2,Nob3,nobb,nobn,Nobva
      integer Nobvb,Novaa,Novab,Novbb,Nrorb,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2
     &,Nvb3
      dimension E(*)
      dimension Dv(20000)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(20000),Mdv
      common/result/T,E2,S2
      common/io/In,Iout,Ipunch
      common/constr/Iopcl
      common/print/Iprint
      equivalence(Dv(1),V(1))
      
      
      
      
      
      
      
      
      
99001 format(///' ALPHA - BETA OVERLAP MATRIX')
      
      call track('EXCHN3')
      
      if(Noa.LE.0.OR.Nob.LE.0.OR.Nva.LE.0.OR.Nvb.LE.0)return
      
      mdv2=Mdv/3
      mdv21=mdv2+1
      mdv3=mdv2+mdv2
      mdv31=mdv3+1
      nobb=Nob+Nrorb
      nobn=Nob*Nrorb+mdv2
      leng=Nvavb
      call fileio(2,-IBUC1,0,0,0)
      call fileio(1,-IBUC3,0,0,0)
      
      if(Iopcl.NE.0)then
      call tread(Ioab,V(mdv21),Loab,1,Loab,1,0)
      if(Iprint.GT.1)then
      write(Iout,99001)
      call matout(V(mdv21),Nrorb,Nrorb,Nrorb,Nrorb)
      endif
      endif
      
      do 100 ii=1,Noa
      ei=E(ii)
      i1=ii+nobn
      j1=mdv2
      do 50 ij=1,Nob
      call fileio(2,IBUC1,leng,V,0)
      eij=ei+E(ij+Nrorb)
      mm=0
      
      do 20 ia=1,Nva
      iva=ia+Noa
      eija=E(iva)-eij
      uaj=V(j1+iva)
      ib1=i1
      do 10 ib=1,Nvb
      eijab=E(ib+nobb)+eija
      mm=mm+1
      a0=V(mm)
      a1=a0/(eijab)
      T=T+a1**2
      E2=E2-a0*a1
      uib=V(ib1)
      S2=S2+a1*uaj*uib
      V(mm+mdv3)=-a1
      ib1=ib1+Nrorb
10    continue
20    continue
      call fileio(1,IBUC3,leng,V(mdv31),0)
      j1=j1+Nrorb
50    continue
100   continue
      S2=S2+S2
      
      return
      
      end
C* :1 * 
      
