
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tstarb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tstarb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "tstarb.web"
      subroutine tstarb(IBUC1,IBUC2,CA,CB,S,INITW,NBASIS)
      implicit none
      double precision CA,CB,S,V
      integer i,IBUC1,IBUC2,Ieval,Ioab,Iopcl,Ispect,lbloc,leftb,leftwi,l
     &eftwo,leng,Loab,Lspect,Maxbuc,Mdv,mdv11,NBASIS,nbloc,nbsq
      integer Noa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,
     &Novaa,Novab,Novbb,nr,Nrorb,Nva,Nva2,Nva3,Nvavb
      integer Nvb,Nvb2,Nvb3,nwi,nwo
      dimension CA(*),CB(*),S(*)
      logical INITW
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(20000),Mdv
      common/constr/Iopcl
      
      
      
      
      
      
      
      call track('TSTARB')
      
      if(Noa.LE.0.OR.Nob.LE.0.OR.Nva.LE.0.OR.Nvb.LE.0)return
      
      nbsq=NBASIS**2
      leftb=Noaob
      if(Iopcl.EQ.0)leftb=Noa2
      leftwi=leftb*nbsq
      leftwo=leftb*Nvavb
      nbloc=Mdv/(Nvavb+nbsq)
      nwi=nbloc*nbsq
      nwo=nbloc*Nvavb
      nr=leftb/nbloc
      if(mod(leftb,nbloc).NE.0)nr=nr+1
      mdv11=nwi+1
      
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      
      do 100 i=1,nr
      leng=min0(leftwi,nwi)
      leftwi=leftwi-leng
      call fileio(2,IBUC1,leng,V,0)
      lbloc=min0(leftb,nbloc)
      leftb=leftb-lbloc
      leng=min0(leftwo,nwo)
      leftwo=leftwo-leng
      if(INITW)call fileio(2,IBUC2,leng,V(mdv11),0)
      call ctwc2(V(mdv11),CA,CB,V,lbloc,S,INITW,NBASIS)
      call fileio(1,IBUC2,leng,V(mdv11),0)
100   continue
      
      return
      
      end
C* :1 * 
      
