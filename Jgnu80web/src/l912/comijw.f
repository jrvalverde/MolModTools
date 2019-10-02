
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 comijw"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "comijw.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "comijw.web"
      subroutine comijw(IBUC1,IBUC2,NDIM)
      implicit none
      integer i,IBUC1,IBUC2,Ieval,ij,ij1,index,Ioab,Ispect,j,leng,Loab,L
     &spect,Maxbuc,Mdv,NDIM,Noa,Noa2,Noa3,Noaob
      integer Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,Novaa,Novab,Novbb,Nr
     &orb,nsq,nsqa,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      double precision V
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(20000),Mdv
      equivalence(nsq,leng)
      
      
      
      
      
      call track('COMIJW')
      if(NDIM.EQ.0.OR.Noa.EQ.0)return
      
      nsq=NDIM**2
      nsqa=nsq*(Noa+1)
      call fileio(1,-IBUC2,0,0,0)
      leng=nsq
      ij1=0
      
      do 100 i=1,Noa
      ij=ij1
      do 50 j=i,Noa
      index=ij
      call fileio(2,-IBUC1,leng,V,index)
      call fileio(1,IBUC2,leng,V,0)
      ij=ij+nsq
50    continue
      ij1=ij1+nsqa
100   continue
      
      return
      
      end
C* :1 * 
      
