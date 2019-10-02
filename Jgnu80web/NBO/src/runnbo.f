
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 runnbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "runnbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "runnbo.web"
      subroutine runnbo(CORE,MEMORY,IOP,ICONTR)
      implicit none
      double precision CORE
      integer i,ICONTR,idone,IOP,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,
     &Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnp
     &nl
      integer Lfnppa,Lfnpr,MEMORY,nboopt
      dimension CORE(*),IOP(50)
      dimension nboopt(10)
      
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      Lfnin=5
      Lfnpr=6
      
      
      do 100 i=1,9
      nboopt(i)=IOP(i+39)
100   continue
      nboopt(10)=82
      
      
      ICONTR=0
      if(abs(nboopt(1)).LT.2)then
      call charpn(4HNBO )
      call nbo(CORE,MEMORY,nboopt)
      
      
      elseif(nboopt(1).EQ.2)then
      call charpn(4HDELE)
      call nboean(CORE,MEMORY,nboopt,idone)
      if(idone.NE.0)ICONTR=1
      if(idone.EQ.0)call delscf(CORE,CORE,nboopt)
      elseif(nboopt(1).EQ.3)then
      call charpn(4HEDEL)
      call delscf(CORE,CORE,nboopt)
      call nboean(CORE,MEMORY,nboopt,idone)
      endif
      
      return
      end
C* :1 * 
      
