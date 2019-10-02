
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 output"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "output.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "output.web"
      subroutine output(A,MR,MC,NR,NC)
      implicit none
      double precision A
      integer i,j,l,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,
     &Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa
      integer Lfnpr,MC,MR,NC,ncl,ncu,nloops,NR
      
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension A(MR,MC)
      
      
      ncl=1
      ncu=6
      nloops=NC/6+1
      do 100 l=1,nloops
      if(ncu.GT.NC)ncu=NC
      write(Lfnpr,99001)(j,j=ncl,ncu)
      do 50 i=1,NR
      write(Lfnpr,99002)i,(A(i,j),j=ncl,ncu)
50    continue
      if(ncu.GE.NC)return
      ncl=ncu+1
      ncu=ncu+6
100   continue
      return
99001 format(/11x,10(i3,9x))
99002 format(1x,i3,10F12.5)
      end
C* :1 * 
      
