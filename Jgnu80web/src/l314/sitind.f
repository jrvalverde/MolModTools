
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sitind"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sitind.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "sitind.web"
      subroutine sitind
      implicit none
      integer i,Idmp,Idump,Is1,Is2,Is3,Istm,itemp1,itemp2,itemp3,jk,jkl,
     &jl,Js1,Js2,Js3,Jstm,kl,Ks1,Ks2
      integer Ks3,Kstm,lim,Lstm,Nfa,Nfb,Nfc,Nfd
      common/dump/Idmp,Idump
      common/site/Is1(10),Js1(10),Ks1(10),Is2(10),Js2(10),Ks2(10),Is3(10
     &),Js3(10),Ks3(10)
      common/nf/Nfa,Nfb,Nfc,Nfd,Istm,Jstm,Kstm,Lstm
      
      
      
      jk=Nfb*Nfc
      jl=Nfb*Nfd
      kl=Nfc*Nfd
      jkl=Nfb*Nfc*Nfd
      
      itemp1=0
      do 100 i=1,Nfa
      Is1(i)=itemp1
      Is2(i)=itemp1
      Is3(i)=itemp1
      itemp1=itemp1+jkl
100   continue
      
      itemp1=0
      itemp2=0
      itemp3=0
      lim=max0(Nfb,Nfc,Nfd)
      do 200 i=1,lim
      Js1(i)=itemp1
      Js2(i)=itemp2
      Js3(i)=itemp3
      itemp1=itemp1+kl
      itemp2=itemp2+jk
      itemp3=itemp3+jl
200   continue
      
      itemp1=0
      itemp2=0
      lim=max0(Nfc,Nfb)
      do 300 i=1,lim
      Ks1(i)=itemp1
      Ks2(i)=itemp2
      Ks3(i)=itemp1
      itemp1=itemp1+Nfd
      itemp2=itemp2+Nfc
300   continue
      
      return
      
      end
C* :1 * 
      
