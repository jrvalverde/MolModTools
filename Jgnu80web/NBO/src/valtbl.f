
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 valtbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "valtbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "valtbl.web"
      subroutine valtbl(IAT,IVAL)
      
      
      
      implicit none
      integer IAT,Iatcr,Iatno,Ichoos,icore,ict,iecp,ii,Ino,iord,Iprint,I
     &pseud,IVAL,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs
      integer Iwmulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Iznuc,Jcore,Jprint,Kopt
     &,l,Ll,LMAX,Lu,MAXATM,MAXBAS,Norbs
      parameter(LMAX=3)
      dimension IVAL(4),icore(4),iord(16)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      
      data iord/1,1,3,1,3,5,1,3,5,1,3,7,5,1,3,7/
      
      
      do 100 l=0,LMAX
      IVAL(l+1)=0
100   continue
      
      
      ii=Iatno(IAT)
      if(ii.GT.0)then
      ict=0
150   ict=ict+1
      l=iord(ict)/2
      IVAL(l+1)=IVAL(l+1)+1
      ii=ii-2*iord(ict)
      if(ii.GT.0)goto 150
      endif
      
      
      iecp=1
      call cortbl(IAT,icore,iecp)
      do 200 l=0,LMAX
      IVAL(l+1)=IVAL(l+1)-icore(l+1)
200   continue
      iecp=0
      call cortbl(IAT,icore,iecp)
      do 300 l=0,LMAX
      if(icore(l+1).LT.0)IVAL(l+1)=IVAL(l+1)+icore(l+1)
300   continue
      return
      end
C* :1 * 
      
