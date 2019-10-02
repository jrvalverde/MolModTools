
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cortbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cortbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "cortbl.web"
      subroutine cortbl(IAT,ICORE,IECP)
      
      
      
      
      
      
      implicit none
      integer IAT,Iatcr,Iatno,Ichoos,ict,IECP,ii,Ino,Iprint,Ipseud,Iw3c,
     &Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab
      integer Iwtnao,Iwtnbo,Iznuc,jat,Jcore,Jprint,Kopt,l,Ll,LMAX,Lu,MAX
     &ATM,MAXBAS,Norbs
      parameter(LMAX=3)
      integer core(57),ICORE(4),iord(16)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      
      data iord/1,1,3,1,3,5,1,3,5,1,3,7,5,1,3,7/
      data core/2,0,8,1,1,8,2,2,1,12,2,3,2,6,3,3,2,1,12,3,4,3,1,6,3,4,3,
     &2,16,3,5,4,2,10,4,5,4,2,1,6,4,5,4,3,1,16,4,6,5,3,1,10,4,6,5,3,2/
      
      
      if((Jcore.NE.1.OR.Iatcr(IAT).LT.0).AND.Iatno(IAT).GT.0)then
      do 50 l=0,LMAX
      ICORE(l+1)=0
50    continue
      jat=Iatno(IAT)
      ii=0
100   ii=ii+1
      jat=jat-core(ii)
      ii=ii+1
      if(jat.LE.0)then
      do 120 l=1,core(ii)
      ICORE(l)=core(ii+l)
120   continue
      else
      ii=ii+core(ii)
      endif
      if(jat.GT.0)goto 100
      else
      
      
      do 150 l=0,LMAX
      ICORE(l+1)=0
150   continue
      ii=Iatcr(IAT)
      if(ii.GT.0)then
      ict=0
160   ict=ict+1
      l=iord(ict)/2
      ICORE(l+1)=ICORE(l+1)+1
      ii=ii-iord(ict)
      if(ii.GT.0)goto 160
      endif
      endif
      
      
      if(Ipseud.NE.0.AND.IECP.EQ.0)then
      ii=(Iatno(IAT)-Iznuc(IAT))/2
      if(ii.GT.0)then
      ict=0
180   ict=ict+1
      l=iord(ict)/2
      ICORE(l+1)=ICORE(l+1)-1
      ii=ii-iord(ict)
      if(ii.GT.0)goto 180
      endif
      endif
      return
      end
C* :1 * 
      
