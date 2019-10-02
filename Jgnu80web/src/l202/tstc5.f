
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tstc5"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tstc5.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "tstc5.web"
      subroutine tstc5(MAXAP3,A,B,NATOMS,ATMCHG,IAT,JAT,KAT,CENTR,ITST)
      implicit none
      double precision A,alpha,ATMCHG,B,beta,CENTR,dij,dik,djk,fact5,gab
     &s,gamma,gatan,gsin,half,one,onept6,phi5,piovr4,pt8
      double precision px,py,pz,t,theta5,Tol2,Toler,two,twopt4
      integer IAT,ITST,JAT,KAT,MAXAP3,NATOMS,numatm
      dimension A(MAXAP3,3),CENTR(3)
      dimension t(3,3),B(*),ATMCHG(*)
      common/tol/Toler,Tol2
      data half,pt8,one,onept6/0.5D0,0.8D0,1.0D0,1.6D0/
      data two,twopt4/2.0D0,2.4D0/
      
      
      
      
      
      
      
      numatm=NATOMS+3
      ITST=0
      piovr4=gatan(one)
      phi5=onept6*piovr4
      theta5=twopt4*piovr4
      fact5=one/(two*gsin(pt8*piovr4)**2)
      
      
      call triang(MAXAP3,A,IAT,JAT,KAT,alpha,beta,gamma,dij,dik,djk)
      
      
      if(gabs(alpha-theta5).LE.Toler.AND.gabs(dij-dik).LE.Toler)then
      px=half*(A(JAT,1)+A(KAT,1))
      py=half*(A(JAT,2)+A(KAT,2))
      pz=half*(A(JAT,3)+A(KAT,3))
      CENTR(1)=A(IAT,1)+fact5*(px-A(IAT,1))
      CENTR(2)=A(IAT,2)+fact5*(py-A(IAT,2))
      CENTR(3)=A(IAT,3)+fact5*(pz-A(IAT,3))
      
      elseif(gabs(beta-theta5).GT.Toler.OR.gabs(dij-djk).GT.Toler)then
      
      if(gabs(gamma-theta5).GT.Toler.OR.gabs(dik-djk).GT.Toler)return
      px=half*(A(IAT,1)+A(JAT,1))
      py=half*(A(IAT,2)+A(JAT,2))
      pz=half*(A(IAT,3)+A(JAT,3))
      CENTR(1)=A(KAT,1)+fact5*(px-A(KAT,1))
      CENTR(2)=A(KAT,2)+fact5*(py-A(KAT,2))
      CENTR(3)=A(KAT,3)+fact5*(pz-A(KAT,3))
      else
      px=half*(A(IAT,1)+A(KAT,1))
      py=half*(A(IAT,2)+A(KAT,2))
      pz=half*(A(IAT,3)+A(KAT,3))
      CENTR(1)=A(JAT,1)+fact5*(px-A(JAT,1))
      CENTR(2)=A(JAT,2)+fact5*(py-A(JAT,2))
      CENTR(3)=A(JAT,3)+fact5*(pz-A(JAT,3))
      endif
      
      call put(MAXAP3,A,B,t,CENTR,numatm,3)
      call rotate(MAXAP3,A,B,NATOMS,t,3,phi5)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,ITST)
      return
      
      end
C* :1 * 
      
