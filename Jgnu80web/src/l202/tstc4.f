
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tstc4"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tstc4.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "tstc4.web"
      subroutine tstc4(MAXAP3,A,B,NATOMS,ATMCHG,IAT,JAT,KAT,CENTR,ITST)
      implicit none
      double precision A,alpha,ATMCHG,B,beta,CENTR,dij,dik,djk,gabs,gamm
     &a,gatan,half,halfpi,one,t,Tol2,Toler,two
      integer IAT,ITST,JAT,KAT,MAXAP3,NATOMS,numatm
      dimension A(MAXAP3,3),CENTR(3)
      dimension t(3,3),B(*),ATMCHG(*)
      common/tol/Toler,Tol2
      data half,one,two/0.5D0,1.0D0,2.0D0/
      
      
      
      
      
      
      
      numatm=NATOMS+3
      ITST=0
      halfpi=two*gatan(one)
      
      
      call triang(MAXAP3,A,IAT,JAT,KAT,alpha,beta,gamma,dij,dik,djk)
      
      
      if(gabs(alpha-halfpi).LE.Toler.AND.gabs(dij-dik).LE.Toler)then
      CENTR(1)=half*(A(JAT,1)+A(KAT,1))
      CENTR(2)=half*(A(JAT,2)+A(KAT,2))
      CENTR(3)=half*(A(JAT,3)+A(KAT,3))
      
      elseif(gabs(beta-halfpi).GT.Toler.OR.gabs(dij-djk).GT.Toler)then
      
      if(gabs(gamma-halfpi).GT.Toler.OR.gabs(dik-djk).GT.Toler)return
      CENTR(1)=half*(A(IAT,1)+A(JAT,1))
      CENTR(2)=half*(A(IAT,2)+A(JAT,2))
      CENTR(3)=half*(A(IAT,3)+A(JAT,3))
      else
      CENTR(1)=half*(A(IAT,1)+A(KAT,1))
      CENTR(2)=half*(A(IAT,2)+A(KAT,2))
      CENTR(3)=half*(A(IAT,3)+A(KAT,3))
      endif
      
      call put(MAXAP3,A,B,t,CENTR,numatm,3)
      call rotate(MAXAP3,A,B,NATOMS,t,3,halfpi)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,ITST)
      return
      
      end
C* :1 * 
      
