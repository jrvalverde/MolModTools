
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tstc3"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tstc3.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "tstc3.web"
      subroutine tstc3(MAXAP3,A,B,NATOMS,ATMCHG,IAT,JAT,KAT,CENTR,ITST)
      implicit none
      double precision A,alpha,ATMCHG,B,beta,CENTR,dij,dik,djk,eight,fac
     &t3,gabs,gamma,gatan,half,one,phi3,px,py,pz
      double precision t,theta3,three,Tol2,Toler,two
      integer IAT,ITST,JAT,KAT,MAXAP3,NATOMS,numatm
      dimension A(MAXAP3,3),CENTR(3)
      dimension t(3,3),B(*),ATMCHG(*)
      common/tol/Toler,Tol2
      data half,one,two,three,eight/0.5D0,1.0D0,2.0D0,3.0D0,8.0D0/
      
      
      
      
      
      
      
      numatm=NATOMS+3
      ITST=0
      phi3=(eight/three)*gatan(one)
      theta3=half*phi3
      fact3=two/three
      
      
      call triang(MAXAP3,A,IAT,JAT,KAT,alpha,beta,gamma,dij,dik,djk)
      
      
      if(gabs(alpha-theta3).GT.Toler.OR.gabs(dij-dik).GT.Toler)return
      px=half*(A(JAT,1)+A(KAT,1))
      py=half*(A(JAT,2)+A(KAT,2))
      pz=half*(A(JAT,3)+A(KAT,3))
      CENTR(1)=A(IAT,1)+fact3*(px-A(IAT,1))
      CENTR(2)=A(IAT,2)+fact3*(py-A(IAT,2))
      CENTR(3)=A(IAT,3)+fact3*(pz-A(IAT,3))
      call put(MAXAP3,A,B,t,CENTR,numatm,3)
      call rotate(MAXAP3,A,B,NATOMS,t,3,phi3)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,ITST)
      return
      
      end
C* :1 * 
      
