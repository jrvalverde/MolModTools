
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 put"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "put.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "put.web"
      subroutine put(MAXAP3,A,B,T,V,NATOMS,IXYZ)
      implicit none
      double precision A,alph,B,beta,gabs,gamm,gsqrt,one,T,t1,Tol2,Toler
     &,V,v1,v2,v2233,v2v2,v3,v3v3,vnorm
      integer i1,i2,i3,IXYZ,MAXAP3,NATOMS
      dimension V(3),T(3,3)
      dimension t1(3,3),A(*),B(*)
      common/tol/Toler,Tol2
      data one/1.0D0/
      
      
      
      
      
      
      
      
      i1=IXYZ
      i2=1+mod(i1,3)
      i3=1+mod(i2,3)
      v1=V(i1)
      v2=V(i2)
      v3=V(i3)
      vnorm=gsqrt(v1*v1+v2*v2+v3*v3)
      if(gabs(gabs(v1)-vnorm).LT.Toler)return
      
      
      alph=v1/vnorm
      beta=v2/vnorm
      gamm=v3/vnorm
      v2v2=v2*v2
      v3v3=v3*v3
      v2233=one/(v2v2+v3v3)
      
      
      t1(1,1)=alph
      t1(1,2)=beta
      t1(1,3)=gamm
      t1(2,1)=-t1(1,2)
      t1(3,1)=-t1(1,3)
      t1(2,3)=v2*v3*(alph-one)*v2233
      t1(3,2)=t1(2,3)
      t1(2,2)=(v2v2*alph+v3v3)*v2233
      t1(3,3)=(v3v3*alph+v2v2)*v2233
      
      
      T(i1,i1)=t1(1,1)
      T(i1,i2)=t1(1,2)
      T(i1,i3)=t1(1,3)
      T(i2,i1)=t1(2,1)
      T(i2,i2)=t1(2,2)
      T(i2,i3)=t1(2,3)
      T(i3,i1)=t1(3,1)
      T(i3,i2)=t1(3,2)
      T(i3,i3)=t1(3,3)
      
      
      call tform(MAXAP3,T,A,B,NATOMS)
      call move(MAXAP3,B,A,NATOMS)
      return
      
      end
C* :1 * 
      
