
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tors"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tors.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "tors.web"
      subroutine tors(NOINT,I,J,K,L,B,IB,C,NPARM)
      implicit none
      double precision B,C,cr1,cr2,dij,dijsq,djk,djksq,dkl,dklsq,dotpj,d
     &otpk,eij,ejk,ekl,f1,f2,gsqrt,one,rij
      double precision rjk,rkl,sinpj,sinpk,smi,smj,sml,zero
      integer I,iaind,IB,J,jaind,K,kaind,L,laind,m,NOINT,NPARM
      dimension B(3,4,NPARM),IB(4,NPARM),C(*)
      dimension rij(3),rjk(3),rkl(3),eij(3),ejk(3),ekl(3),cr1(3),cr2(3)
      data zero/0.D0/,one/1.D0/
      
      
      
      
      
      
      
      iaind=3*(I-1)
      jaind=3*(J-1)
      kaind=3*(K-1)
      laind=3*(L-1)
      IB(1,NOINT)=I
      IB(2,NOINT)=J
      IB(3,NOINT)=K
      IB(4,NOINT)=L
      dijsq=zero
      djksq=zero
      dklsq=zero
      do 100 m=1,3
      rij(m)=C(m+jaind)-C(m+iaind)
      dijsq=dijsq+rij(m)**2
      rjk(m)=C(m+kaind)-C(m+jaind)
      djksq=djksq+rjk(m)**2
      rkl(m)=C(m+laind)-C(m+kaind)
      dklsq=dklsq+rkl(m)**2
100   continue
      dij=gsqrt(dijsq)
      djk=gsqrt(djksq)
      dkl=gsqrt(dklsq)
      do 200 m=1,3
      eij(m)=rij(m)/dij
      ejk(m)=rjk(m)/djk
      ekl(m)=rkl(m)/dkl
200   continue
      cr1(1)=eij(2)*ejk(3)-eij(3)*ejk(2)
      cr1(2)=eij(3)*ejk(1)-eij(1)*ejk(3)
      cr1(3)=eij(1)*ejk(2)-eij(2)*ejk(1)
      cr2(1)=ejk(2)*ekl(3)-ejk(3)*ekl(2)
      cr2(2)=ejk(3)*ekl(1)-ejk(1)*ekl(3)
      cr2(3)=ejk(1)*ekl(2)-ejk(2)*ekl(1)
      dotpj=-(eij(1)*ejk(1)+eij(2)*ejk(2)+eij(3)*ejk(3))
      dotpk=-(ejk(1)*ekl(1)+ejk(2)*ekl(2)+ejk(3)*ekl(3))
      sinpj=gsqrt(one-dotpj**2)
      sinpk=gsqrt(one-dotpk**2)
      do 300 m=1,3
      smi=-cr1(m)/(dij*sinpj*sinpj)
      B(m,1,NOINT)=smi
      f1=(cr1(m)*(djk-dij*dotpj))/(djk*dij*sinpj*sinpj)
      f2=(dotpk*cr2(m))/(djk*sinpk*sinpk)
      smj=f1-f2
      B(m,2,NOINT)=smj
      sml=cr2(m)/(dkl*sinpk*sinpk)
      B(m,4,NOINT)=sml
      B(m,3,NOINT)=(-smi-smj-sml)
300   continue
      
      return
      
      end
C* :1 * 
      
