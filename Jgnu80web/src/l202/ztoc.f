
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ztoc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ztoc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 95 "ztoc.web"
      subroutine ztoc(MAXNZ,NZ,IANZ,IZ,BL,ALPH,BET,TTEST,NATOMS,IAN,C,CZ
     &,A,B,D,ALPHA,BETA,IOUT,ERROR,CHARGE)
      implicit none
      double precision A,ALPH,ALPHA,arg,B,BET,BETA,BL,C,CHARGE,CZ,D,dcaj
     &,dcbj,denom,dsaj,dsbj,f180,four,gabs
      double precision gacos,gamma,gatan,gcos,gfloat,gsign,gsin,gsqrt,on
     &e,pi,r,tenm10,tenm5,tenm6,tetang,tetdat,tettol,tettst,three,toldat
      double precision torad,two,u1,u2,u3,u4,v3,v3mag,vj,vp,x,zero,zeta
      integer i,iaind,IAN,IANZ,ind3,IOUT,itemp,IZ,j,jnd3,jtemp,k,MAXNZ,n
     &aind,nat3,NATOMS,numtet,NZ,nz3
      logical test,TTEST,ERROR,vecerr
      dimension IANZ(*),IZ(MAXNZ,4),BL(3),ALPHA(*),BETA(*),IAN(*),C(*),C
     &Z(9),A(*),B(*),D(*),ALPH(3),BET(*),CHARGE(*)
      dimension u1(3),u2(3),u3(3),u4(3),vj(3),vp(3),v3(3)
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/
      data tenm5/1.0D-5/,tenm6/1.0D-6/
      data f180/180.0D0/,four/4.0D0/
      data tenm10/1.0D-10/
      data tetdat/109.471D0/,toldat/0.001D0/,three/3.D0/
      
      
99001 format(1x,i7,' Z-MATRIX CARDS IS GREATER THAN THE MAXIMUM OF ',i4,
     &' IN SUBROUTINE ZTOC')
99002 format(1x,'ERROR ON Z-MATRIX CARD NUMBER ',i4/1x,'INVALID BETA ANG
     &LE TYPE (Z4)')
99003 format(1x,'ERROR ON Z-MATRIX CARD NUMBER ',i4/1x,'REFERENCE MADE T
     &O AN UNDEFINED CENTER')
99004 format(1x,'ERROR ON Z-MATRIX CARD NUMBER ',i4/1x,'MULTIPLE REFEREN
     &CES TO A CENTER ON THE SAME CARD')
99005 format(1x,'ERROR ON Z-MATRIX CARD NUMBER ',i4/1x,'INCIPIENT FLOATI
     &NG POINT ERROR DETECTED')
99006 format(1x,'ERROR ON Z-MATRIX CARD NUMBER ',i4/1x,'ANGLE ALPHA IS O
     &UTSIDE THE VALID RANGE OF 0 TO 180')
99007 format(1x,'ERROR ON Z-MATRIX CARD NUMBER ',i4/1x,'NEGATIVE BOND LE
     &NGTH')
99008 format(1x,'ERROR ON Z-MATRIX CARD NUMBER ',i4/1x,'ANGLE BETA IS OU
     &TSIDE THE VALID RANGE OF 0 TO 180')
99009 format(1x,i3,' TETRAHEDRAL ANGLES REPLACED')
      
      test(x)=gabs(x-tettst).LT.tettol
      
      
      
      
      
      if(NZ.LE.MAXNZ)then
      
      
      
      ERROR=.FALSE.
      if(NZ.GE.2)then
      do 20 i=2,NZ
      if(i.GT.3)then
      if(iabs(IZ(i,4)).GT.1)then
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99002)i
      endif
      endif
      if(IZ(i,1).GE.i.OR.IZ(i,2).GE.i.OR.IZ(i,3).GE.i)then
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99003)i
      endif
      if(i.GT.2)then
      if(i.GT.3)then
      
      if(IZ(i,1).EQ.IZ(i,2).OR.IZ(i,1).EQ.IZ(i,3).OR.IZ(i,2).EQ.IZ(i,3))
     &then
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99004)i
      endif
      elseif(IZ(i,1).EQ.IZ(i,2))then
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99004)i
      endif
      endif
20    continue
      if(ERROR)return
      endif
      pi=four*gatan(one)
      torad=pi/f180
      
      tetang=gacos(-one/three)
      tettst=tetdat*torad
      tettol=toldat*torad
      nz3=3*NZ
      call aclear(nz3,CZ)
      
      
      numtet=0
      do 50 i=1,NZ
      ALPHA(i)=ALPH(i)
      BETA(i)=BET(i)
      if(BL(i).LE.zero.AND.i.NE.1)then
      ERROR=.TRUE.
      write(IOUT,99007)i
      endif
      if(.NOT.(i.LE.2.OR.(ALPHA(i).GE.zero.AND.ALPHA(i).LE.pi)))then
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99006)i
      endif
      if(TTEST)then
      if(test(ALPHA(i)))then
      ALPHA(i)=tetang
      ALPH(i)=tetang
      numtet=numtet+1
      endif
      if(test(BETA(i)))then
      BETA(i)=tetang
      BET(i)=tetang
      numtet=numtet+1
      endif
      if(IZ(i,4).NE.0.AND.i.GT.3)then
      if(BETA(i).LT.zero.OR.BETA(i).GT.pi)then
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99008)i
      endif
      endif
      endif
50    continue
      if((numtet.NE.0).AND.(IOUT.NE.0))write(IOUT,99009)numtet
      if(ERROR)return
      
      CZ(6)=BL(2)
      if(NZ.GE.3)then
      CZ(7)=BL(3)*gsin(ALPHA(3))
      if(IZ(3,1).NE.1)then
      CZ(9)=CZ(6)-BL(3)*gcos(ALPHA(3))
      else
      CZ(9)=BL(3)*gcos(ALPHA(3))
      endif
      if(NZ.GE.4)then
      do 60 i=4,NZ
      ind3=(i-1)*3
      if(gabs(CZ(1+ind3-3)).GE.tenm5)goto 70
      CZ(1+ind3)=BL(i)*gsin(ALPHA(i))
      itemp=(IZ(i,1)-1)*3
      jtemp=(IZ(i,2)-1)*3
      CZ(3+ind3)=CZ(3+itemp)-BL(i)*gcos(ALPHA(i))*gsign(one,CZ(3+itemp)-
     &CZ(3+jtemp))
60    continue
70    k=i
      if(k.LE.NZ)then
      do 80 j=k,NZ
      jnd3=(j-1)*3
      dcaj=gcos(ALPHA(j))
      dsaj=gsin(ALPHA(j))
      dcbj=gcos(BETA(j))
      dsbj=gsin(BETA(j))
      if(IZ(j,4).EQ.0)then
      call vec(tenm6,vecerr,u1,CZ,IZ(j,2),IZ(j,3))
      if(.NOT.vecerr)then
      
      call vec(tenm6,vecerr,u2,CZ,IZ(j,1),IZ(j,2))
      if(.NOT.vecerr)then
      
      call vprod(vp,u1,u2)
      arg=one-(u1(1)*u2(1)+u1(2)*u2(2)+u1(3)*u2(3))**2
      if(arg.GE.zero)then
      
      r=gsqrt(arg)
      if(r.GE.tenm6)then
      
      do 72 i=1,3
      u3(i)=vp(i)/r
72    continue
      call vprod(u4,u3,u2)
      do 74 i=1,3
      vj(i)=BL(j)*(-u2(i)*dcaj+u4(i)*dsaj*dcbj+u3(i)*dsaj*dsbj)
      itemp=(IZ(j,1)-1)*3
      CZ(i+jnd3)=vj(i)+CZ(i+itemp)
74    continue
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      
      elseif(iabs(IZ(j,4)).NE.1)then
      
      call vec(tenm6,vecerr,u1,CZ,IZ(j,1),IZ(j,3))
      if(.NOT.vecerr)then
      
      call vec(tenm6,vecerr,u2,CZ,IZ(j,2),IZ(j,1))
      if(.NOT.vecerr)then
      
      zeta=-(u1(1)*u2(1)+u1(2)*u2(2)+u1(3)*u2(3))
      call vprod(v3,u1,u2)
      v3mag=gsqrt(v3(1)*v3(1)+v3(2)*v3(2)+v3(3)*v3(3))
      denom=one-zeta**2
      if(gabs(denom).GT.tenm6)then
      
      A(j)=v3mag*dcbj/denom
      arg=(one-dcaj*dcaj-A(j)*dcbj*v3mag)/denom
      if(arg.GE.zero)then
      
      B(j)=gsqrt(arg)
      if(IZ(j,4).NE.2)B(j)=-B(j)
      D(j)=B(j)*zeta+dcaj
      do 76 i=1,3
      u3(i)=B(j)*u1(i)+D(j)*u2(i)+A(j)*v3(i)
      vj(i)=BL(j)*u3(i)
      itemp=(IZ(j,1)-1)*3
      CZ(i+jnd3)=vj(i)+CZ(i+itemp)
76    continue
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      call vec(tenm6,vecerr,u1,CZ,IZ(j,1),IZ(j,3))
      if(.NOT.vecerr)then
      
      call vec(tenm6,vecerr,u2,CZ,IZ(j,2),IZ(j,1))
      if(.NOT.vecerr)then
      
      zeta=-(u1(1)*u2(1)+u1(2)*u2(2)+u1(3)*u2(3))
      denom=one-zeta**2
      if(gabs(denom).GE.tenm6)then
      
      A(j)=(-dcbj+zeta*dcaj)/denom
      B(j)=(dcaj-zeta*dcbj)/denom
      r=zero
      gamma=pi/two
      if(gabs(zeta).GE.tenm6)then
      if(zeta.LT.zero)r=pi
      if(denom.GE.zero)then
      
      gamma=gatan(gsqrt(denom)/zeta)+r
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      endif
      D(j)=zero
      if(gabs(gamma+ALPHA(j)+BETA(j)-two*pi).GE.tenm6)then
      arg=(one+A(j)*dcbj-B(j)*dcaj)/denom
      if(arg.GE.zero)then
      
      D(j)=gfloat(IZ(j,4))*gsqrt(arg)
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      endif
      call vprod(v3,u1,u2)
      do 78 i=1,3
      u3(i)=A(j)*u1(i)+B(j)*u2(i)+D(j)*v3(i)
      vj(i)=BL(j)*u3(i)
      itemp=(IZ(j,1)-1)*3
      CZ(i+jnd3)=vj(i)+CZ(i+itemp)
78    continue
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      else
      ERROR=.TRUE.
      if(IOUT.NE.0)write(IOUT,99005)i
      return
      endif
      endif
80    continue
      endif
      endif
      endif
      else
      ERROR=.TRUE.
      write(IOUT,99001)NZ,MAXNZ
      return
      endif
      
      NATOMS=0
      iaind=0
      naind=0
      do 100 i=1,NZ
      if(IANZ(i).GE.0)then
      NATOMS=NATOMS+1
      IAN(NATOMS)=IANZ(i)
      CHARGE(NATOMS)=CHARGE(i)
      C(1+naind)=CZ(1+iaind)
      C(2+naind)=CZ(2+iaind)
      C(3+naind)=CZ(3+iaind)
      naind=naind+3
      endif
      iaind=iaind+3
100   continue
      
      nat3=3*NATOMS
      do 200 i=1,nat3
      if(gabs(C(i)).LE.tenm10)C(i)=zero
200   continue
      
      return
      
      end
C* :1 * 
      
