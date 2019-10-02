
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 deriv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "deriv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "deriv.web"
      subroutine deriv(TOANG)
      implicit none
      double precision Alpha,Convrg,D1var,D1vold,D2var,Delvar,dinc,F,F1,
     &Fminus,Fplus,Fzero,gsqrt,H,one,Pool0,Pool1,sign,Tcurcy,Telcur
      double precision Teltot,Tlstcy,Tmax,TOANG,two,Vname,Xi,Yold,zero
      integer i,iblank,icur,Idone,Iflinf,Ihflag,In,Incldh,Index,Intent,I
     &op,Iout,Ipunch,Iret,Isect,Istats,istr,Ititle,itmp,Itype
      integer j,Jump,K,Lambda,len,Mode,modulo,ncur,Ncyc,Ncycls,Ncytot,No
     &inch,Noruns,Npar,Nstep,Nvar
      integer getchr
      dimension itmp(10),istr(10)
      common/j102/Jump,Iret
      common/io/In,Iout,Ipunch
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      common/iop/Iop(50)
      equivalence(Fplus,F1(1))
      equivalence(Fminus,F1(2))
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/
      data iblank/1H /
      
      
      
      
      
      
99001 format(1x,1x,'VARIABLE',4x,' FIRST DERIVATIVE',13x,'SECOND DERIVAT
     &IVE')
99002 format(1x,1x,10A1,g20.10,10x,g20.10)
99003 format(1x,'IN DERIV, VARIABLE ',i3,' INCREMENTED:  WAS ',d20.10,',
     &  STEPPED BY ',d10.3,',  IS NOW ',d20.10,'.')
      
      if(Intent.NE.0)goto 200
      Nstep=0
      Index=1
      sign=one
100   Pool0(Index)=Pool1(Index)+Delvar(Index)*sign
      dinc=Delvar(Index)*sign
      if(Iop(34).NE.0.OR.Iop(33).NE.0)write(Iout,99003)Index,Pool1(Index
     &),dinc,Pool0(Index)
      Nstep=Nstep+Mode+1
      
      call value
      if(Iret.GT.0)goto 300
      
200   modulo=mod(Nstep,2)
      if(modulo.NE.0)then
      Fplus=F
      sign=-one
      goto 100
      else
      
      if(Mode.NE.0)then
      
      Fplus=F
      D1vold(Index)=D1var(Index)
      D1var(Index)=(Fplus-Fzero)/Delvar(Index)-D2var(Index)*Delvar(Index
     &)/two
      else
      Fminus=F
      if(Isect.NE.3)D1vold(Index)=D1var(Index)*gsqrt(D2var(Index))
      D1var(Index)=(Fplus-Fminus)/(Delvar(Index)+Delvar(Index))
      D2var(Index)=(Fplus+Fminus-(Fzero+Fzero))/(Delvar(Index)*Delvar(In
     &dex))
      if(Isect.NE.3)D1vold(Index)=D1vold(Index)/gsqrt(D2var(Index))
      if(D2var(Index).LE.zero)call fperr(-1)
      endif
      sign=one
      Pool0(Index)=Pool1(Index)
      Index=Index+1
      if(Index.LE.Nvar)goto 100
      write(Iout,99001)
      ncur=0
      do 250 i=1,Nvar
      call getb(2,itmp,len,Vname,ncur)
      do 220 j=1,10
      istr(j)=iblank
220   continue
      len=min0(10,len)
      icur=0
      do 240 j=1,len
      istr(j)=getchr(itmp,icur)
240   continue
      write(Iout,99002)(istr(j),j=1,10),D1var(i),D2var(i)
250   continue
      call contt(TOANG)
      endif
      
300   return
      
      end
C* :1 * 
      
