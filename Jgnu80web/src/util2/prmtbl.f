
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prmtbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prmtbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "prmtbl.web"
      subroutine prmtbl(IHDG,XNAME,X,ITYPE,DX,NVAR,LBL,NZ,TOANG)
      implicit none
      double precision conver,DX,f45,gabs,gatan,hundrd,one,str,ten,TOANG
     &,todeg,value,X,XNAME,zero
      integer i,IHDG,ii,In,Iout,Ipunch,itmp,ITYPE,j,l,LBL,len,line,ncur,
     &nrep,numrep,NVAR,NZ
      integer getchr
      dimension XNAME(2),X(2),ITYPE(2),DX(2),itmp(8)
      dimension str(5),line(20),LBL(*)
      common/io/In,Iout,Ipunch
      data zero/0.0D0/,ten/10.0D0/,hundrd/100.0D0/
      data one/1.0D0/,f45/45.0D0/
      
      
      
99001 format(1x,72('-'))
99002 format(1x,22x,27(1H-)/1x,22x,'!   INITIAL PARAMETERS    !'/1x,22x,
     &'! (ANGSTROMS AND DEGREES) !'/1x,22(1H-),27x,23(1H-))
99003 format(1x,22x,27(1H-)/1x,22x,'!  OPTIMIZED PARAMETERS   !'/1x,22x,
     &'! (ANGSTROMS AND DEGREES) !'/1x,22(1H-),27x,23(1H-))
99004 format(1x,22x,28(1H-)/1x,22x,'! NON-OPTIMIZED PARAMETERS !'/1x,22x
     &,'! (ANGSTROMS AND DEGREES)  !'/1x,22(1H-),28x,22(1H-))
99005 format(1x,'!',6x,'NAME',6x,4x,'VALUE',3x,'DERIVATIVE INFORMATION (
     &ATOMIC UNITS)',5x,'!')
      
      
      
      todeg=f45/gatan(one)
      
      if(IHDG.EQ.0)write(Iout,99002)
      if(IHDG.EQ.1)write(Iout,99003)
      if(IHDG.EQ.2)write(Iout,99004)
      write(Iout,99005)
      write(Iout,99001)
      
      ncur=0
      do 100 i=1,NVAR
      
      l=0
      call pad(line,l,72,1H )
      l=0
      call putchr('!',line,l)
      l=71
      call putchr('!',line,l)
      l=0
      
      call getb(2,str,len,XNAME,ncur)
      
      len=min0(16,len)
      l=8-len/2+1
      call putb(str,len,line,l)
      
      conver=todeg
      numrep=nrep(i,LBL,NZ)
      if(numrep.NE.0)conver=TOANG
      value=X(i)*conver
      
      l=17
      if(value.GE.zero)l=l+1
      if(gabs(value).LT.hundrd)l=l+1
      if(gabs(value).LT.ten)l=l+1
      call putfp(value,4,line,l)
      l=l-1
      call putchr(' ',line,l)
      
      l=29
      j=iabs(ITYPE(i))
      if(ITYPE(i).EQ.-1)j=4
      
      if(j.EQ.0)call putbc('ESTIMATE D2E/DX2',16,line,l)
      if(j.EQ.1)call putbc('D2E/DX2 =',9,line,l)
      if(j.EQ.2)call putbc('CALC D2E/DX2,  STEPSIZE =',25,line,l)
      if(j.EQ.3)call putbc('CALC D2E/DXDY, STEPSIZE =',25,line,l)
      if(j.EQ.4)call putbc('CALCULATE D2E/DX2 ANALYTICALLY',30,line,l)
      if(j.EQ.5)call putbc('D2E/DX2 = IDENTITY',18,line,l)
      if(j.EQ.97)call putbc('D2E/DX2 =',9,line,l)
      if(j.EQ.98)call putbc('DE/DX =',7,line,l)
      if(j.EQ.99)call putbc('-DE/DX =',8,line,l)
      
      if(j.NE.0.AND.j.NE.4.AND.j.NE.5)then
      if(j.NE.0)call putchr(' ',line,l)
      ii=i+(i-1)*NVAR
      if(j.EQ.99.OR.j.EQ.98.OR.j.EQ.97)ii=i
      if(DX(ii).GE.zero)l=l+1
      if(gabs(DX(ii)).LT.hundrd)l=l+1
      if(gabs(DX(ii)).LT.ten)l=l+1
      if(j.NE.0)call putfp(DX(ii),6,line,l)
      if(j.NE.0)l=l-1
      if(j.NE.0)call putchr(' ',line,l)
      endif
      call strout(Iout,line,72,1)
100   continue
      write(Iout,99001)
      return
      
      end
C* :1 * 
      
