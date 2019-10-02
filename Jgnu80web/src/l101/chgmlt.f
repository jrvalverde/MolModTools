
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 chgmlt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "chgmlt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "chgmlt.web"
      subroutine chgmlt(ICHARG,MULTIP)
      implicit none
      double precision Alpha,Beta,Bl
      integer i,Ianz,ICHARG,In,Iout,Ipunch,isum,Iz,Lalpha,Lbeta,Lbl,MULT
     &IP,Nvar,Nz
      common/io/In,Iout,Ipunch
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvar
      
      isum=0
      do 100 i=1,Nz
      if(Ianz(i).GT.0)isum=isum+Ianz(i)
100   continue
      
      isum=isum-ICHARG
      
      if(mod(isum,2).NE.mod(MULTIP,2))return
      
      write(Iout,99001)
      
99001 format('  THE SPECIFIED CHARGE AND MULTIPLICITY ARE IMPOSSIBLE',' 
     &IN THIS MOLECULE.')
      
      write(Iout,99002)isum
      
99002 format(' THE SUM OF ATOMIC NUMBERS IS',i3,', NZ IS',i3)
      
      write(Iout,99003)(Ianz(i),i=1,Nz)
      
99003 format('  ATOMIC NUMBER VECTOR:',20I3)
      
      call lnk1e
      stop
      
      end
C* :1 * 
      
