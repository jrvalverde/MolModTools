
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fillcp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fillcp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "fillcp.web"
      subroutine fillcp(IT,IGBEG,IGSP,IGDF,CA,CMAXI)
      implicit none
      double precision absc,C1,C2,C3,C4,CA,CMAXI,Exx,gabs,gmax1,Pt5,R1,R
     &2,R3,R3ov2,R4,Root15,Root3,Root5,temp
      double precision X,Y,Z,Z1,Z2,Z3
      integer IGBEG,IGDF,IGSP,In,inddf,Iout,Ipunch,IT,Jan,MAXPRM,MAXS21,
     &MAXSH1,MAXSHL,Maxtyp,Nshell
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      dimension CA(20)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/io/In,Iout,Ipunch
      
      
      
      if(IT.LT.3)then
      
      CA(1)=C1(IGSP)
      CMAXI=gabs(C1(IGSP))
      
      if(IT.GT.0)then
      
      CA(2)=C2(IGSP)
      CA(3)=C2(IGSP)
      CA(4)=C2(IGSP)
      absc=gabs(C2(IGSP))
      CMAXI=gmax1(CMAXI,absc)
      
      if(IT.GT.1)then
      
      inddf=IGDF+(IGSP-IGBEG)
      CA(5)=C3(inddf)
      CA(6)=C3(inddf)
      CA(7)=C3(inddf)
      temp=C3(inddf)*Root3
      CA(8)=temp
      CA(9)=temp
      CA(10)=temp
      absc=gabs(C3(inddf))
      CMAXI=gmax1(CMAXI,absc)
      endif
      endif
      else
      
      inddf=IGDF+(IGSP-IGBEG)
      CA(11)=C4(inddf)
      CA(12)=C4(inddf)
      CA(13)=C4(inddf)
      temp=C4(inddf)*Root5
      CA(14)=temp
      CA(15)=temp
      CA(16)=temp
      CA(17)=temp
      CA(18)=temp
      CA(19)=temp
      CA(20)=C4(inddf)*Root15
      CMAXI=gabs(C4(inddf))
      endif
      
      return
      
      end
C* :1 * 
      
