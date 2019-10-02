
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0314"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0314.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "uu0314.web"
      blockdata uu0314
      implicit none
      integer Ifao,Ifcont,Ifpure,Indao,Indix,Indiy,Indiz,Indjx,Indjy,Ind
     &jz,Indkx,Indky,Indkz,Indlx,Indly,Indlz,Ipure,Jpure,Kpure,Limxyz
      integer Lpure,Maxxyz
      common/jpure/Ifpure(4,4),Ipure,Jpure,Kpure,Lpure
      common/aoinds/Ifcont,Limxyz,Maxxyz,Ifao,Indao(1296)
      common/indxyz/Indix(20),Indiy(20),Indiz(20),Indjx(20),Indjy(20),In
     &djz(20),Indkx(20),Indky(20),Indkz(20),Indlx(20),Indly(20),Indlz(20
     &)
      data Indlx/1,2,1,1,3,1,1,2,2,1,4,1,1,2,3,3,2,1,1,2/
      data Indly/1,1,2,1,1,3,1,2,1,2,1,4,1,3,2,1,1,2,3,2/
      data Indlz/1,1,1,2,1,1,3,1,2,2,1,1,4,1,1,2,3,3,2,2/
      data Indkx/0,4,0,0,8,0,0,4,4,0,12,0,0,4,8,8,4,0,0,4/
      data Indky/0,0,4,0,0,8,0,4,0,4,0,12,0,8,4,0,0,4,8,4/
      data Indkz/0,0,0,4,0,0,8,0,4,4,0,0,12,0,0,4,8,8,4,4/
      data Indjx/0,16,0,0,32,0,0,16,16,0,48,0,0,16,32,32,16,0,0,16/
      data Indjy/0,0,16,0,0,32,0,16,0,16,0,48,0,32,16,0,0,16,32,16/
      data Indjz/0,0,0,16,0,0,32,0,16,16,0,0,48,0,0,16,32,32,16,16/
      data Indix/0,64,0,0,128,0,0,64,64,0,192,0,0,64,128,128,64,0,0,64/
      data Indiy/0,0,64,0,0,128,0,64,0,64,0,192,0,128,64,0,0,64,128,64/
      data Indiz/0,0,0,64,0,0,128,0,64,64,0,0,192,0,0,64,128,128,64,64/
      data Ifpure/0,0,-1,+1,0,0,0,+1,0,0,-1,0,4*0/
      data Maxxyz/0/
      end
C* :1 * 
      
