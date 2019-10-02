
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0716"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0716.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "uu0716.web"
      blockdata uu0716
      
      
      implicit none
      double precision Dummy,Ex,Ffx,Fx,Fxyz
      integer Idrv1,Irwibf,Irwpa,Irwpb,Irwpt,Irww,Lenibf
      common/bd0716/Dummy
      common/irw716/Irww,Irwpt,Idrv1,Irwpa,Irwpb
      common/irwibf/Irwibf,Lenibf
      common/fxyz/Fxyz(105)
      common/force/Ex,Fx(105),Ffx(5565)
      data Irww,Irwpt,Idrv1,Irwpa,Irwpb/571,532,100,528,530/
      data Irwibf,Lenibf/513,15/
      data Fxyz/105*0./
      data Ex/0./,Fx/105*0./,Ffx/5565*0./
      
      
      end
C* :1 * 
      
