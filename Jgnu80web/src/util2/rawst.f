
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rawst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rawst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "rawst.web"
      subroutine rawst(S,T,LAX,LAY,LAZ,LBX,LBY,LBZ,AS,TWOASQ,SX,SY,SZ)
      implicit none
      double precision AS,cnst,gfloat,S,SX,sxy,sxz,SY,syz,SZ,T,TWOASQ
      integer ijx,ijy,ijz,LAX,LAY,LAZ,LBX,LBY,LBZ
      dimension SX(2),SY(2),SZ(2),cnst(6)
      data cnst/0.D0,0.D0,1.D0,3.D0,6.D0,10.D0/
      
      
      ijx=4*(LAX-1)+LBX
      ijy=4*(LAY-1)+LBY
      ijz=4*(LAZ-1)+LBZ
      sxy=SX(ijx)*SY(ijy)
      sxz=SX(ijx)*SZ(ijz)
      syz=SY(ijy)*SZ(ijz)
      S=sxy*SZ(ijz)
      T=gfloat(2*(LAX+LAY+LAZ)-3)*AS*S-TWOASQ*(SX(ijx+8)*syz+SY(ijy+8)*s
     &xz+SZ(ijz+8)*sxy)
      if(LAX.GT.2)T=T-cnst(LAX)*SX(ijx-8)*syz
      if(LAY.GT.2)T=T-cnst(LAY)*SY(ijy-8)*sxz
      if(LAZ.GT.2)T=T-cnst(LAZ)*SZ(ijz-8)*sxy
      return
      
      end
C* :1 * 
      
