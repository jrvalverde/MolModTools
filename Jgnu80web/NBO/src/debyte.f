
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 debyte"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "debyte.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "debyte.web"
      subroutine debyte(I,IBYTE)
      implicit none
      integer I,ib1,ib2,ib3,IBYTE,ioff,ir
      dimension IBYTE(4)
      
      data ib1,ib2,ib3,ioff/256,65536,16777216,538976256/
      
      
      IBYTE(4)=I/ib3
      ir=I-IBYTE(4)*ib3
      IBYTE(3)=ir/ib2
      ir=ir-IBYTE(3)*ib2
      IBYTE(2)=ir/ib1
      IBYTE(1)=ir-IBYTE(2)*ib1
      
      IBYTE(1)=IBYTE(1)+ioff
      IBYTE(2)=IBYTE(2)+ioff
      IBYTE(3)=IBYTE(3)+ioff
      IBYTE(4)=IBYTE(4)+ioff
      return
      end
C* :1 * 
      
