
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gcfact"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gcfact.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "gcfact.web"
      subroutine gcfact
      implicit none
      double precision f15,five,four,gsqrt,one,Pt5,R1,R2,R3,R3ov2,R4,Roo
     &t15,Root3,Root5,three,two,Z1,Z2,Z3
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      data three/3.0D0/,five/5.0D0/,f15/15.0D0/
      data two/2.0D0/,one/1.0D0/,four/4.0D0/
      
      Root3=gsqrt(three)
      R3ov2=Pt5*Root3
      R3=R3ov2
      Root5=gsqrt(five)
      Root15=gsqrt(f15)
      R1=Pt5*gsqrt(five/two)
      R2=Pt5*three*Root5
      R4=Pt5*gsqrt(three/two)
      Z1=four/Root5
      Z2=one/Root5
      Z3=three/Root5
      return
      
      end
C* :1 * 
      
