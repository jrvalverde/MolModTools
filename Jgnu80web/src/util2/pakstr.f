
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pakstr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pakstr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "pakstr.web"
      subroutine pakstr(INSTR,LEN,OUTSTR,OUTCUR)
      implicit none
      integer blank,chr,getchr,i,ibln,INSTR,l,LEN,OUTCUR,OUTSTR,putone
      dimension INSTR(*),OUTSTR(*)
      data blank/'    '/
      
      l=0
      putone=0
      ibln=0
      
      do 100 i=1,LEN
      chr=getchr(INSTR,l)
      if(chr.EQ.blank)then
      
      ibln=1
      else
      if(putone.NE.0)then
      if(ibln.EQ.1)call puticr(blank,OUTSTR,OUTCUR)
      endif
      ibln=0
      putone=1
      call puticr(chr,OUTSTR,OUTCUR)
      endif
100   continue
      
      return
      
      end
C* :1 * 
      
