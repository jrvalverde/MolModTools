
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 mtget"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "mtget.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "mtget.web"
      integer function mtget(ISH,JSH,KSH,LSH,ISHP,JSHP,KSHP,LSHP,NSET)
      implicit none
      integer iab,ibc,icd,ISH,ISHP,JSH,JSHP,KSH,KSHP,LSH,LSHP,mtype,NSET
     &,nsetd
      dimension nsetd(8)
      data nsetd/3,2,2,1,2,2,1,1/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      iab=0
      if(ISH.EQ.JSH)iab=4
      ibc=0
      if(JSH.EQ.KSH)ibc=2
      icd=0
      if(KSH.EQ.LSH)icd=1
      mtype=iab+ibc+icd+1
      
      NSET=nsetd(mtype)
      
      ISHP=ISH
      JSHP=JSH
      KSHP=KSH
      LSHP=LSH
      if(mtype.EQ.1)then
      elseif(mtype.EQ.2)then
      elseif(mtype.EQ.3)then
      elseif(mtype.EQ.4)then
      elseif(mtype.EQ.5)then
      goto 100
      elseif(mtype.EQ.6)then
      elseif(mtype.NE.8)then
      goto 100
      endif
      goto 200
100   ISHP=LSH
      JSHP=KSH
      KSHP=JSH
      LSHP=ISH
      
200   mtget=mtype
      return
      
      end
C* :1 * 
      
