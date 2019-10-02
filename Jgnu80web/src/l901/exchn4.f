
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 exchn4"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "exchn4.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "exchn4.web"
      subroutine exchn4(NO,IBUC1,IBUC2,ISCR1,ISCR2)
      implicit none
      integer i,IBUC1,IBUC2,In,ind,indj,indk,indl,Iout,ip,Ipunch,ISCR1,I
     &SCR2,j,k,kp,l,leng,lst,Mdv
      integer mdv2,NO,no2,no3,no4,nom,nos
      double precision one,V
      common/v/V(20000),Mdv
      common/io/In,Iout,Ipunch
      data one/1.D0/
      
      
      
      
      
      
      
      
      
      call track('EXCHN4')
      
      mdv2=Mdv/2
      nos=NO*(NO+1)/2
      no2=NO*NO
      no3=no2*NO
      no4=no3*NO
      
      call expsym(nos,IBUC1,ISCR1)
      call exp78(NO,NO,ISCR1,ISCR2)
      call mattrn(NO,NO,NO,NO,2,ISCR2,ISCR1,mdv2)
      call mattrn(NO,NO,NO,NO,3,ISCR1,ISCR2,mdv2)
      call sumn(ISCR2,ISCR1,no4,-one)
      
      nom=NO-1
      call fileio(2,-ISCR1,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      
      do 100 i=1,nom
      ip=i+1
      leng=no3
      call fileio(2,ISCR1,leng,V,0)
      ind=0
      
      do 50 j=ip,NO
      indj=(j-1)*no2
      do 20 k=i,nom
      kp=k+1
      lst=kp
      if(i.EQ.k)lst=j
      indk=indj+(k-1)*NO
      do 10 l=lst,NO
      indl=indk+l
      ind=ind+1
      V(ind)=V(indl)
10    continue
20    continue
50    continue
      
      leng=ind
      call fileio(1,IBUC2,leng,V,0)
100   continue
      
      return
      
      end
C* :1 * 
      
