c============================
c Author: S.Ray
c Microfluidics Lab, IITKGP 2020
c============================
      
      
      subroutine rect_chan_geo
     +
     +   (Ncl
     +   ,Iwall
     +   ,wall
     +   )
	 
      Implicit Double Precision (a-h,o-z)
      Dimension NE(nsg),Itp(nsg), xcntr(nsg), ycntr(nsg), Actis(nsg)
      Dimension RT(nsg),ptsx(np),ptsy(np)
      Dimension x2(500),y2(500),s2(500)
      Dimension xm(200),ym(200),sm(200)
      Dimension xg2(nsg,200),yg2(nsg,200),sg2(nsg,200)
      Dimension x0(500),y0(500),s0(500)
      Dimension phi0(nsg,200),dphidn0(nsg,200)
      Dimension AL(nsg*200,nsg*200) BL(nsg*200) SOL(nsg*200)
      Dimension elml(nsg,200)
      Dimension tnx0(500),tny0(500),vnx0(500),vny0(500)
      Integer*4 :: line_points(500,2),curve_points(500,2)
      Integer*4 :: count_seg,count_col,k,inc,pl,pc,linepos,curvepos
      real*8 radius,rad,theta1,dist,r1,rhs,xmid,ymid,t1,theta1,theta2
     +      ,arc_length
c-------------------------------------
c common blocks 
c-------------------------------------
      common xxx01/ Iflow,nsg,ngl,NE,Itp,RT
      common xxx02/ xg2,yg2,sg2,dphidn0,phi0
      common xxx03/ actis,xcntr,ycntr
      common xxx04/ tnx0,tny0,vnx0,vny0
      common xxx05/ Vx,Vy
      common xxx06/ xwmin,ywmin,xwmax,ywmax
      common xxx07/ x0,y0,s0,count_col
      common xxx08/ ptsx,ptsy
      common xxx09/ line_points,curve_points
      common xxx10/ nlinepts,ncurvepts,np
c-----------------------------------------------------
c Read in the numbers of line points and curve points 
c------------------------------------------------------
      
      nsg = np+1
      open(4,file='details_boundary.dat')
      do l = 1,np
       read(4,*,end=99) ptsx(l),ptsy(l)
      end do
c-----------------------------------------------
c Extracting the straight line segments from the data
c--------------------------------------------      
      call lines_arc_break(ptsx,ptsy)
   99 continue  
c--------------------------------------
c print out the line and curve points --- 
c---------------------------------
c Print out the line points 
c---------------------------------------
      do kl = 1,nlinepts
       write(*,*) ptsx(line_points(kl)),ptsy((line_points(kl))
      end do	
c---------------------------
c print out the curve points 
c-------------------------------
      do kll = 1,ncurvepts
       write(*,*) ptsx(curve_points(kll)),ptsy((curve_points(kll))
      end do
c-------------------------------------------------
c Setting the element parameters for each segment 
c-----------------------------------------------------
      do ll = 1,nlinepts
       NEL(ll) = 10
       RTL(ll) = 0.5
      enddo
      do ll = 1,ncurvepts
       NEC(ll) = 10
       RTC(ll) = 0.5
      enddo
c-------------------------------------
c Discretisation of the segments
c-------------------------------------       
      count_col = 0
      count_seg = 0
      linepos = 0
      curvepos = 0
      inc = 1
      k = 1
      pl = 0
      pc = 0
c--------------------------------------------------------------------------------------
c We check if the point falls on a collinear segment and if it does then discretise it
c-------------------------------------------------------------------------------------
      do while(k<=np)
       do lc = 1,nlinepts
           if(line_points(lc,1).eq.k)then
              linepos=1
              pl=pl+1
           end if  
       enddo
       do cc = 1,ncurvepts
           if(curve_points(cc,1).eq.k)then
              curvepos=1
              pc=pc+1
           end if  
       enddo
       if(linepos.eq.1)then
        call elm_line(NEL(pl),RTL(pl)
     +   ,ptsx(line_points(pl,1)),ptsy(line_points(pl,1))
     +   ,ptsx(line_points(pl,2)),ptsy(line_points(pl,2))
     +   ,0
     +   ,0
     +   ,x2,y2,s2
     +   ,xm,ym,sm)
        
        count_seg=count_seg+1
        do i=1,NEL(pl)+1
         xg2(count_seg,i) = x2(i)
         yg2(count_seg,i) = y2(i)
         sg2(count_seg,i) = s2(i)
        end do
		  
        do i = 1,NEL(pl)
         count_col=count_col+1
         ddx = xg2(count_seg,i)-xg2(count_seg,i+1)
         ddy = yg2(count_seg,i)-yg2(count_seg,i+1)
         elml(count_seg,i)=sqrt((ddx**2)+(ddy**2))
         x0(count_col) = xm(i)
         y0(count_col) = ym(i)
         s0(count_col) = sm(i)
         tnx0(count_col) = ddx/elml(count_seg,i)
         tny0(count_col) = ddy/elml(count_seg,i)
        end do    
        inc = line_pts(pl,2)-k
        k=k+inc+1
c---------------------------------------------------------------------------------
c We check if it falls on a curvy segment and if it does then discretise it 
c---------------------------------------------------------------------------------
       elseif(curvepos.eq.1)then
        call elm_line(NEC(pc),RTC(pc)
     +   ,ptsx(curve_points(pc,1)),ptsy(curve_points(pc,1))
     +   ,ptsx(curve_points(pc,2)),ptsy(curve_points(pc,2))
     +   ,0
     +   ,0
     +   ,x2,y2,s2
     +   ,xm,ym,sm)
        
        count_seg=count_seg+1
        do i=1,NEC(pc)+1
         xg2(count_seg,i) = x2(i)
         yg2(count_seg,i) = y2(i)
         sg2(count_seg,i) = s2(i)
        end do
		  
        do i = 1,NEC(pc)
         count_col=count_col+1
         ddx = xg2(count_seg,i)-xg2(count_seg,i+1)
         ddy = yg2(count_seg,i)-yg2(count_seg,i+1)
         elml(count_seg,i)=sqrt((ddx**2)+(ddy**2))
         x0(count_col) = xm(i)
         y0(count_col) = ym(i)
         s0(count_col) = sm(i)
         tnx0(count_col) = ddx/elml(count_seg,i)
         tny0(count_col) = ddy/elml(count_seg,i)
        end do    
        inc = curve_pts(pc,2)-k
        k=k+inc+1
        endif
       
      end do
      Return
      end
	  