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
      Dimension NE(nsg),Itp(nsg), xcntr(nsg), ycntr(nsg), Actis(nsg),
      Dimension RT(nsg),ptsx(500),ptsy(500)
      Dimension line_points(500,500),curve_points(500,500)
      Dimension x2(200),y2(200),s2(200)
	Dimension xm(200),ym(200),sm(200)
      Dimension xg2(nsg,200),yg2(nsg,200),sg2(nsg,200)
      Dimension x0(500),y0(500),s0(500)
      Dimension phi0(nsg,200),dphidn0(nsg,200)
      Dimension AL(nsg*200,nsg*200) BL(nsg*200) SOL(nsg*200)
      Dimension elml(nsg,200)
      Dimension tnx0(500),tny0(500),vnx0(500),vny0(500)
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
c-----------------------------------------------
c Extracting the straight line segments from the data
c--------------------------------------------
      write(4,*) 'Enter the number of points'
      read(4,*) np
      nsg = np+1
      open(4,file='details_boundary.dat')
      do l = 1,np
	   read(4,*,end=99) ptsx(l),ptsy(l)
      end do
      call lines_arc_break(ptsx,ptsy)
   99	continue  
c--------------------------------------
c print out the line and curve points --- 
c---------------------------------
c Print out the line points 
c---------------------------------------
	do kl = 1,len(lines_points)
	  write(*,*) ptsx(line_points(kl)),ptsy((line_points(kl))
	end do	
c---------------------------
c print out the curve points 
c-------------------------------
	do kll = 1,len(curve_points)
	  write(*,*) ptsx(curve_points(kll)),ptsy((curve_points(kll))
	end do
c-------------------------------------
c Discretisation of the lines
c-------------------------------------       
      count_col = 0
      do k = 1,len(line_points)-1
       call elm_line(NE(k),RT(k)
     +   ,ptsx(line_points(k,1)),ptsy(line_points(k,1))
     +   ,ptsx(line_points(k+1,2)),ptsy(line_points(k+1,2))
     +   ,0
     +   ,0
     +   ,x2,y2,s2
     +   ,xm,ym,sm)
          
       do i=1,NE(k)+1
         xg2(k,i) = x2(i)
         yg2(k,i) = y2(i)
         sg2(k,i) = s2(i)
       end do
		  
       do i = 1,NE(k)
         count_col+=1
         ddx = xg2(k,i)-xg2(k,i+1)
         ddy = yg2(k,i)-yg2(k,i+1)
         elml(k,i)=sqrt((ddx**2)+(ddy**2))
         x0(count_col) = xm(i)
         y0(count_col) = ym(i)
         s0(count_col) = sm(i)
         tnx0(count_col) = ddx/elml(k,i)
         tny0(count_col) = ddy/elml(k,i)
       end do    
      end do 
c-----------------------------------------------------------
c discretisation of the remaining curves with curved elements
c---------------------------------------------------------
      do k = 1,len(curve_points)-1
		if(curve_points(k,1).eq.0.0D0)then
			continue
		end if
		
	end do
      Return
      end
	  