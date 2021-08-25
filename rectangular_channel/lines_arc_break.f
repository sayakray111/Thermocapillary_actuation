c============================
c Author: S.Ray
c Micorfluidics Lab, IITKGP 2020
c============================    
      
          subroutine lines_arc_break(ptsx,ptsy)
         
          Implicit Double Precision (a-h,o-z)
          Dimension line_points(500),curve_points(500)
          Dimension ptsx(500),ptsy(500)
          
          common xxx01/ ptsx,ptsy
          common xxx02/ line_points,curve_points
c------------------------------------------------------------
c Checking if the first point lies on a curve or a straight line 
c----------------------------------------------------------
          error_margin = 0.0001D0
          slope1=(ptsy(2)-ptsy(l))/(ptsx(2)-ptsx(l))
          slope2=(ptsy(3)-ptsy(2))/(ptsx(3)-ptsx(2))
          count1 = 1
          if((slope1-slope2).GT.error_margin)then
              curve_points(count1) = 1
              count1+=1
          end if
c------------------------------------------------
c finding which points are in line/curve segments
c-------------------------------------------------
          count = 0
		inc = 1
		p1 = 0
		pl = 1
		flag = 0
          do i=1,len(ptsx),inc
		   slope1=(ptsy(i+1)-ptsy(i))/(ptsx(i+1)-ptsx(i))
		   do k = 2,len(ptsx)-i
		       slope2=(ptsy(i+k)-ptsy(i))/(ptsx(i+k)-ptsx(i))
		       if((slope1-slope2).LT.error_margin)then
				   if(flag==0)then
					   count+=1
					   p1+=1
					end if
                     line_points(p1,count)=i
                     line_points(p1,count+1)=i+k
				   flag = 1
		       else 
                     curve_points(pl,count1)=i+k-1
				   curve_points(pl,count1+1)=i+k
                     count1+=2
				   pl+=1
				   flag = 1
				   exit
			   end if
		   end do
		   inc = k+1
		   flag = 0
          end do		          
          return 
          end