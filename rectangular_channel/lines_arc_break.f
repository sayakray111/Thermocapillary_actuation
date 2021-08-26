c============================
c Author: S.Ray
c Micorfluidics Lab, IITKGP 2020
c============================    
      
          subroutine lines_arc_break(ptsx,ptsy)
         
          Implicit Double Precision (a-h,o-z)
          Dimension line_points(500,2),curve_points(500,2)
          Dimension ptsx(500),ptsy(500)
		
          error_margin = 0.0001D0
          nlinepts = 0
          ncurvepts = 0
          common xxx01/ ptsx,ptsy
          common xxx02/ line_points,curve_points
          common xxx03/ nlinepts,ncurvepts

          error_margin = 0.0001D0
c---------------------------------------
c Filling the matrices with zero values
c------------------------------------------
          do ll = 1,500
           line_points(ll,1) = 0
           line_points(ll,2) = 0
          end do
          do ll = 1,500
           curve_points(ll,1) = 0
           curve_points(ll,2) = 0
          end do
c------------------------------------------------
c Extracting the collinear points from the set
c-------------------------------------------------
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
               p1+=1
             end if
             line_points(p1,1)=i
             line_points(p1,2)=i+k
             flag = 1
            else 
             curve_points(pl,1)=i+k-1
             curve_points(pl,2)=i+k
             pl+=1
             flag = 1
             exit
            end if
           end do
           inc = k-1
           flag = 0
          end do
c----------------------------------------------------------------------------
c find out the number of collinear segments ----------------------
c---------------------------------------------------------------------------
          do jk = 1,len(line_points)
           if(line_points(jk,1).neq.0)then
            nlinepts+=1
           end if
          end do
	
          
c-----------------------------------------------------------------------------------------------
c Cleaning the curved sections data and assigning zero elements and last element to the curved set
c---------------------------------------------------------------------------------------------		
          do h = 1,nlinepts
           do a = 1,ncurvepts
            if(line_points(h,1).eq.curve_points(a,1))then
             curve_points(a,1) = 0
              curve_points(a,2) = 0
            end if
           end do
          end do
c----------------------------------------------------------------------------
c find out the number of non collinear segments ----------------------
c---------------------------------------------------------------------------
      
          do jk = 1,len(curve_points)
           if(curve_points(jk,1).neq.0)then
            ncurvepts+=1
           end if
          end do
c---------------------------------------------------------------
c Joining the last two points to get another curved segment------
c-------------------------------------
          curve_points(ncurvepts+1,1)=len(ptsx)
          curve_points(ncurvepts+1,2)=1
          return 
          end