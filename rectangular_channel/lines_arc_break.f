c============================
c Author: S.Ray
c Micorfluidics Lab, IITKGP 2020
c============================    
      
          subroutine lines_arc_break(ptsx,ptsy)
         
          Implicit Double Precision (a-h,o-z)
          Dimension line_points(500),curve_points(500)
          Dimension ptsx(500),ptsy(500)
          
          common xxx01/ ptsx,ptsy
          common xxx02/ line_points,curve_points,nsg
c------------------------
c putting the value of the first curved point
c--------------------------------
          
          slope1=(ptsy(2)-ptsy(l))/(ptsx(2)-ptsx(l))
          slope2=(ptsy(3)-ptsy(2))/(ptsx(3)-ptsx(2))
          count1 = 1
          if((slope1-slope2).GT.error_margin)then
              curve_points(count1) = 1
              count1+=1
          end if
          
c-------------------------------------
c solving for the rest of the points
c--------------------------------------
          
          k = 1
          count = 1
          error_margin = 0.0001D0
          do i=k,len(ptsx)
            do l=i,len(ptsx)-2
              slope1=(ptsy(l+1)-ptsy(l))/(ptsx(l+1)-ptsx(l))
              slope2=(ptsy(l+2)-ptsy(l+1))/(ptsx(l+2)-ptsx(l+1))
              if((slope1-slope2).LT.error_margin)then
                  k = l+2
                  line_points(count) = i
                  line_points(count+1) = k
                  continue
              else 
                  curve_points(count1)=l+1                  
                  count1+=1
                  k = l+1
                  exit
              end if
            end do
            count+=1
          end do
          
      return 
      end