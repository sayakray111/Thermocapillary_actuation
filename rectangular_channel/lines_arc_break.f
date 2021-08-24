          subroutine lines_arc_break(ptsx,ptsy)
         
          Implicit Double Precision (a-h,o-z)
          Dimension line_points(500,500),curve_points(500,500)
          Dimension ptsx(500),ptsy(500)
          
          common xxx01/ ptsx,ptsy
          common xxx02/ line_points,curve_points
          k = 1
          count = 1
          error_margin = 0.0001D0
          do i=k,len(ptsx)
            do l=i,len(ptsx)-2
              slope1=(ptsy(l+1)-ptsy(l))/(ptsy(l+1)-ptsy(l))
              slope2=(ptsy(l+2)-ptsy(l+1))/(ptsy(l+2)-ptsy(l+1))
              if((slope1-slope2).LT.error_margin)then
                  k = l+2
                  line_points(count) = i
                  line_points(count+1) = k
                  continue
              else 
                  curve_points(count)=l+1                  
                  count+=1
                  k = l+1
                  exit
              end if
            end do
          end do
          
      return 
      end