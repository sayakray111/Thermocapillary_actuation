      type Front_edge
          Integer*4 :: x1,y1,x2,y2
          type (Front_edge) pointer :: next
      end type Front_edge 
      subroutine advancing_front()
      
      Implicit Double Precision (a-h,o-z)
      Dimension xg2(nsg,200),yg2(nsg,200)
      Dimension xnctr(nsg),ycntr(nsg),NE(nsg),Itp(nsg),RT(nsg)
     +         ,Actis(nsg)
      Dimension RT(nsg),x2(500),y2(500),s2(500)
      Dimension x0(500),y0(500)
      Dimension xg(200*nsg,2),yg(200*nsg,2)
      
      common /xxx01/ nsg
      common /xxx02/ xnctr,ynctr,NE,Itp,RT
      common /xxx03/ RT,x2,y2,s2
      common /xxx04/ xg2,yg2
c-----------------------------------------------------------------------------------
c converting the 2D array into a 1D array
c---------------------------------------------------------------------------------------
      k = 1
      do i = 1,nsg
         do j = 1,199
             if(xg2(i,j).le.0.0D0)then
                 continue 
              end if
             xg(k,1) = xg2(i,j)
             xg(k,2) = xg2(i,j+1)
             yg(k,1) = yg2(i,j)
             yg(k,2) = yg2(i,j+1)
             k=k+1
         enddo
      enddo
      number_points = k
c--------------------------------------------------------------------------------------
c Creating circular linked list for all the nodes in the system
c--------------------------------------------------------------------------------
      type (Front_edge):: pointer first_edge,last_edge,current_edge
     +                   ,previous_edge,next_edge
      
      allocate(last_edge)
      last_edge%x1 => xg(k,1)
      last_edge%y1 => yg(k,1)
      last_edge%x2 => xg(k,2)
      last_edge%y2 => yg(k,2)
      k = 1
      allocate(first_edge)
      first_edge%x1 => xg(k,1)
      first_edge%y1 => yg(k,1)
      first_edge%x2 => xg(k,2)
      first_edge%y2 => yg(k,2)
      first_edge%next=>last_edge
      last_edge=>first_edge
      k=k+1
      allocate(previous_edge)
      allocate(next_edge)
      previous_edge=>first_edge
      next_edge=>last_edge
      do while(k<=number_points)
          allocate(current_edge)         
          current_edge%x1 => xg(k,1)
          current_edge%y1 => yg(k,1)
          current_edge%x2 => xg(k,2)
          current_edge%y2 => yg(k,2)
          call push_element(previous_edge,current_edge,next_edge)
          previous_edge=>current_edge
          current_edge=>current_edge%next
          k=k+1          
      end do
      current_edge=>first_edge
c-----------------------------------------------------------------------
c Print out the nodes 
c-------------------------------------------------------------------------
      do while(current_edge%next.ne.first_edge)
          print(*,*) 'The current edges are formed by the elements :'
     +    ,current_edge%x1,current_edge%y1,current_edge%x2,current_edge
     +    %y2
          current_edge=>current_edge%next
      end do

c-----------------------------------------------------------------------------
c Now form the algorithm
c----------------------------------------------------------------------------
      current_edge=>first_edge
      do while(current_edge%next.ne.first_edge)
          fpx <= current_edge%x1
          fpy <= current_edge%y1
          lpx <= current_edge%x2
          lpy <= current_edge%y2
          xm = (fpx+lpx)/2
          ym = (fpy+lpy)/2
          slope1 = (lpy-fpy)/(lpx-fpx)
          dist=sqrt(((fpx-xm)**2)+((fpy-ym)**2))
          slope2=(dist+(slope1*h))/(h-(slope1*dist))
          
          call proximity_search(xi,yi,h,number_points)
          call delete_front_edges(fpx,fpy,lpx,lpy)
          call add_front_edges(fpx,fpy,xi,yi,lpx,lpy)
          current_edge=>current_edge%next
          
      end do
c----------------------------------------------------------------------------
c Subroutine to push an edge into the front -----------------------------------------
c--------------------------------------------------------------------------
      
      subroutine push_element(previous_edge,edge,next_edge,first_edge
     +                      ,last_edge)
      type (Front_edge) pointer:: edge,next_edge,previous_edge
      edge%next=>previous_edge%next
      previous_edge%next=>edge
      return
      end
      
c----------------------------------------------------------------------------
c Subroutine to delete an edge from the front-----------------------------------------
c--------------------------------------------------------------------------
      
      subroutine pop(edge,previous_edge,first_edge,last_edge)
      type (Front_edge) pointer:: edge,previous_edge,temp
      temp=>first_edge
      if(edge=>first_edge)then
         do while(temp%next.ne.first_edge)
             temp=>temp%next
         end do
      temp%next=>edge%next
      else if(
      end if
      Return
      end
      
   

      Return
      end