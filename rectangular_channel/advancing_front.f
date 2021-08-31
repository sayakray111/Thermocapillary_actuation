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
      Dimension xg(200*nsg),yg(200*nsg)
      
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
c--------------------------------------------------------------------------------------
c Creating linked list for all the nodes in the system
c--------------------------------------------------------------------------------
      type (Front_edge):: pointer first_edge,current_edge
      k = 1
      first_edge%x1 = xg(k,1)
      first_edge%y1 = yg(k,1)
      first_edge%x2 = xg(k,2)
      first_edge%y2 = yg(k,2)
      allocate(first_edge)
      current_edge=>first_edge
      k=k+1
      do while(xg(k).le.0.0D0)
          allocate(current_edge%next)         
          current_edge=>current_edge%next
          current_edge%x1 = xg(k,1)
          current_edge%y1 = yg(k,1)
          current_edge%x2 = xg(k,2)
          current_edge%y2 = yg(k,2)
          k=k+1          
      end do
      current_edge=>first_edge
c-----------------------------------------------------------------------
c Print out the nodes 
c-------------------------------------------------------------------------
      do while(.not. associated(current_edge))
          print(*,*) 'The current edges are formed by the elements :'
     +    ,current_edge%x1,current_edge%y1,current_edge%x2,current_edge
     +    %y2
          current_edge=>current_edge%next
      end do

c-----------------------------------------------------------------------------
c Measure of ideal distance from the edge
c----------------------------------------------------------------------------
      current_edge=>first_edge
      do while(.not. associated(current_edge))
          xm = ((current_edge%x1)+(current_edge%x2))/2
          ym = ((current_edge%y1)+(current_edge%y2))/2
          fpx = current_edge%x1
          fpy = current_edge%y1
          lpx = current_edge%x2
          lpy = current_edge%y2
          dist = sqrt(((fpx-xm)**2)+((fpy-ym)**2))
          slope1 = (h/dist)
          slope2 = (lpy-fpy)/(lpx-fpx)
          xi = ((slope1*slope2*fpx)+xm)/(ym-fpy)
          yinum = 
      end do
      
      subroutine push(index,edge)
      type (Front_edge) pointer:: edges
      edges=>edge
      return
      end
      end do
      Return
      end