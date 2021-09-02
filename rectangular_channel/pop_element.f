c----------------------------------------------------------------------------
c Subroutine to delete an edge from the front-----------------------------------------
c--------------------------------------------------------------------------
      
      subroutine pop_element(edge,previous_edge)
      type (Front_edge) pointer:: edge,previous_edge,temp
     +                           ,first_edge,last_edge
      common /xxx01/ first_edge,last_edge
      temp=>first_edge
      if(edge=>first_edge)then
         do while(temp%next.ne.first_edge)
             temp=>temp%next
         end do
      temp%next=>edge%next
      else if(edge=>last_edge)then
           edge%next=>first_edge
      else
           previous_edge%next=>edge%next
      end if
      Return
      end