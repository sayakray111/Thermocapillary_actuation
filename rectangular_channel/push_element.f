c----------------------------------------------------------------------------
c Subroutine to push an edge into the front -----------------------------------------
c--------------------------------------------------------------------------
      
      subroutine push_element(previous_edge,edge)
      type (Front_edge) pointer:: edge,first_edge,last_edge
     +                           ,previous_edge
      
      common /xxx01/ first_edge,last_edge
      edge%next=>previous_edge%next
      previous_edge%next=>edge
      return
      end