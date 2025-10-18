selectie x s =
    case ( x , s ) of
         (0 , _ ) -> s
         (1 , z : zs ) -> zs
         (1 , [ ] ) -> [ ]
         _ -> ( s ++ s )