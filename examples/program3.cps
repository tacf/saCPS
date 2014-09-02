f := (\proc(x,y).(if (\k.k x) 
                     (\k.k (\k.k y)) 
                     (g (\k.k 2))(\x.(if (\k.k y)
                                     (\k.k 3)(\x.(\k.k(\k.k x)))
                                     (\k.k  y)(\x.(\k.k(\k.k x)))))))

g := (\proc(w).(\k.k w)(\z.(\k.k 2)(\z.(\k.k(\k.k z)))))