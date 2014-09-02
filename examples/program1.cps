f := (\proc(x,y).(if (\k.k x) 
                     (\k.k (\k.k y)) 
                     (\k.k 1)(\x.(if (\k.k y)
                                     (\k.k(\k.k x))
                                     (\k.k  x)(\y.(\k.k(\k.k y)))))))

g := (\proc(w).(\k.k w)(\z.(\k.k 2)(\z.(\k.k(\k.k z))))