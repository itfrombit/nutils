(function range (start end)
     (set i start)
     (set result nil)
     (while (<= i end)
            (set result (append result (list i)))
            (set i (+ i 1)))
     result)

(puts (range 1 10))
