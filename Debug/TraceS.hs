module Debug.TraceS (traceS)

traceS :: Show a => a -> a
traceS x = trace (show x) x

