type Position = (Double, Double)

data Graphics 
    = Line { start :: Position, end :: Position } 
    | Circle { center :: Position, radius :: Double }
    | Compound [Graphics]

test :: Graphics -> String 
test Line{start = s, end = e} = "line"
test Circle{center = c, radius = r} = "circle"
test (Compound list) = "gay"
