module Rubik where

-- Simple difinition of how the cube is made
-- TODO: Insert the notion of orientation

-- A intuitive notion of how the cube is made
data Cube = Cube Edges Corners deriving (Show, Eq) -- The Cube is made out of a set of Edges and Corners
data Edges = Edges Edge Edge Edge Edge Edge Edge Edge Edge Edge Edge Edge Edge deriving (Show, Eq) -- The Edges are all 12 Edge Pieces
data Corners = Corners Corner Corner Corner Corner Corner Corner Corner Corner deriving (Show, Eq) -- The Corners are all 8 Corner Pieces
data Edge = Edge Facet Facet deriving (Show, Eq) -- An edge is made out of 2 faces
data Corner = Corner Facet Facet Facet deriving (Show, Eq) -- An corner is made out of 3 faces
data Facet = Red | Orange | Yellow | White | Green | Blue deriving (Show, Eq) -- A facet has one of those six colors

-- -- All the cublets -- --
-- The Edges in the top layer
uf = (Edge White Red     )
ur = (Edge White Blue    )
ub = (Edge White Orange  )
ul = (Edge White Green   )

-- The Edges in the middle layer
fr = (Edge Red    Blue   )
fl = (Edge Red    Green  )
br = (Edge Orange Blue   )
bl = (Edge Orange Green  )

-- The edges in the buttom layer
df = (Edge Yellow Red    )
dr = (Edge Yellow Blue   )
dl = (Edge Yellow Green  )
db = (Edge Yellow Orange )

-- All of the above but inverted
fu = (Edge Red    White  )
ru = (Edge Blue   White  )
bu = (Edge Orange White  )
lu = (Edge Green  White  )
rf = (Edge Blue   Red    )
lf = (Edge Green  Red    )
rb = (Edge Blue   Orange )
lb = (Edge Green  Orange )
fd = (Edge Red    Yellow )
rd = (Edge Blue   Yellow )
ld = (Edge Green  Yellow )
bd = (Edge Orange Yellow )

-- The Corners in the top layer
ufr = (Corner White Red Blue)
ufl = (Corner White Red Green)
ubr = (Corner White Orange Blue)
ubl = (Corner White Orange Green)

-- The Corners in the buttom layer
dfr = (Corner Yellow Red Blue)
dfl = (Corner Yellow Red Green)
dbr = (Corner Yellow Orange Blue)
dbl = (Corner Yellow Orange Green)

-- A function ta returns the inverse of a edge
-- It is obviously correct, unless there is some typo :)
inverted (Edge Red    White  ) = (Edge White  Red    )
inverted (Edge Blue   White  ) = (Edge White  Blue   )
inverted (Edge Orange White  ) = (Edge White  Orange )
inverted (Edge Green  White  ) = (Edge White  Green  )
inverted (Edge Blue   Red    ) = (Edge Red    Blue   )
inverted (Edge Green  Red    ) = (Edge Red    Green  )
inverted (Edge Blue   Orange ) = (Edge Orange Blue   )
inverted (Edge Green  Orange ) = (Edge Orange Green  )
inverted (Edge Red    Yellow ) = (Edge Yellow Red    )
inverted (Edge Blue   Yellow ) = (Edge Yellow Blue   )
inverted (Edge Green  Yellow ) = (Edge Yellow Green  )
inverted (Edge Orange Yellow ) = (Edge Yellow Orange )

inverted (Edge White  Red    ) = (Edge Red    White  )
inverted (Edge White  Blue   ) = (Edge Blue   White  )
inverted (Edge White  Orange ) = (Edge Orange White  )
inverted (Edge White  Green  ) = (Edge Green  White  )
inverted (Edge Red    Blue   ) = (Edge Blue   Red    )
inverted (Edge Red    Green  ) = (Edge Green  Red    )
inverted (Edge Orange Blue   ) = (Edge Blue   Orange )
inverted (Edge Orange Green  ) = (Edge Green  Orange )
inverted (Edge Yellow Red    ) = (Edge Red    Yellow )
inverted (Edge Yellow Blue   ) = (Edge Blue   Yellow )
inverted (Edge Yellow Green  ) = (Edge Green  Yellow )
inverted (Edge Yellow Orange ) = (Edge Orange Yellow )

-- Clockwise rotation of a corner piece
-- TODO

-- Counter-Clockwise rotation of a corner piece
-- TODO

-- The cube in a solved state
solved = (Cube (Edges uf  ul  ub  ur
                      fr  fl  bl  br
                      df  dl  db  dr)

              (Corners ufr  ufl  ubl  ubr
                       dfr  dfl  dbl  dbr))

-- Returns true if the cube is solved
isSolved x = x == solved

--move_U :: Cube -> Cube
move_U (Cube (Edges a b c d e f g h i j k l) (Corners m n o p q r s t)) = (Cube (Edges d a b c e f g h i j k l) (Corners p m n o q r s t))

-- Since a cube face is a rotation group it is possible to define U2 as U two times and U' as U three times
-- Looks nice and it is obviously both code and mathematically correct, not very speed efficient though
move_U2 x = move_U $ move_U x
move_U' x = move_U $ move_U $ move_U x

--move_R :: Cube -> Cube
move_R (Cube (Edges a b c d e f g h i j k l) (Corners m n o p q r s t)) = (Cube (Edges a b c e l f g d i j k h) (Corners q n o m t r s p))
move_R2 x = move_R $ move_R x
move_R' x = move_R $ move_R $ move_R x



-- Some simple tests
-- it should return True always
test = and $ [solved == solved
            , move_U solved == (move_U' $ move_U' $ move_U' solved)
            , move_U ( move_U solved ) == move_U' (move_U' solved)
            , move_R ( move_R solved ) == move_R' (move_R' solved)
            , move_R2 (solved )        == move_R' (move_R' solved) ]
