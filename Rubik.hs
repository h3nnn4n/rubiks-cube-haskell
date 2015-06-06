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

-- The Edges in the top layer
uf = (Edge White Red)
ur = (Edge White Blue)
ub = (Edge White Orange)
ul = (Edge White Green)

-- The Edges in the middle layer
fr = (Edge Red Blue)
fl = (Edge Red Green)
br = (Edge Orange Blue)
bl = (Edge Orange Green)

-- The edges in the buttom layer
df = (Edge Yellow Red)
dr = (Edge Yellow Blue)
dl = (Edge Yellow Green)
db = (Edge Yellow Orange)

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

-- The cube in a solved state
solved = (Cube (Edges uf  ul  ub  ur
                      fr  fl  bl  br
                      df  dl  db  dr)

              (Corners ufr  ufl  ubl  ubr
                       dfr  dfl  dbl  dbr))

--move_U :: Cube -> Cube
move_U (Cube (Edges a b c d e f g h i j k l) (Corners m n o p q r s t)) = (Cube (Edges d a b c e f g h i j k l) (Corners p m n o q r s t))

-- Since a cube face is a rotation group it is possible to define U2 as U two times and U' as U three times
-- Looks nice and it is obviously both code and mathematically correct, not very speed efficient though
move_U2 x = move_U $ move_U x
move_U' x = move_U $ move_U $ move_U x

--move_R :: Cube -> Cube
move_R (Cube (Edges a b c d e f g h i j k l) (Corners m n o p q r s t)) = (Cube (Edges a b c e l f g d i j k h) (Corners q n o m t r s p))


-- Some simple tests
-- it should return True always
test = and $ [solved == solved
            , move_U solved == (move_U' $ move_U' $ move_U' solved)
            , move_U ( move_U solved ) == move_U' (move_U' solved)]
