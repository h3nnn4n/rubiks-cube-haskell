module Rubik where

-- Simple difinition of how the cube is made
-- TODO: Insert the notion of orientation

-- A Cube is made out of a set of Edge and a set of Corner
data Cube = Cube [Edge] [Corner] deriving (Show, Eq)
data Edge = Edge Facet Facet deriving (Show, Eq)
data Corner = Corner Facet Facet Facet deriving (Show, Eq)
data Facet = Red | Orange | Yellow | White | Green | Blue deriving (Show, Eq)

-- The Edges in the top layer
uf = Edge White Red
ur = Edge White Blue
ub = Edge White Orange
ul = Edge White Green

-- The Edges in the middle layer
fr = Edge Red Blue
fl = Edge Red Green
br = Edge Orange Blue
bl = Edge Orange Green

-- The edges in the buttom layer
df = Edge Yellow Red
dr = Edge Yellow Blue
dl = Edge Yellow Green
db = Edge Yellow Orange

-- The Corners in the top layer
ufr = Corner White Red Blue
ufl = Corner White Red Green
ubr = Corner White Orange Blue
ubl = Corner White Orange Green

-- The Corners in the buttom layer
dfr = Corner Yellow Red Blue
dfl = Corner Yellow Red Green
dbr = Corner Yellow Orange Blue
dbl = Corner Yellow Orange Green

-- The cube in a solved state
solved = Cube [uf, ul, ub, ur, fr, fl, bl, br, df, dl, db, dr] [ufr, ufl, ubl, ubr, dfr, dfl, dbl, dbr]

move_U :: Cube -> Cube
move_U (Cube (a:b:c:d:e:f:g:h:i:j:k:l) (m:n:o:p:q:r:s:t)) = (Cube (d:a:b:c:e:f:g:h:i:j:k:l) (p:m:n:o:q:r:s:t))

-- Since a cube face is a rotation group it is possible to define U2 as U two times and U' as U three times
move_U2 x = move_U $ move_U x

move_U' x = move_U $ move_U $ move_U x

-- Some simple tests
-- it should return True always
test = and $ [solved == solved
            , move_U solved == (move_U' $ move_U' $ move_U' solved)
            , move_U ( move_U solved ) == move_U' (move_U' solved)]
