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
uf = (Edge White  Red    )
ur = (Edge White  Blue   )
ub = (Edge White  Orange )
ul = (Edge White  Green  )

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
ufr = (Corner White Red Blue     )
ufl = (Corner White Red Green    )
ubr = (Corner White Orange Blue  )
ubl = (Corner White Orange Green )

-- The Corners in the buttom layer
dfr = (Corner Yellow Red Blue    )
dfl = (Corner Yellow Red Green   )
dbr = (Corner Yellow Orange Blue )
dbl = (Corner Yellow Orange Green)

-- The Clockwise rotation of all base (the ones above) corners
fru = (Corner Red    Blue  White )
flu = (Corner Red    Green White )
bru = (Corner Orange Blue  White )
blu = (Corner Orange Green White )
frd = (Corner Red    Blue  Yellow)
fld = (Corner Red    Green Yellow)
brd = (Corner Orange Blue  Yellow)
bld = (Corner Orange Green Yellow)

-- The Counter-Clockwise rotation of all base (the ones above) corners
ruf = (Corner Blue  White  Red   )
luf = (Corner Green White  Red   )
rub = (Corner Blue  White  Orange)
lub = (Corner Green White  Orange)
rdf = (Corner Blue  Yellow Red   )
ldf = (Corner Green Yellow Red   )
rdb = (Corner Blue  Yellow Orange)
ldb = (Corner Green Yellow Orange)

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
-- The Corners in the top layer

turn_cw (Corner White  Red    Blue ) = (Corner Red    Blue  White )
turn_cw (Corner White  Red    Green) = (Corner Red    Green White )
turn_cw (Corner White  Orange Blue ) = (Corner Orange Blue  White )
turn_cw (Corner White  Orange Green) = (Corner Orange Green White )
turn_cw (Corner Yellow Red    Blue ) = (Corner Red    Blue  Yellow)
turn_cw (Corner Yellow Red    Green) = (Corner Red    Green Yellow)
turn_cw (Corner Yellow Orange Blue ) = (Corner Orange Blue  Yellow)
turn_cw (Corner Yellow Orange Green) = (Corner Orange Green Yellow)

turn_cw (Corner Red    Blue  White ) = (Corner Blue  White  Red   )
turn_cw (Corner Red    Green White ) = (Corner Green White  Red   )
turn_cw (Corner Orange Blue  White ) = (Corner Blue  White  Orange)
turn_cw (Corner Orange Green White ) = (Corner Green White  Orange)
turn_cw (Corner Red    Blue  Yellow) = (Corner Blue  Yellow Red   )
turn_cw (Corner Red    Green Yellow) = (Corner Green Yellow Red   )
turn_cw (Corner Orange Blue  Yellow) = (Corner Blue  Yellow Orange)
turn_cw (Corner Orange Green Yellow) = (Corner Green Yellow Orange)

turn_cw (Corner Blue  White  Red   ) = (Corner White  Red    Blue )
turn_cw (Corner Green White  Red   ) = (Corner White  Red    Green)
turn_cw (Corner Blue  White  Orange) = (Corner White  Orange Blue )
turn_cw (Corner Green White  Orange) = (Corner White  Orange Green)
turn_cw (Corner Blue  Yellow Red   ) = (Corner Yellow Red    Blue )
turn_cw (Corner Green Yellow Red   ) = (Corner Yellow Red    Green)
turn_cw (Corner Blue  Yellow Orange) = (Corner Yellow Orange Blue )
turn_cw (Corner Green Yellow Orange) = (Corner Yellow Orange Green)

-- Counter-Clockwise rotation of a corner piece
-- Since the orientation of corners is a group,
-- one can define a Counter-Clockwise rotation as two Clockwise rotations
-- it is simpler and more compact but probably not as effiencient as
-- redefining everything as we did above
turn_ccw x = turn_cw $ turn_cw x

-- The cube in a solved state
solved = (Cube (Edges uf  ul  ub  ur
                      fr  fl  bl  br
                      df  dl  db  dr)

              (Corners ufr  ufl  ubl  ubr
                       dfr  dfl  dbl  dbr))

-- Returns true if the cube is solved
isSolved x = x == solved

----------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------

--Sample Function for a move
--move_I (Cube (Edges a b c d e f g h i j k l) FCorners m n o p q r s t)) = (Cube (Edges a b c d e f g h i j k l) (Corners m n o p q r s t))
--move_U :: Cube -> Cube
move_U (Cube (Edges a b c d e f g h i j k l)
           (Corners m n o p q r s t)) = (Cube
             (Edges d a b c e f g h i j k l)
           (Corners p m n o q r s t))

-- Since a cube face is a rotation group it is possible to define U2 as U two times and U' as U three times
-- Looks nice and it is obviously both code and mathematically correct, not very speed efficient though
move_U2 x = move_U $ move_U x
move_U' x = move_U $ move_U $ move_U x

-------------------------------------------
--move_R :: Cube -> Cube
move_R2 x = move_R $ move_R x
move_R' x = move_R $ move_R $ move_R x

move_R (Cube (Edges a b c d e f g h i j k l)
             (Corners m n o p q r s t)) =
             (Cube (Edges a b c e l f g d i j k h)
             (Corners (turn_ccw q) n o
                      (turn_cw m)
                      (turn_ccw t) r s
                      (turn_cw p)))

-------------------------------------------

move_F2 x = move_F $ move_F x
move_F' x = move_F $ move_F $ move_F x

move_F (Cube (Edges a b c d e f g h i j k l)
           (Corners m n o p q r s t)) = (Cube
             (Edges f b c d (inverted a) (inverted i) g h e j k l)
           (Corners (turn_ccw n)
                      (turn_cw r) o p
                      (turn_ccw m)
                      (turn_cw q) s t))

-------------------------------------------

move_L2 x = move_L $ move_L x
move_L' x = move_L $ move_L $ move_L x

move_L (Cube (Edges a b c d e f g h i j k l)
           (Corners m n o p q r s t)) = (Cube
             (Edges a g c d e b j h i f k l)
           (Corners m (turn_cw o)
                      (turn_ccw s) p q
                      (turn_cw n)
                      (turn_ccw r) t))

--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Since the expression is evaluated right to left, we must write the Cube algorithm reversed
-- R2 U' R' U' R U R U R U' R
perm_ua x = move_R2 $ move_U' $ move_R' $ move_U' $ move_R $ move_U $ move_R $ move_U $ move_R $ move_U' $ move_R x

-- R' U R' U' R' U' R' U R U R2
perm_ub x = move_R' $ move_U $ move_R' $ move_U' $ move_R' $ move_U' $ move_R' $ move_U $ move_R $ move_U $ move_R2 x

-- Ja perm
-- R' U L' U2 R U' R' U2 L R U'
perm_ja x = move_U' $ move_R $ move_L $ move_U2 $ move_R' $ move_U' $ move_R $ move_U2 $ move_L' $ move_U $ move_R' x

-- Jb perm
-- R U R' F' R U R' U' R' F R2 U' R' U'
perm_jb x = move_U' $ move_R' $ move_U' $ move_R2 $ move_F $ move_R' $ move_U' $ move_R' $ move_U $ move_R $ move_F' $ move_R' $ move_U $ move_R x

-- T perm
-- R U R' U' R' F R2 U' R' U' R U R' F'
perm_t x = move_F' $ move_R' $ move_U $ move_R $ move_U' $ move_R' $ move_U' $ move_R2 $ move_F $ move_R' $ move_U' $ move_R' $ move_U $ move_R x

-- F perm
-- R' U' F' R U R' U' R' F R2 U' R' U' R U R' U R
perm_f x = move_R $ move_U $ move_R' $ move_U $ move_R $ move_U' $ move_R' $ move_U' $ move_R2 $ move_F $ move_R' $ move_U' $ move_R' $ move_U $ move_R $ move_F' $ move_U' $ move_R' x

--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Dummy cycles for testing
-- All three should be of size 105 since they are the same cycle, but rotated
dummy_ru x = move_U $ move_R x
dummy_fu x = move_U $ move_F x
dummy_rf x = move_F $ move_R x
dummy_fr x = move_R $ move_F x
dummy_lu x = move_U $ move_L x
dummy_lf x = move_L $ move_F x
dummy_fl x = move_L $ move_F x

-- Finds the size of a cycle
cycleSize x = 1 + (length $ takeWhile (\y -> isSolved y == False) $ iterate x (x solved))
cycleTest = map cycleSize [dummy_ru, dummy_fu, dummy_rf, dummy_fr, dummy_lu, dummy_lf, dummy_fl]

-- Some simple tests
-- it should return True always
test = and $ [solved == solved
            , move_U solved == (move_U' $ move_U' $ move_U' solved)
            , move_U ( move_U solved ) == move_U' (move_U' solved)
            , move_R ( move_R solved ) == move_R' (move_R' solved)
            , perm_ub solved           == ( perm_ua $ perm_ua solved)
            , move_R2 (solved )        == move_R' (move_R' solved)
            , inverted uf == fu
            , and $ map (\x -> x == 105) cycleTest
            , turn_cw flu == luf
            ]
