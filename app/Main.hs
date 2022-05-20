module Main where

import Test.QuickCheck
import System.Environment ( getArgs )

import Data.List
import Data.Maybe
  
-- Cube text format description
-- example: solved cube (W-White, O-Oragne, G-Green, R-Red, B-Blue, Y-Yellow)
--         WWW
--         WWW
--         WWW
--
--     OOO GGG RRR BBB
--     OOO GGG RRR BBB
--     OOO GGG RRR BBB
--
--         YYY
--         YYY
--         YYY
--
-- In the above example:
--  left face is Orange,
--  right face is Red, 
--  front face is Green,
--  back face is Blue,
--  up face is White
--  down face is Yellow 
--
-- See file cube.txt for another example

type Cube = [CubeElement]

data CubeElement = Center Face Color
                | Edge Face Color Face Color
                | Corner Face Color Face Color Face Color
                deriving (Eq, Show)

type Color = Char

data Face = FaceF | FaceB | FaceL | FaceR | FaceU | FaceD deriving (Eq, Show)

allFaces :: [Face]
allFaces = [FaceF, FaceB, FaceL, FaceR, FaceU, FaceD]

cubeFromFile :: String -> IO [CubeElement]
cubeFromFile filename = do
   content <- readFile filename
   return $ cubeFromString content

cubeFromString :: String -> [CubeElement]
cubeFromString s = centers ++ edges ++ corners
    where
    ls = lines s

    rowU1 = take 3 $ drop 4 $ ls !! 0
    rowU2 = take 3 $ drop 4 $ ls !! 1
    rowU3 = take 3 $ drop 4 $ ls !! 2

    rowL1 = take 3 $ ls !! 4
    rowL2 = take 3 $ ls !! 5
    rowL3 = take 3 $ ls !! 6

    rowF1 = take 3 $ drop 4 $ ls !! 4
    rowF2 = take 3 $ drop 4 $ ls !! 5
    rowF3 = take 3 $ drop 4 $ ls !! 6

    rowR1 = take 3 $ drop 8 $ ls !! 4
    rowR2 = take 3 $ drop 8 $ ls !! 5
    rowR3 = take 3 $ drop 8 $ ls !! 6

    rowB1 = take 3 $ drop 12 $ ls !! 4
    rowB2 = take 3 $ drop 12 $ ls !! 5
    rowB3 = take 3 $ drop 12 $ ls !! 6

    rowD1 = take 3 $ drop 4 $ ls !! 8
    rowD2 = take 3 $ drop 4 $ ls !! 9
    rowD3 = take 3 $ drop 4 $ ls !! 10

    [u1, u2, u3] = rowU1
    [u4, u5, u6] = rowU2
    [u7, u8, u9] = rowU3

    [f1, f2, f3] = rowF1
    [f4, f5, f6] = rowF2
    [f7, f8, f9] = rowF3

    [d1, d2, d3] = rowD1
    [d4, d5, d6] = rowD2
    [d7, d8, d9] = rowD3

    [r1, r2, r3] = rowR1
    [r4, r5, r6] = rowR2
    [r7, r8, r9] = rowR3

    [l1, l2, l3] = rowL1
    [l4, l5, l6] = rowL2
    [l7, l8, l9] = rowL3

    [b1, b2, b3] = rowB1
    [b4, b5, b6] = rowB2
    [b7, b8, b9] = rowB3

    centerF = Center FaceF f5
    centerB = Center FaceB b5
    centerL = Center FaceL l5
    centerR = Center FaceR r5
    centerU = Center FaceU u5
    centerD = Center FaceD d5

    edgeFL = Edge FaceF f4 FaceL l6
    edgeFR = Edge FaceF f6 FaceR r4
    edgeFU = Edge FaceF f2 FaceU u8
    edgeFD = Edge FaceF f8 FaceD d2

    cornerFLU = Corner FaceF f1 FaceL l3 FaceU u7
    cornerFRU = Corner FaceF f3 FaceR r1 FaceU u9
    cornerFLD = Corner FaceF f7 FaceL l9 FaceD d1
    cornerFRD = Corner FaceF f9 FaceR r7 FaceD d3

    edgeLU = Edge FaceL l2 FaceU u4
    edgeLD = Edge FaceL l8 FaceD d4
    edgeLB = Edge FaceL l4 FaceB b6

    edgeRU = Edge FaceR r2 FaceU u6
    edgeRD = Edge FaceR r8 FaceD d6
    edgeRB = Edge FaceR r6 FaceB b4

    cornerLUB = Corner FaceL l1 FaceU u1 FaceB b3
    cornerLDB = Corner FaceL l7 FaceD d7 FaceB b9

    cornerRUB = Corner FaceR r3 FaceU u3 FaceB b1
    cornerRDB = Corner FaceR r9 FaceD d9 FaceB b7

    edgeUB = Edge FaceU u2 FaceB b2
    edgeDB = Edge FaceD d8 FaceB b8

    edges :: [CubeElement]
    edges = [
        edgeUB,
        edgeDB,
        edgeFL,
        edgeFR,
        edgeFU,
        edgeFD,
        edgeLU,
        edgeLD,
        edgeLB,
        edgeRU,
        edgeRD,
        edgeRB
        ]

    corners = [
        cornerFLU,
        cornerFRU,
        cornerFLD,
        cornerFRD,
        cornerLUB,
        cornerLDB,
        cornerRUB,
        cornerRDB
        ]

    centers = [
        centerF,
        centerB,
        centerL,
        centerR,
        centerU,
        centerD
        ]

writeCubeToFile :: String -> Cube -> IO ()
writeCubeToFile fileName e = do
    writeFile fileName $ prettyCube e

-- CW clockwise
-- CCW counterclockwise
data Direction = CW | CCW deriving (Eq, Show)

-- notation:
-- https://ruwix.com/the-rubiks-cube/notation/

data Rotation = F | F' | B | B' | L | L' | R | R' | U | U' | D | D' deriving (Eq, Show)

allRotations :: [Rotation]
allRotations = [F, F', B, B', L, L', R, R', U, U', D, D']

type CubeAndRotations = (Cube, [Rotation]) -- TODO rename type

isCenter :: CubeElement -> Bool
isCenter (Center _ _) = True
isCenter _ = False

isEdge :: CubeElement -> Bool
isEdge (Edge _ _ _ _) = True
isEdge _ = False

isCorner :: CubeElement -> Bool
isCorner (Corner _ _ _ _ _ _ ) = True
isCorner _ = False


centers :: [CubeElement] -> [CubeElement]
centers = filter isCenter

edges :: [CubeElement] -> [CubeElement]
edges = filter isEdge

corners :: [CubeElement] -> [CubeElement]
corners = filter isCorner


onFace :: Face -> CubeElement -> Bool
onFace a (Center b _) = a == b
onFace a (Edge b _ c _) = a == b || a == c
onFace a (Corner b _ c _ d _) = elem a [b,c,d]

centerColor :: CubeElement -> Color
centerColor (Center _ c) = c

facesAndColors :: CubeElement -> [(Face, Color)]
facesAndColors (Center f c) = [(f,c)]
facesAndColors (Edge f1 c1 f2 c2) = [(f1,c1),(f2,c2)]
facesAndColors (Corner f1 c1 f2 c2 f3 c3) = [(f1,c1),(f2,c2),(f3,c3)]

colors :: CubeElement -> [Color]
colors e = map snd $ facesAndColors e

-- returns color on given face
color :: Face -> CubeElement -> Color
color f e = fromJust $ lookup f $ facesAndColors e

colorF = color FaceF
colorB = color FaceB
colorL = color FaceL
colorR = color FaceR
colorU = color FaceU
colorD = color FaceD

ofFace :: Face -> [CubeElement] -> [CubeElement]
ofFace face = filter (onFace face)

ofFaceF = ofFace FaceF
ofFaceB = ofFace FaceB
ofFaceL = ofFace FaceL
ofFaceR = ofFace FaceR
ofFaceU = ofFace FaceU
ofFaceD = ofFace FaceD

faceMap :: (Face -> Face) -> CubeElement -> CubeElement
faceMap g (Center face c) = Center (g face) c
faceMap g (Edge f1 c1 f2 c2) = Edge (g f1) c1 (g f2) c2
faceMap g (Corner f1 c1 f2 c2 f3 c3) = Corner (g f1) c1 (g f2) c2 (g f3) c3

rotateFrontFace :: Direction -> Cube -> Cube
rotateFrontFace r xs =
    notAffected ++ map (faceMap transition) affected
    where
    notAffected = filter (not . onFace FaceF) xs
    affected = ofFace FaceF xs

    transitionF f = case f of
        FaceF -> FaceF
        FaceB -> FaceB
        FaceL -> FaceU
        FaceR -> FaceD
        FaceU -> FaceR
        FaceD -> FaceL

    transitionF' f = case f of
        FaceF -> FaceF
        FaceB -> FaceB
        FaceL -> FaceD
        FaceR -> FaceU
        FaceU -> FaceL
        FaceD -> FaceR

    transition = case r of
        CW -> transitionF
        CCW -> transitionF'

rotateCubeTo :: Face -> Cube -> Cube
rotateCubeTo face xs =
    case face of
        FaceF -> xs
        FaceB -> map (faceMap toFaceB) xs
        FaceL -> map (faceMap toFaceL) xs
        FaceR -> map (faceMap toFaceR) xs
        FaceU -> map (faceMap toFaceU) xs
        FaceD -> map (faceMap toFaceD) xs
    where
        toFaceB f = case f of
            FaceF -> FaceB
            FaceB -> FaceF
            FaceL -> FaceR
            FaceR -> FaceL
            FaceU -> FaceU
            FaceD -> FaceD

        toFaceL f = case f of
            FaceF -> FaceR
            FaceB -> FaceL
            FaceL -> FaceF
            FaceR -> FaceB
            FaceU -> FaceU
            FaceD -> FaceD

        toFaceR f = case f of
            FaceF -> FaceL
            FaceB -> FaceR
            FaceL -> FaceB
            FaceR -> FaceF
            FaceU -> FaceU
            FaceD -> FaceD

        toFaceU f = case f of
            FaceF -> FaceD
            FaceB -> FaceU
            FaceL -> FaceL
            FaceR -> FaceR
            FaceU -> FaceF
            FaceD -> FaceB

        toFaceD f = case f of
            FaceF -> FaceU
            FaceB -> FaceD
            FaceL -> FaceL
            FaceR -> FaceR
            FaceU -> FaceB
            FaceD -> FaceF

undoRotateCubeTo :: Face -> Cube -> Cube
undoRotateCubeTo face = rotateCubeTo (reverseForRotationTo face)
    where
    reverseForRotationTo :: Face -> Face
    reverseForRotationTo face =
        case face of
            FaceF -> FaceF
            FaceB -> FaceB
            FaceL -> FaceR
            FaceR -> FaceL
            FaceU -> FaceD
            FaceD -> FaceU

rotateFace :: Face -> Direction -> Cube -> Cube
rotateFace face rot xs =
    undoRotateCubeTo face
    $ rotateFrontFace rot
    $ rotateCubeTo face xs


center :: Face -> [CubeElement] -> CubeElement
center FaceF = centerF
center FaceB = centerB
center FaceL = centerL
center FaceR = centerR
center FaceU = centerU
center FaceD = centerD

centerF = head . ofFaceF . centers
centerB = head . ofFaceB . centers
centerL = head . ofFaceL . centers
centerR = head . ofFaceR . centers
centerU = head . ofFaceU . centers
centerD = head . ofFaceD . centers

edgeFL = head . ofFaceF . ofFaceL . edges
edgeFR = head . ofFaceF . ofFaceR . edges
edgeFU = head . ofFaceF . ofFaceU . edges
edgeFD = head . ofFaceF . ofFaceD . edges

edgeLU = head . ofFaceL . ofFaceU . edges
edgeLD = head . ofFaceL . ofFaceD . edges
edgeLB = head . ofFaceL . ofFaceB . edges

edgeRU = head . ofFaceR . ofFaceU . edges
edgeRD = head . ofFaceR . ofFaceD . edges
edgeRB = head . ofFaceR . ofFaceB . edges

edgeUB = head . ofFaceU . ofFaceB . edges
edgeDB = head . ofFaceD . ofFaceB . edges

cornerFLU = head . ofFaceF . ofFaceL . ofFaceU . corners
cornerFRU = head . ofFaceF . ofFaceR . ofFaceU . corners
cornerFLD = head . ofFaceF . ofFaceL . ofFaceD . corners
cornerFRD = head . ofFaceF . ofFaceR . ofFaceD . corners

cornerLUB = head . ofFaceL . ofFaceU . ofFaceB . corners
cornerLDB = head . ofFaceL . ofFaceD . ofFaceB . corners

cornerRUB = head . ofFaceR . ofFaceU . ofFaceB . corners
cornerRDB = head . ofFaceR . ofFaceD . ofFaceB . corners

-------------------------------

prettyCube :: Cube -> String
prettyCube xs =
    "    " ++ [u1,u2,u3] ++ "\n" ++
    "    " ++ [u4,u5,u6] ++ "\n" ++
    "    " ++ [u7,u8,u9] ++ "\n" ++
    "\n" ++
    [l1,l2,l3] ++ " " ++ [f1,f2,f3] ++ " " ++ [r1,r2,r3] ++ " " ++ [b1,b2,b3] ++ "\n" ++
    [l4,l5,l6] ++ " " ++ [f4,f5,f6] ++ " " ++ [r4,r5,r6] ++ " " ++ [b4,b5,b6] ++ "\n" ++
    [l7,l8,l9] ++ " " ++ [f7,f8,f9] ++ " " ++ [r7,r8,r9] ++ " " ++ [b7,b8,b9] ++ "\n" ++
    "\n" ++
    "    " ++ [d1,d2,d3] ++ "\n" ++
    "    " ++ [d4,d5,d6] ++ "\n" ++
    "    " ++ [d7,d8,d9] ++ "\n"
    where
    f1 = colorF $ cornerFLU xs
    f2 = colorF $ edgeFU xs
    f3 = colorF $ cornerFRU xs
    f4 = colorF $ edgeFL xs
    f5 = colorF $ centerF xs
    f6 = colorF $ edgeFR xs
    f7 = colorF $ cornerFLD xs
    f8 = colorF $ edgeFD xs
    f9 = colorF $ cornerFRD xs

    b1 = colorB $ cornerRUB xs
    b2 = colorB $ edgeUB xs
    b3 = colorB $ cornerLUB xs
    b4 = colorB $ edgeRB xs
    b5 = colorB $ centerB xs
    b6 = colorB $ edgeLB xs
    b7 = colorB $ cornerRDB xs
    b8 = colorB $ edgeDB xs
    b9 = colorB $ cornerLDB xs

    l1 = colorL $ cornerLUB xs
    l2 = colorL $ edgeLU xs
    l3 = colorL $ cornerFLU xs
    l4 = colorL $ edgeLB xs
    l5 = colorL $ centerL xs
    l6 = colorL $ edgeFL xs
    l7 = colorL $ cornerLDB xs
    l8 = colorL $ edgeLD xs
    l9 = colorL $ cornerFLD xs

    r1 = colorR $ cornerFRU xs
    r2 = colorR $ edgeRU xs
    r3 = colorR $ cornerRUB xs
    r4 = colorR $ edgeFR xs
    r5 = colorR $ centerR xs
    r6 = colorR $ edgeRB xs
    r7 = colorR $ cornerFRD xs
    r8 = colorR $ edgeRD xs
    r9 = colorR $ cornerRDB xs

    u1 = colorU $ cornerLUB xs
    u2 = colorU $ edgeUB xs
    u3 = colorU $ cornerRUB xs
    u4 = colorU $ edgeLU xs
    u5 = colorU $ centerU xs
    u6 = colorU $ edgeRU xs
    u7 = colorU $ cornerFLU xs
    u8 = colorU $ edgeFU xs
    u9 = colorU $ cornerFRU xs

    d1 = colorD $ cornerFLD xs
    d2 = colorD $ edgeFD xs
    d3 = colorD $ cornerFRD xs
    d4 = colorD $ edgeLD xs
    d5 = colorD $ centerD xs
    d6 = colorD $ edgeRD xs
    d7 = colorD $ cornerLDB xs
    d8 = colorD $ edgeDB xs
    d9 = colorD $ cornerRDB xs


rotateF  = rotateFace FaceF CW
rotateF' = rotateFace FaceF CCW
rotateB  = rotateFace FaceB CW
rotateB' = rotateFace FaceB CCW
rotateL  = rotateFace FaceL CW
rotateL' = rotateFace FaceL CCW
rotateR  = rotateFace FaceR CW
rotateR' = rotateFace FaceR CCW
rotateU  = rotateFace FaceU CW
rotateU' = rotateFace FaceU CCW
rotateD  = rotateFace FaceD CW
rotateD' = rotateFace FaceD CCW

rotate :: Rotation -> [CubeElement] -> [CubeElement]
rotate F  = rotateF
rotate F' = rotateF'
rotate B  = rotateB
rotate B' = rotateB'
rotate L  = rotateL
rotate L' = rotateL'
rotate R  = rotateR
rotate R' = rotateR'
rotate U  = rotateU
rotate U' = rotateU'
rotate D  = rotateD
rotate D' = rotateD'

doRotations :: [Rotation] -> Cube -> Cube
doRotations r cube = foldl' (flip rotate) cube r

exampleSolvedCube :: [CubeElement]
exampleSolvedCube = [
    Center FaceF f,
    Center FaceB b,
    Center FaceL l,
    Center FaceR r,
    Center FaceU u,
    Center FaceD d,
    Edge FaceU u FaceB b,
    Edge FaceD d FaceB b,
    Edge FaceF f FaceL l,
    Edge FaceF f FaceR r,
    Edge FaceF f FaceU u,
    Edge FaceF f FaceD d,
    Edge FaceL l FaceU u,
    Edge FaceL l FaceD d,
    Edge FaceL l FaceB b,
    Edge FaceR r FaceU u,
    Edge FaceR r FaceD d,
    Edge FaceR r FaceB b,
    Corner FaceF f FaceL l FaceU u,
    Corner FaceF f FaceR r FaceU u,
    Corner FaceF f FaceL l FaceD d,
    Corner FaceF f FaceR r FaceD d,
    Corner FaceL l FaceU u FaceB b,
    Corner FaceL l FaceD d FaceB b,
    Corner FaceR r FaceU u FaceB b,
    Corner FaceR r FaceD d FaceB b
    ]
    where
        f = 'G'
        b = 'B'
        l = 'O'
        r = 'R'
        u = 'W'
        d = 'Y'

-- gets rotation from given face perspective
-- returns rotations from front face perspective
slowTranslateRotationFrom :: Face -> Rotation -> Rotation
slowTranslateRotationFrom face r =
    head [ a | a <- allRotations, let c = rotate a cube, c == after]
    where
    cube = exampleSolvedCube
    after =
        undoRotateCubeTo face
        $ rotate r
        $ rotateCubeTo face cube

translateLookupTable :: [(Face, [(Rotation, Rotation)])]
translateLookupTable =
    flip map allFaces
    $ \f -> (,) f
    $ [(r,r2) |  r <- allRotations, let r2 = slowTranslateRotationFrom f r]

-- the same as slowTranslateRotationFrom but uses lookup table for better performance
translateRotationFrom :: Face -> Rotation -> Rotation
translateRotationFrom f r = fromJust $ lookup r $ fromJust $ lookup f translateLookupTable

-- gets rotations from given face perspective
-- returns rotations from front face perspective
translateFrom :: Face -> [Rotation] -> [Rotation]
translateFrom face = map (translateRotationFrom face)


withCubeRotationTo :: Face -> Cube -> (Cube -> Either String CubeAndRotations) -> Either String CubeAndRotations
withCubeRotationTo face cube f = do
    let c = rotateCubeTo face cube
    (c2, rot) <- f c
    let c3 = undoRotateCubeTo face c2
    return (c3, translateFrom face rot)

withCubeUpsideDown :: Cube -> (Cube -> Either String CubeAndRotations) -> Either String CubeAndRotations
withCubeUpsideDown cube f = do
    let cubeUpsideDown = rotateCubeTo FaceD $ rotateCubeTo FaceD cube
    (cubeUpsideDown2, steps) <- f cubeUpsideDown
    let cube2 = undoRotateCubeTo FaceD $ undoRotateCubeTo FaceD cubeUpsideDown2
    let steps2 = translateFrom FaceD $ translateFrom FaceD steps
    return (cube2, steps2)


equal :: Eq a => [a] -> Bool
equal [] = True
equal (x:xs) = all (== x) xs

seemsValid :: Cube -> Either String ()
seemsValid cube = do
    if has8corners then Right () else Left "there should be 8 corners"
    if everyCornerHas3Colors then Right () else Left "every corner should have 3 colors"
    if has12edges then Right () else Left "there should be 12 edges"
    if everyEdgeHas2Colors then Right () else Left "every edge should have 2 colors"
    if has6centers then Right () else Left "there should be 6 centers"
    if centersHaveDistincColors then Right () else Left "centers should have distinc colors"
    if thereAre6colors then Right () else Left "there should be six colors"
    if has26elements then Right () else Left $ "there should be 26 elements but there are " ++ show (length cube)
    sixOccurencesOfColor $ centerColor $ centerF cube
    sixOccurencesOfColor $ centerColor $ centerB cube
    sixOccurencesOfColor $ centerColor $ centerL cube
    sixOccurencesOfColor $ centerColor $ centerR cube
    sixOccurencesOfColor $ centerColor $ centerU cube
    sixOccurencesOfColor $ centerColor $ centerD cube
    where
        has8corners = 8 == length (corners cube)
        everyCornerHas3Colors = all (\c -> 3 == length (nub (colors c))) (corners cube)
        has12edges = 12 == length (edges cube)
        everyEdgeHas2Colors = all (\e -> 2 == length (nub (colors e))) (edges cube)
        has6centers = 6 == length (centers cube)
        centersHaveDistincColors = 6 == (length $ nub $ map centerColor $ centers cube)
        colorsCount = length $ nub $ concatMap colors cube
        thereAre6colors = 6 == colorsCount
        has26elements = 26 == length cube
        sixOccurencesOfColor x =
            let occur = (length $ filter (== x) $ concatMap colors cube)
            in if 9 == occur then Right () else Left $ "there should be 9 occurences of color " ++ show x ++ "but there are " ++ show occur

checkStep1 :: Cube -> Bool
checkStep1 xs =
    uCrossOK
    && (centerColor $ centerF xs) == colorF (edgeFU xs)
    && (centerColor $ centerB xs) == colorB (edgeUB xs)
    && (centerColor $ centerL xs) == colorL (edgeLU xs)
    && (centerColor $ centerR xs) == colorR (edgeRU xs)
    where
        uCenterColor = centerColor $ centerU xs
        uElements = ofFaceU xs
        crossElements = filter (not . isCorner) uElements
        uCrossOK = all (== uCenterColor) $ map colorU crossElements

checkStep2 :: Cube -> Bool
checkStep2 cube = uFaceOK && adjacentOK
    where
        uFaceOK = equal $ map colorU $ ofFaceU cube
        uElements = ofFaceU cube
        adjacent = [FaceF, FaceB, FaceL, FaceR]
        faceOK f = equal $ map (color f) $ (center f cube) : ofFace f uElements
        adjacentOK = all faceOK adjacent

checkStep3 :: Cube -> Bool
checkStep3 cube =
    checkStep2 cube
    && secondLayerOK
    where
        adjacent = [FaceF, FaceB, FaceL, FaceR]
        secondLayerElements =
            filter (not . onFace FaceD)
            $ filter (not . onFace FaceU) cube
        faceOK f = equal
            $ map (color f)
            $ ofFace f secondLayerElements
        secondLayerOK = all faceOK adjacent


checkStep4 :: Cube -> Bool
checkStep4 cube =
    checkStep3 cube
    && crossOK
    where
        crossElements = filter (not . isCorner) $ ofFaceD cube
        crossOK = equal $ map colorD crossElements

checkStep5 :: Cube -> Bool
checkStep5 cube =
    checkStep4 cube
    && colorsOK
    where
        fs = [FaceF, FaceB, FaceL, FaceR]
        centerColors = flip map fs $ \f -> (f, centerColor $ center f cube)
        affectedEdges = edges $ ofFaceD cube
        edgeColorOnFace f = color f $ head $ ofFace f affectedEdges
        colorsOK = and $ flip map centerColors $ \(f,c) -> c == edgeColorOnFace f

checkStep6 :: Cube -> Bool
checkStep6 cube =
    checkStep5 cube
    && cornersOK
    where
        dCorners = ofFaceD $ corners cube
        pairs = [(FaceF,FaceL), (FaceF,FaceR), (FaceR,FaceB), (FaceL,FaceB)]
        pairOK (f1,f2) =
            let c1 = centerColor $ center f1 cube
                c2 = centerColor $ center f2 cube
                relevantCorner = head $ ofFace f2 $ ofFace f1 dCorners
                cornerColor1 = color f1 relevantCorner
                cornerColor2 = color f2 relevantCorner
                cornerColor3 = colorD relevantCorner
                cornerColors = [cornerColor1,cornerColor2,cornerColor3]
            in elem c1 cornerColors
                && elem c2 cornerColors
        cornersOK = all pairOK pairs

checkStep7 :: Cube -> Bool
checkStep7 cube =
    all faceOK allFaces
    && has6Colors
    where
        faceOK f = equal $ map (color f) $ ofFace f cube
        centerColors =
            flip map allFaces
            $ \f -> centerColor
            $ center f cube
        has6Colors = 6 == length (nub centerColors)

prepend :: [Rotation] -> CubeAndRotations -> CubeAndRotations
prepend rs (c,rs2) = (c, rs ++ rs2)

step1 :: Cube -> Either String CubeAndRotations
step1 cube = do
   (cube2, r1) <- step1fixFrontFace cube
   (cube3, r2) <- withCubeRotationTo FaceL cube2 step1fixFrontFace
   (cube4, r3) <- withCubeRotationTo FaceR cube3 step1fixFrontFace
   (cube5, r4) <- withCubeRotationTo FaceB cube4 step1fixFrontFace
   return (cube5, r1 ++ r2 ++ r3 ++ r4)

-- solves only front face of step1
-- algorithm
-- https://ruwix.com/the-rubiks-cube/how-to-solve-the-rubiks-cube-beginners-method/step-1-first-layer-edges/
step1fixFrontFace :: Cube -> Either String CubeAndRotations
step1fixFrontFace cube
    -- case: needed edge is on front face
    | faceOK = Right (cube, [])
    | rotateFisEnough = Right (rotate F cube, [F])
    | rotateF2isEnough = Right (rotate F $ rotate F cube, [F,F])
    | rotateF'isEnough = Right (rotate F' cube, [F'])
    | flipFUedge =
        let flipAlg = [F,U',R,U]
        in Right (doRotations flipAlg cube, flipAlg)
    | rotateFandRestart = restartAfter [F]
    | rotateF'andRestart = restartAfter [F']
    | rotateF2andRestart = restartAfter [F,F]
    -- case: needed edge is on down face 
    | rotateDandRestart = restartAfter [D]
    | rotateD'andRestart = restartAfter [D']
    | rotateD2andRestart = restartAfter [D,D]
    -- case: needed edge is on back face
    | rotateBDB'andRestart = restartAfter [B,D,B']
    | rotateB'DBandRestart = restartAfter [B',D,B]
    -- case: needed edge is on up face
    | rotateB2andRestart = restartAfter [B,B]
    | rotateL2andRestart = restartAfter [L,L]
    | rotateR2andRestart = restartAfter [R,R]
    | otherwise = Left "step1 failed"
    where
        uCenterColor = centerColor $ centerU cube
        fCenterColor = centerColor $ centerF cube
        e = edgeFU cube
        faceOK =
            uCenterColor == colorU e
            && fCenterColor == colorF e

        rotateFisEnough =
            let e = edgeFL cube
            in colorF e == fCenterColor && colorL e == uCenterColor

        rotateF2isEnough =
            let e = edgeFD cube
            in colorF e == fCenterColor && colorD e == uCenterColor

        rotateF'isEnough =
            let e = edgeFR cube
            in colorF e == fCenterColor && colorR e == uCenterColor

        flipFUedge =
            let e = edgeFU cube
            in fCenterColor == colorU e && uCenterColor == colorF e

        restartAfter :: [Rotation] -> Either String CubeAndRotations
        restartAfter alg =
            fmap (prepend alg) $ step1fixFrontFace $ doRotations alg cube

        needColors = [uCenterColor, fCenterColor]
        rotateFandRestart =
            let e = edgeFL cube
            in sort (colors e) == sort needColors

        rotateF'andRestart =
            let e = edgeFR cube
            in sort (colors e) == sort needColors

        rotateF2andRestart =
            let e = edgeFD cube
            in sort (colors e) == sort needColors

        rotateDandRestart =
            let e = edgeLD cube
            in sort (colors e) == sort needColors

        rotateD'andRestart =
            let e = edgeRD cube
            in sort (colors e) == sort needColors

        rotateD2andRestart =
            let e = edgeDB cube
            in sort (colors e) == sort needColors


        rotateBDB'andRestart =
            let e = edgeLB cube
            in sort (colors e) == sort needColors

        rotateB'DBandRestart =
            let e = edgeRB cube
            in sort (colors e) == sort needColors

        rotateB2andRestart =
            let e = edgeUB cube
            in sort (colors e) == sort needColors

        rotateL2andRestart =
            let e = edgeLU cube
            in sort (colors e) == sort needColors

        rotateR2andRestart =
            let e = edgeRU cube
            in sort (colors e) == sort needColors

step2 :: Cube -> Either String CubeAndRotations
step2 cube = do
    (c2, r1) <- frontTopRightCornerFix cube
    (c3, r2) <- withCubeRotationTo FaceL c2 frontTopRightCornerFix
    (c4, r3) <- withCubeRotationTo FaceR c3 frontTopRightCornerFix
    (c5, r4) <- withCubeRotationTo FaceB c4 frontTopRightCornerFix
    return (c5, r1 ++ r2 ++ r3 ++ r4)

-- this function fixes only one corner: front top right corner
frontTopRightCornerFix :: Cube -> Either String CubeAndRotations
frontTopRightCornerFix cube
    | cornerOK = Right (cube,[])
    | iterateAlg = restartAfter alg
    -- case: corner is on down face
    | rotateDandRestart = restartAfter [D]
    | rotateD'andRestart = restartAfter [D']
    | rotateD2andRestart = restartAfter [D,D]
    -- case: corner is on up face
    | moveFRUcornerDownAndRestart = restartAfter moveFRUcornerDownAlg
    | neededCornerIsFLU = do
        let face = FaceL
        (cube2, r) <- withCubeRotationTo face cube $ \c -> Right (doRotations moveFRUcornerDownAlg c, moveFRUcornerDownAlg)
        fmap (prepend r) $ frontTopRightCornerFix cube2
    | neededCornerIsRUB = do
        let face = FaceR
        (cube2, r) <- withCubeRotationTo face cube $ \c -> Right (doRotations moveFRUcornerDownAlg c, moveFRUcornerDownAlg)
        fmap (prepend r) $ frontTopRightCornerFix cube2
    | neededCornerIsLUB = do
        let face = FaceB
        (cube2, r) <- withCubeRotationTo face cube $ \c -> Right (doRotations moveFRUcornerDownAlg c, moveFRUcornerDownAlg)
        fmap (prepend r) $ frontTopRightCornerFix cube2
    | otherwise = Left "step2 failed"
    where
        uCenterColor = centerColor $ centerU cube
        fCenterColor = centerColor $ centerF cube
        rCenterColor = centerColor $ centerR cube
        furCorner = cornerFRU cube
        cornerOK =
            colorU furCorner == uCenterColor
            && colorF furCorner == fCenterColor
            && colorR furCorner == rCenterColor

        needColors = [uCenterColor, fCenterColor, rCenterColor]
        frdCorner = cornerFRD cube
        alg = [R',D',R,D]
        iterateAlg = sort (colors frdCorner) == sort needColors

        restartAfter rot =
            fmap (prepend rot) $ frontTopRightCornerFix $ doRotations rot cube

        rotateDandRestart =
            let c = cornerFLD cube
            in sort (colors c) == sort needColors
        rotateD'andRestart =
            let c = cornerRDB cube
            in sort (colors c) == sort needColors
        rotateD2andRestart =
            let c = cornerLDB cube
            in sort (colors c) == sort needColors

        moveFRUcornerDownAlg = [R',D',R,D]
        moveFRUcornerDownAndRestart =
            let c = cornerFRU cube
            in sort (colors c) == sort needColors

        neededCornerIsFLU =
            let c = cornerFLU cube
            in sort (colors c) == sort needColors

        neededCornerIsRUB =
            let c = cornerRUB cube
            in sort (colors c) == sort needColors

        neededCornerIsLUB =
            let c = cornerLUB cube
            in sort (colors c) == sort needColors


step3 :: Cube -> Either String CubeAndRotations
step3 cube = withCubeUpsideDown cube solveSecondLayerAllFaces

-- precondition: solved step2 AND cube rotated upside down!
solveSecondLayerAllFaces :: Cube -> Either String CubeAndRotations
solveSecondLayerAllFaces c = do
    (c1,r1) <- solveSecondLayerFrontFace c
    (c2,r2) <- withCubeRotationTo FaceL c1 solveSecondLayerFrontFace
    (c3,r3) <- withCubeRotationTo FaceR c2 solveSecondLayerFrontFace
    (c4,r4) <- withCubeRotationTo FaceB c3 solveSecondLayerFrontFace
    return (c4, r1 ++ r2 ++ r3 ++ r4)

-- solves FR edge
-- precondition: solved step2 AND cube rotated upside down!
solveSecondLayerFrontFace :: Cube -> Either String CubeAndRotations
solveSecondLayerFrontFace cube
    | eFRok = Right (cube, [])
    | canDoRightAlg =
        let alg = rightAlg
        in Right (doRotations alg cube, alg)
    | wrongOrientation =
        let alg = wrongOrientationAlg
        in Right (doRotations alg cube, alg)
    | needEdgeFU =
        let alg = rightAlg
        in fmap (prepend alg) $ solveSecondLayerFrontFace $ doRotations alg cube
    -- where is needed edge? 
    -- edge is in up layer. Place edge to FU
    | needEdgeLU = tryAgainAfter [U']
    | needEdgeRU = tryAgainAfter [U]
    | needEdgeUB = tryAgainAfter [U,U]
    -- edges is in middle layer 
    | needEdgeFL = do
        (cube2,r) <- moveFRedgeToFUafterRotationTo FaceL
        (cube3,r2) <- solveSecondLayerFrontFace cube2
        return (cube3, r ++ r2)
    | needEdgeRB = do
        (cube2,r) <- moveFRedgeToFUafterRotationTo FaceR
        (cube3,r2) <- solveSecondLayerFrontFace cube2
        return (cube3, r ++ r2)
    | needEdgeLB = do
        (cube2,r) <- moveFRedgeToFUafterRotationTo FaceB
        (cube3,r2) <- solveSecondLayerFrontFace cube2
        return (cube3, r ++ r2)
    | otherwise = Left "step3 failed"
    where
        fCenterColor = centerColor $ centerF cube
        lCenterColor = centerColor $ centerL cube
        rCenterColor = centerColor $ centerR cube

        eFL = edgeFL cube
        eFR = edgeFR cube
        eFU = edgeFU cube
        eLU = edgeLU cube
        eRU = edgeRU cube
        eUB = edgeUB cube
        eLB = edgeLB cube
        eRB = edgeRB cube

        eFRok = colorF eFR == fCenterColor && colorR eFR == rCenterColor

        rightAlg = [U,R,U',R',U',F',U,F]

        canDoRightAlg =
            colorF eFU == fCenterColor
            && colorU eFU == rCenterColor

        needColorsForEdgeFR = [rCenterColor, fCenterColor]

        wrongOrientationAlg = rightAlg ++ [U,U] ++ rightAlg
        edgeFRtoFUalg = rightAlg ++ [U,U]

        wrongOrientation =
            let e = eFR
            in colorF e == rCenterColor
                && colorR e == fCenterColor

        needEdgeFU = sort needColorsForEdgeFR == sort (colors eFU)

        needEdgeLU = sort needColorsForEdgeFR == sort (colors eLU)
        needEdgeRU = sort needColorsForEdgeFR == sort (colors eRU)
        needEdgeUB = sort needColorsForEdgeFR == sort (colors eUB)

        needEdgeLB = sort needColorsForEdgeFR == sort (colors eLB)
        needEdgeFL = sort needColorsForEdgeFR == sort (colors eFL)
        needEdgeRB = sort needColorsForEdgeFR == sort (colors eRB)

        tryAgainAfter alg =
            fmap (prepend alg) $ solveSecondLayerFrontFace $ doRotations alg cube

        moveFRedgeToFUafterRotationTo face =
            withCubeRotationTo face cube
                $ \c -> let a = edgeFRtoFUalg
                        in Right (doRotations a c, a)


step4 :: Cube -> Either String CubeAndRotations
step4 cube = withCubeUpsideDown cube anotherCross

-- precondition: cube rotated upsidedown! 
anotherCross :: Cube -> Either String CubeAndRotations
anotherCross cube
    | crossOK = Right (cube,[])
    | horizontalLine = Right (doRotations alg cube, alg)
    | verticalLine = withCubeRotationTo FaceL cube $ anotherCross
    | bendShapeLB = fmap (prepend alg) $ anotherCross $ doRotations alg cube
    | bendShapeRB = withCubeRotationTo FaceL cube $ anotherCross
    | bendShareFR = withCubeRotationTo FaceB cube $ anotherCross
    | bendShareFL = withCubeRotationTo FaceR cube $ anotherCross
    | otherwise = fmap (prepend alg) $ anotherCross $ doRotations alg cube
    where
        crossOK = equal $ map colorU $ filter (not . isCorner) $ ofFaceU cube
        horizontalLine = equal $ map colorU $ [centerU cube, edgeLU cube, edgeRU cube]
        verticalLine = equal $ map colorU $ [centerU cube, edgeFU cube, edgeUB cube]
        bendShapeLB = equal $ map colorU $ [centerU cube, edgeLU cube, edgeUB cube]
        bendShapeRB = equal $ map colorU $ [centerU cube, edgeRU cube, edgeUB cube]
        bendShareFR = equal $ map colorU $ [centerU cube, edgeFU cube, edgeRU cube]
        bendShareFL = equal $ map colorU $ [centerU cube, edgeFU cube, edgeLU cube]

        alg = [F,R,U,R',U',F']

step5 :: Cube -> Either String CubeAndRotations -- TODO FIX 
step5 cube = withCubeUpsideDown cube swapEdges

-- precondition: cube rotated upsidedown
swapEdges :: Cube -> Either String CubeAndRotations
swapEdges cube = do
    (c1,r1) <- fixFUedge cube
    (c2,r2) <- withCubeRotationTo FaceL c1 fixFUedge
    (c3,r3) <- withCubeRotationTo FaceR c2 fixFUedge
    (c4,r4) <- withCubeRotationTo FaceB c3 fixFUedge
    return (c4, r1 ++ r2 ++ r3 ++ r4)

-- precondition: cube rotated upsidedown
fixFUedge :: Cube -> Either String CubeAndRotations
fixFUedge cube
    | fuEdgeOK = Right (cube, [])
    | switchEdgesFUandLU =
        let alg = switchEdgesFUandLUalg
        in Right (doRotations alg cube, alg)
    | switchEdgesFUandRU =
        let alg = switchEdgesFUandRUalg
        in Right (doRotations alg cube, alg)
    | switchEdgesFUandUB =
        let alg = switchEdgesFUandUBalg
        in Right (doRotations alg cube, alg)
    | otherwise = Left "step5 failed"
    where
        fuEdge = edgeFU cube
        fuEdgeOK = centerColor (centerF cube) == colorF fuEdge
        switchEdgesFUandLUalg = [R,U,R',U,R,U,U,R',U]
        switchEdgesFUandRUalg = [B,U,B',U,B,U,U,B',U]
        switchEdgesFUandUBalg = [U,B,U,B',U,B,U,U,B',U,F,U,F',U,F,U,U,F',U]

        luEdge = edgeLU cube
        ruEdge = edgeRU cube
        ubEdge = edgeUB cube

        lCenterColor = centerColor $ centerL cube
        fCenterColor = centerColor $ centerF cube
        rCenterColor = centerColor $ centerR cube
        bCenterColor = centerColor $ centerB cube

        switchEdgesFUandLU = colorL luEdge == fCenterColor
        switchEdgesFUandRU = colorR ruEdge == fCenterColor
        switchEdgesFUandUB = colorB ubEdge == fCenterColor

step6 :: Cube -> Either String CubeAndRotations
step6 cube = withCubeUpsideDown cube positionCorners

-- precondition: cube rotated upsidedown
-- "
-- Due to the parity the amount of the correctly positioned yellow cubelets 
-- is limited to three cases: 
-- there's no yellow corner piece in the correct position,
-- or there's only one, 
-- or all the four pieces are correct.
-- "
positionCorners :: Cube -> Either String CubeAndRotations
positionCorners cube
    | cornersOK = Right (cube, [])
    | cFRUok =
        fmap (prepend alg) $ positionCorners $ doRotations alg cube
    | cFLUok =
        withCubeRotationTo FaceL cube positionCorners
    | cLUBok =
        withCubeRotationTo FaceB cube positionCorners
    | cRUBok =
        withCubeRotationTo FaceR cube positionCorners
    | otherwise =
        fmap (prepend alg) $ positionCorners $ doRotations alg cube
    where
        cFRU = cornerFRU cube
        cFLU = cornerFLU cube
        cLUB = cornerLUB cube
        cRUB = cornerRUB cube

        fCenterColor = centerColor $ centerF cube
        bCenterColor = centerColor $ centerB cube
        lCenterColor = centerColor $ centerL cube
        rCenterColor = centerColor $ centerR cube
        uCenterColor = centerColor $ centerU cube

        needColorsFRU = [fCenterColor, rCenterColor, uCenterColor]
        needColorsFLU = [fCenterColor, lCenterColor, uCenterColor]
        needColorsLUB = [lCenterColor, uCenterColor, bCenterColor]
        needColorsRUB = [rCenterColor, uCenterColor, bCenterColor]

        cFRUok = sort (colors cFRU) == sort needColorsFRU
        cFLUok = sort (colors cFLU) == sort needColorsFLU
        cLUBok = sort (colors cLUB) == sort needColorsLUB
        cRUBok = sort (colors cRUB) == sort needColorsRUB

        cornersOK = cFRUok && cFLUok && cLUBok && cRUBok

        alg = [U,R,U',L',U,R',U',L]

step7 :: Cube -> Either String CubeAndRotations
step7 cube = withCubeUpsideDown cube orientCorners

-- preconition: cube rotated upsidedown
orientCorners :: Cube -> Either String CubeAndRotations
orientCorners cube
    | uFaceOK =
        if checkStep7 cube -- TODO use local function
        then Right (cube, [])
        else fmap (prepend [U]) $ orientCorners $ rotateU cube
    | uCenterColor == fruCornerColorU =
         fmap (prepend [U]) $ orientCorners $ rotateU cube
    | otherwise =
        fmap (prepend alg) $ orientCorners $ doRotations alg cube
    where
        uFaceOK = equal $ map colorU $ ofFaceU cube
        uCenterColor = centerColor $ centerU cube
        fruCornerColorU = colorU $ cornerFRU cube

        alg = [R',D',R,D]

solve :: Cube -> Either String CubeAndRotations
solve c = do
    (c1,r1) <- step1 c
    (c2,r2) <- step2 c1
    (c3,r3) <- step3 c2
    (c4,r4) <- step4 c3
    (c5,r5) <- step5 c4
    (c6,r6) <- step6 c5
    (c7,r7) <- step7 c6
    return (c7, r1 ++ r2 ++ r3 ++ r4 ++ r5 ++ r6 ++ r7 )

solveChecked :: Cube -> Either String CubeAndRotations
solveChecked c = do
    seemsValid c
    (c1,r1) <- step1 c
    check c1 checkStep1 "step 1 result is invalid"
    checkRotations r1 c c1 "rotations from step 1 are invalid"

    (c2,r2) <- step2 c1
    check c2 checkStep2 "step 2 result is invalid"
    checkRotations r2 c1 c2 "rotations from step 2 are invalid"

    (c3,r3) <- step3 c2
    check c3 checkStep3 "step 3 result is invalid"
    checkRotations r3 c2 c3 "rotations from step 3 are invalid"

    (c4,r4) <- step4 c3
    check c4 checkStep4 "step 4 result is invalid"
    checkRotations r4 c3 c4 "rotations from step 4 are invalid"

    (c5,r5) <- step5 c4
    check c5 checkStep5 "step 5 result is invalid"
    checkRotations r5 c4 c5 "rotations from step 5 are invalid"

    (c6,r6) <- step6 c5
    check c6 checkStep6 "step 6 result is invalid"
    checkRotations r6 c5 c6 "rotations from step 6 are invalid"

    (c7,r7) <- step7 c6
    check c7 checkStep7 "step 7 result is invalid"
    checkRotations r7 c6 c7 "rotations from step 7 are invalid"

    let rotations = r1 ++ r2 ++ r3 ++ r4 ++ r5 ++ r6 ++ r7
    checkRotations rotations c c7 "rotations error"

    return (c7, rotations)
    where
        check cube f failMsg =
            if f cube
            then return ()
            else Left failMsg
        checkRotations rot inCube outCube msg =
            if doRotations rot inCube == outCube
            then return ()
            else Left msg


transformCube :: Cube -> [Color]
transformCube xs =
    [f1,f2,f3,f4,f5,f6,f7,f8,f9]
    ++ [b1,b2,b3,b4,b5,b6,b7,b8,b9]
    ++ [l1,l2,l3,l4,l5,l6,l7,l8,l9]
    ++ [r1,r2,r3,r4,r5,r6,r7,r8,r9]
    ++ [u1,u2,u3,u4,u5,u6,u7,u8,u9]
    ++ [d1,d2,d3,d4,d5,d6,d7,d8,d9]
    where
    f1 = colorF $ cornerFLU xs
    f2 = colorF $ edgeFU xs
    f3 = colorF $ cornerFRU xs
    f4 = colorF $ edgeFL xs
    f5 = colorF $ centerF xs
    f6 = colorF $ edgeFR xs
    f7 = colorF $ cornerFLD xs
    f8 = colorF $ edgeFD xs
    f9 = colorF $ cornerFRD xs

    b1 = colorB $ cornerRUB xs
    b2 = colorB $ edgeUB xs
    b3 = colorB $ cornerLUB xs
    b4 = colorB $ edgeRB xs
    b5 = colorB $ centerB xs
    b6 = colorB $ edgeLB xs
    b7 = colorB $ cornerRDB xs
    b8 = colorB $ edgeDB xs
    b9 = colorB $ cornerLDB xs

    l1 = colorL $ cornerLUB xs
    l2 = colorL $ edgeLU xs
    l3 = colorL $ cornerFLU xs
    l4 = colorL $ edgeLB xs
    l5 = colorL $ centerL xs
    l6 = colorL $ edgeFL xs
    l7 = colorL $ cornerLDB xs
    l8 = colorL $ edgeLD xs
    l9 = colorL $ cornerFLD xs

    r1 = colorR $ cornerFRU xs
    r2 = colorR $ edgeRU xs
    r3 = colorR $ cornerRUB xs
    r4 = colorR $ edgeFR xs
    r5 = colorR $ centerR xs
    r6 = colorR $ edgeRB xs
    r7 = colorR $ cornerFRD xs
    r8 = colorR $ edgeRD xs
    r9 = colorR $ cornerRDB xs

    u1 = colorU $ cornerLUB xs
    u2 = colorU $ edgeUB xs
    u3 = colorU $ cornerRUB xs
    u4 = colorU $ edgeLU xs
    u5 = colorU $ centerU xs
    u6 = colorU $ edgeRU xs
    u7 = colorU $ cornerFLU xs
    u8 = colorU $ edgeFU xs
    u9 = colorU $ cornerFRU xs

    d1 = colorD $ cornerFLD xs
    d2 = colorD $ edgeFD xs
    d3 = colorD $ cornerFRD xs
    d4 = colorD $ edgeLD xs
    d5 = colorD $ centerD xs
    d6 = colorD $ edgeRD xs
    d7 = colorD $ cornerLDB xs
    d8 = colorD $ edgeDB xs
    d9 = colorD $ cornerRDB xs

main :: IO ()
main = do
    args <- getArgs
    let defaultFileName = "cube.txt"
    let fileName = if null args then defaultFileName else head args
    putStrLn $ "reading from file " ++ fileName
    cube <- cubeFromFile fileName

    let inputValid = seemsValid cube
    case inputValid of
        (Right _) -> do
            putStrLn "input cube seems valid"
            case solveChecked cube of
                (Left msg) -> putStrLn $ "solving failed with message: " ++ msg
                (Right (c,rot)) -> do
                    putStrLn "solving succeded."
                    putStrLn "result cube: "
                    putStrLn $ prettyCube c
                    putStrLn "rotation count: "
                    putStrLn $ show $ length rot
                    putStrLn "result rotations: "
                    putStrLn $ show rot
        (Left msg) -> do
            putStrLn $ "input cube is invalid: " ++ msg
            putStrLn "try again with valid cube"


funnyTest :: Bool
funnyTest =
    transformCube cube == transformCube after1260
    where
        cube = exampleSolvedCube
        alg = [R,U,U,D',B,D']
        rotations = concat $ replicate 1260 alg
        after1260 = doRotations rotations cube

funnyTest2 :: Integer
funnyTest2 = i
    where
    cube = exampleSolvedCube
    alg = [R,U,U,D',B,D']
    comp x y = transformCube x /= transformCube y

    (i,_) = head $ dropWhile (comp cube . snd) $ tail $ zip [0..] $ iterate (doRotations alg) cube


-- for QuickCheck

instance Arbitrary Rotation where
    arbitrary = oneof (map return allRotations)

prop_solve scramble = within 5000000 $
    collect (length scramble) $
    case result of
        (Left _) -> False
        (Right (outCube, sol)) ->
            checkStep7 outCube && outCube == doRotations sol inCube
    where
        inCube = doRotations scramble exampleSolvedCube
        result = solve inCube
