module Object where

import Vector3
import Vertex
import Primitive
import Material
import Intersectable

type Objects = [Object]

data Object = MeshObject Vertices Primitives deriving (Show, Read)

instance Intersectable Object where
  hitDistance ray (MeshObject v p) = Nothing
  intersect ray (MeshObject v p) = Nothing

makeBox :: Double -> Double -> Double -> Material -> Object
makeBox x y z mtl = do
  -- define the corners of the box
  let p000 = Vector3 (-x) (-y) (-z)
  let p001 = Vector3 (-x) (-y) z
  let p010 = Vector3 (-x) y (-z)
  let p011 = Vector3 (-x) y z
  let p100 = Vector3 x (-y) (-z)
  let p101 = Vector3 x (-y) z
  let p110 = Vector3 x y (-z)
  let p111 = Vector3 x y z

  -- define texcoords
  let t00 = origin
  let t01 = yaxis
  let t10 = xaxis
  let t11 = xaxis + yaxis

  -- right face
  let rightVertices = foldl (\acc (p, t) -> acc ++ [Vertex p xaxis t]) [] $ zip [p101, p100, p110, p111] [t00, t10, t11, t01]
  let rightTriangles = [Triangle (rightVertices !! 0) (rightVertices !! 1) (rightVertices !! 2) mtl] ++ [Triangle (rightVertices !! 0) (rightVertices !! 2) (rightVertices !! 3) mtl]

  -- left face
  let leftVertices = foldl (\acc (p, t) -> acc ++ [Vertex p (negate xaxis) t]) [] $ zip [p000, p001, p011, p010] [t00, t10, t11, t01]
  let leftTriangles = [Triangle (leftVertices !! 0) (leftVertices !! 1) (leftVertices !! 2) mtl] ++ [Triangle (leftVertices !! 0) (leftVertices !! 2) (leftVertices !! 3) mtl]

  -- top face
  let topVertices = foldl (\acc (p, t) -> acc ++ [Vertex p yaxis t]) [] $ zip [p011, p111, p110, p010] [t00, t10, t11, t01]
  let topTriangles = [Triangle (topVertices !! 0) (topVertices !! 1) (topVertices !! 2) mtl] ++ [Triangle (topVertices !! 0) (topVertices !! 2) (topVertices !! 3) mtl]

  -- bottom face
  let bottomVertices = foldl (\acc (p, t) -> acc ++ [Vertex p (negate yaxis) t]) [] $ zip [p000, p100, p101, p001] [t00, t10, t11, t01]
  let bottomTriangles = [Triangle (bottomVertices !! 0) (bottomVertices !! 1) (bottomVertices !! 2) mtl] ++ [Triangle (bottomVertices !! 0) (bottomVertices !! 2) (bottomVertices !! 3) mtl]

  -- front face
  let frontVertices = foldl (\acc (p, t) -> acc ++ [Vertex p zaxis t]) [] $ zip [p001, p101, p111, p011] [t00, t10, t11, t01]
  let frontTriangles = [Triangle (frontVertices !! 0) (frontVertices !! 1) (frontVertices !! 2) mtl] ++ [Triangle (frontVertices !! 0) (frontVertices !! 2) (frontVertices !! 3) mtl]

  -- back face
  let backVertices = foldl (\acc (p, t) -> acc ++ [Vertex p (negate zaxis) t]) [] $ zip [p100, p000, p010, p110] [t00, t10, t11, t01]
  let backTriangles = [Triangle (backVertices !! 0) (backVertices !! 1) (backVertices !! 2) mtl] ++ [Triangle (backVertices !! 0) (backVertices !! 2) (backVertices !! 3) mtl]

  let allVertices = leftVertices ++ rightVertices ++ topVertices ++ bottomVertices ++ frontVertices ++ backVertices

  let allTriangles = leftTriangles ++ rightTriangles ++ topTriangles ++ bottomTriangles ++ frontTriangles ++ backTriangles

  MeshObject allVertices allTriangles