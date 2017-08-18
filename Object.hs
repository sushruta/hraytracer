module Object where

import Vertex
import Primitive
import Intersectable
 
type Objects = [Object]

data Object = MeshObject Vertices Primitives deriving (Show, Read)

instance Intersectable Object where
  intersect ray (MeshObject v p) = Nothing
