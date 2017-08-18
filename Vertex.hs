module Vertex where

import Vector3

-- Vertex is representated by position, normal and texcoord
-- hence the three Vector3 elements

type Vertices = [Vertex]

data Vertex = Vertex Vector3 Vector3 Vector3 deriving (Show, Read, Eq)
