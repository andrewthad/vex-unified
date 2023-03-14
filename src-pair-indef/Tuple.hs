signature Tuple where

import qualified ArrayA as A
import qualified ArrayB as B

data Element :: 'TupleRep '[A.R, B.R]

first :: Element -> TYPE
