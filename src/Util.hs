module Util (
    (</>)
) where

import qualified System.Posix.FilePath as P ((</>))

import Types (Name(..), Path(..))

--------------------------------------------------------------------------------
infixr 5 </>

(</>) :: Path -> Name -> Path
(Path p) </> (Name n) = Path (p P.</> n)
