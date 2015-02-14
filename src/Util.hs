module Util (
    (</>),
    P.combine
) where

import qualified System.Posix.FilePath as P ((</>), combine)

import Types (Name(..), Path(..))

--------------------------------------------------------------------------------
infixr 5 </>

(</>) :: Path -> Name -> Path
(Path p) </> (Name n) = Path (p P.</> n)
