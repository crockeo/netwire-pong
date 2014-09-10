module Network (runNetwork) where

--------------------
-- Global Imports --
import Data.IORef

----------
-- Code --

{-|
  The front end of running the network. All of the grunt work is actually done
  by runNetwork'.
-}
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = print "Nothing here yet."
