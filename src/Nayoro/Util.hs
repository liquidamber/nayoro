module
  Nayoro.Util
  ( sinkVector
  ) where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Conduit as C
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GM

-- TODO too slow since Vector's grow creates new vector again and again
sinkVector :: (MonadIO m, GV.Vector v a) => C.Consumer a m (v a)
sinkVector = do
  (liftIO . GM.new) 0 >>= loop >>= (liftIO . GV.freeze)
    where
      loop v = do
        mx <- C.await
        case mx of
          Nothing -> return v
          Just x -> do
            let i = GM.length v
            v <- liftIO $ GM.grow v 1
            liftIO $ GM.write v i x
            loop v
