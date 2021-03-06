module JTL.Context (
    Context, fromValue, withKey, withValue, withMember, fromDocument, getValue, lookupKey, getKey,
    lookupKeyContext
    ) where

import qualified JTL.Value as V

data Context = Context { getValue :: V.Value, lookupKey :: Maybe V.Value } deriving Show

fromValue v = Context (V.toValue v) Nothing
fromDocument v = Context v Nothing
withKey k (Context v _) = Context v $ Just k
withValue v (Context _ k) = Context v k
withMember k v (Context _ _) = Context v $ Just k
getKey (Context _ (Just k)) = k
lookupKeyContext (Context _ k) = case k of
    Nothing -> Nothing
    Just k -> Just $ Context k Nothing
