{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, IncoherentInstances #-}

module JTL.Value (
    Boolean, Number, String, Array, Object, Value(..),
    toBoolean, fromBoolean, true, false,
    toNumber, fromNumber, zero, one, mod,
    toString, fromString, epsilon, concat,
    toArray, fromArray,
    toObject, fromObject, lookupObjectMember,
    members,
    ValueLike(..)
    ) where

import Data.Fixed (mod')
import Control.Monad (forM)
import Data.Generics (Data)
import Data.Typeable (Typeable)
import Prelude hiding (String, concat, mod)
import qualified Text.JSON as J
import qualified Data.Map as M

newtype Boolean = Boolean Bool deriving (Eq, Ord, Typeable, Data)
newtype Number = Number Rational deriving (Eq, Ord, Typeable, Data)
newtype String = String [Char] deriving (Eq, Ord, Typeable, Data)
newtype Array = Array [Value] deriving (Eq, Ord, Typeable, Data)
newtype Object = Object (M.Map String Value) deriving (Eq, Ord, Typeable, Data)

data Value =
      VNull
    | VBoolean Boolean
    | VNumber Number
    | VString String
    | VArray Array
    | VObject Object
    deriving (Eq, Ord, Typeable, Data)

toBoolean b = Boolean b
fromBoolean (Boolean b) = b

true = Boolean True
false = Boolean False

toNumber r = Number $ toRational r
fromNumber (Number r) = r

zero = Number $ fromIntegral 0
one = Number $ fromIntegral 1

toString s = String s
fromString (String s) = s

epsilon = String ""

toArray a = Array a
fromArray (Array a) = a

toObject o = Object $ M.fromList o
fromObject (Object o) = M.toAscList o

lookupObjectMember k (Object o) = M.lookup k o

members (VArray (Array a)) = zip (map (VNumber . Number . fromIntegral) [0..]) a
members (VObject o) = map (\(k, v) -> (VString k, v)) $ fromObject o
members _ = []

instance Num Number where
    (Number a) + (Number b) = Number $ a + b
    (Number a) - (Number b) = Number $ a - b
    (Number a) * (Number b) = Number $ a * b
    negate (Number a) = Number $ negate a
    abs (Number a) = Number $ abs a
    signum (Number a) = Number $ signum a
    fromInteger = Number . fromInteger

instance Fractional Number where
    (Number a) / (Number b) = Number $ a / b
    recip (Number a) = Number $ recip a
    fromRational = Number . fromRational

(Number a) `mod` (Number b) = Number (if a < 0 then negate $ negate a `mod'` abs b else a `mod'` abs b)

concat (String s1) (String s2) = String $ s1 ++ s2

instance Show Value where
    show VNull = "null"
    show (VBoolean b) = show b
    show (VNumber n) = show n
    show (VString s) = show s
    show (VArray a) = show a
    show (VObject o) = show o

instance Show Boolean where
    show (Boolean False) = "false"
    show (Boolean True) = "true"

instance Show Number where
    show (Number n) = let s = show ((fromRational n) :: Double) in case reverse s of
        '0':'.':s' -> reverse s'
        _ -> s
    
instance Show String where
    show (String s) = show s

instance Show Array where
    show (Array a) = "[" ++ (join ", " $ map show a) ++ "]"

instance Show Object where
    show (Object o) = "{" ++ (join ", " $ map go $ M.toAscList o) ++ "}" where
        go (k, v) = (show k) ++ ": " ++ (show v)

readJSON' J.JSNull = VNull
readJSON' (J.JSBool b) = VBoolean $ Boolean b
readJSON' (J.JSRational _ r) = VNumber $ Number r
readJSON' (J.JSString s) = VString $ String $ J.fromJSString s
readJSON' (J.JSArray a) = VArray $ Array $ map readJSON' a
readJSON' (J.JSObject o) = VObject $ Object $ M.fromList $ map go $ J.fromJSObject o where
    go (s, v) = (String s, readJSON' v)

instance J.JSON Value where
    readJSON v = J.Ok $ readJSON' v
    
    showJSON VNull = J.JSNull
    showJSON (VBoolean (Boolean b)) = J.JSBool b
    showJSON (VNumber (Number n)) = J.JSRational False n
    showJSON (VString (String s)) = J.JSString $ J.toJSString s
    showJSON (VArray (Array a)) = J.JSArray $ map J.showJSON a
    showJSON (VObject (Object o)) = J.JSObject $ J.toJSObject $ map go $ M.toAscList o where
        go (String s, v) = (s, J.showJSON v)


join sep [] = []
join sep [a] = a
join sep (a:b:as) = a ++ sep ++ join sep (b:as)


class ValueLike a where
    toValue :: a -> Value
    tryFromValue :: Value -> Either [Char] a
    fromValue :: Value -> a
    fromValue v = case tryFromValue v of
        Left msg -> error msg
        Right v' -> v'

instance ValueLike Boolean where
    toValue = VBoolean
    tryFromValue (VBoolean b) = Right b
    tryFromValue _ = Left "Expecting boolean"

instance ValueLike Number where
    toValue = VNumber
    tryFromValue (VNumber n) = Right n
    tryFromValue _ = Left "Expecting number"

instance ValueLike String where
    toValue = VString
    tryFromValue (VString s) = Right s
    tryFromValue _ = Left "Expecting string"

instance ValueLike Array where
    toValue = VArray
    tryFromValue (VArray a) = Right a
    tryFromValue _ = Left "Expecting array" 

instance ValueLike Object where
    toValue = VObject
    tryFromValue (VObject o) = Right o
    tryFromValue _ = Left "Expecting object" 

instance ValueLike Bool where
    toValue = VBoolean . toBoolean
    tryFromValue (VBoolean b) = Right $ fromBoolean b
    tryFromValue _ = Left "Expecting boolean"

instance ValueLike Int where
    toValue i = VNumber $ toNumber i
    tryFromValue (VNumber n) = Right $ floor $ fromNumber n
    tryFromValue _ = Left "Expecting number"

instance ValueLike Integer where
    toValue i = VNumber $ toNumber i
    tryFromValue (VNumber n) = Right $ floor $ fromNumber n
    tryFromValue _ = Left "Expecting number"

instance ValueLike [Char] where
    toValue = VString . toString
    tryFromValue (VString x) = Right $ fromString x
    tryFromValue _ = Left "Expecting string"

instance ValueLike a => ValueLike (Maybe a) where
    toValue Nothing = VNull
    toValue (Just x) = toValue x
    tryFromValue VNull = Right Nothing
    tryFromValue x = tryFromValue x >>= return . Just

instance ValueLike a => ValueLike [a] where
    toValue x = VArray $ toArray $ map toValue x
    tryFromValue (VArray a) = mapM tryFromValue $ fromArray a
    tryFromValue _ = Left "Expecting array"

instance ValueLike a => ValueLike (M.Map [Char] a) where
    toValue x = VObject $ toObject $ map go $ M.toAscList x where
        go (k, v) = (toString k, toValue v)
    tryFromValue (VObject o) = do
        o' <- forM (fromObject o) $ \(k, v) -> do
            v' <- tryFromValue v
            return (fromString k, v')
        return $ M.fromList o'
    tryFromValue _ = Left "Expecting object"

instance ValueLike Value where
    toValue = id
    tryFromValue = Right
    
instance ValueLike J.JSValue where
    toValue = readJSON'
    tryFromValue = Right . J.showJSON
