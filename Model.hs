module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Bson as B
import qualified Data.Attoparsec.Number as APN
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = (mkPersistSettings (ConT ''MongoBackend))
                        { mpsGeneric = False
                        }
 in share [mkPersist mongoSettings]
    $(persistFileWith lowerCaseSettings "config/models")

instance B.Val B.Value where
    val   = id
    cast' = Just 
 
toBson :: A.Value -> B.Value
toBson (A.String s)         = B.String s
toBson (A.Number (APN.I n)) = B.Int64 (fromIntegral n)
toBson (A.Number (APN.D n)) = B.Float n
toBson (A.Bool b)           = B.Bool b
toBson (A.Array a)          = B.Array $ map toBson (V.toList a)
toBson (A.Object o)         = B.Doc $ map (\(k, v) -> k =: toBson v) (M.toList o)
toBson (A.Null)             = B.Null
 
 
fromBson :: B.Value -> A.Value
fromBson (B.Float f)   = A.Number (APN.D f)
fromBson (B.String s)  = A.String s
fromBson (B.Doc d)     = A.object $ map fieldToPair d
fromBson (B.Array a)   = A.Array . V.fromList $ map fromBson a
fromBson (B.ObjId n)   = A.String . T.pack $ show n
fromBson (B.Bool b)    = A.Bool b
fromBson (B.UTC t)     = A.String . T.pack $ show t
fromBson (B.Int32 n)   = A.Number (APN.I $ fromIntegral n)
fromBson (B.Int64 n)   = A.Number (APN.I $ fromIntegral n)
fromBson (B.Uuid u)    = A.String . T.pack $ show u
fromBson (B.RegEx r)   = A.String . T.pack $ show r
fromBson (B.Null)      = A.Null
-- discard these BSON values
fromBson (B.Bin _)     = A.Null
fromBson (B.Fun _)     = A.Null
fromBson (B.Md5 _)     = A.Null
fromBson (B.UserDef _) = A.Null
fromBson (B.Stamp _)   = A.Null
fromBson (B.MinMax _)  = A.Null
fromBson (B.JavaScr _) = A.Null
fromBson (B.Sym _)     = A.Null
 
fieldToPair :: B.Field -> AT.Pair
fieldToPair f = (B.label f, fromBson (B.value f))
 
fromDocument :: B.Document -> A.Value
fromDocument d = A.object $ map fieldToPair d
