module Cryptog where

import Crypto.Random (SystemDRG, DRG (randomBytesGenerate))
import qualified Data.Text as T
import qualified Data.ByteString as B
import Crypto.Hash (SHA256(SHA256), hashWith)
import Crypto.KDF.BCrypt (bcrypt)
import Crypto.Number.Generate (generateBetween)
import Data.Text.Encoding ( encodeUtf8 )

createToken :: Monad m => SystemDRG -> B.ByteString  -> m String
createToken drg salt = do
    let (bytes, gen) = randomBytes drg 16 :: (B.ByteString, SystemDRG)
        digest = bcrypt 8 bytes (salt :: B.ByteString) :: B.ByteString
    pure . show . hashWith SHA256 $ digest
    where
        randomBytes = flip randomBytesGenerate

hashEmail email = show $ hashWith SHA256 email

createPollId :: IO Integer
createPollId = generateBetween 1 100000000
