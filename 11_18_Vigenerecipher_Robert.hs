import Data.Char

testCode :: String
testCode = "ALLY"

testMsg :: String
testMsg = "MEET AT DOWN"

-- | Encrypt a msg with the given code.
encrypt :: String -- ^ The code for encryption
  -> String -- ^ The message to encrypt
  -> String
encrypt = encryptDecrypt (+)


-- | Decrypt a msg with the given code.
decrypt :: String -- ^ The code to use
        -> String -- ^ The message to decrypt
        -> String
decrypt = encryptDecrypt (flip (-))

encryptDecrypt :: (Int -> Int -> Int) -> String -> String -> String
encryptDecrypt op code msg =
  let
    code' = cycle code
    msg' = filter isLetter msg
  in
    reconstruct msg $ encryptDecryptAlpha op code' msg'

-- | Encrypt a message only consisting of letters (no whitespace)
encryptDecryptAlpha :: (Int -> Int -> Int) -> String -> String -> String
encryptDecryptAlpha op code msg
  = map toChar
    $ zipWith op (map fromChar code) (map fromChar msg)



fromChar :: Char -> Int
fromChar = subtract (ord 'A') . ord . toUpper


toChar :: Int -> Char
toChar = chr . (+ ord 'A') . (`mod` 26)

-- Add back spaces and stuff to encrypted message.
reconstruct :: String -> String -> String
reconstruct [] es = es
reconstruct os [] = os
reconstruct (o:os) es'@(e:es) =
    if isLetter o
    then e' : reconstruct os es
    else o : reconstruct os es'
  where
    e' = if isLower o then toLower e else e


test :: Bool
test = encrypt testCode "meet at dawn" == "MPPR AE OYWY"


test1 :: Bool
test1 = (decrypt testCode . encrypt testCode) testMsg == testMsg
