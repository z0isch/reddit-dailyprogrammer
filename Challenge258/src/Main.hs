module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Network.IRC.Base      as IRC
import qualified Network.IRC.Parser    as IRCParser
import           Pipes
import           Pipes.Network.TCP

main :: IO ()
main = withSocketsDo $ do
  (sock,sockAddr) <- connectSock "chat.freenode.net" "6667"
  send sock (BS.pack "NICK z0isch\r\nUSER z0isch 0 * :zoisch\r\n")
  runEffect $ for (fromSocket sock 512) $ \msg -> do
    let msgStr = BS.unpack msg
    let parsed = IRCParser.decode msg
    lift $ putStr msgStr
    case parsed of
      (Just message) -> if ((IRC.msg_command message) == BS.pack "PING")
        then do
          lift $ send sock $ IRC.encode $ IRC.Message Nothing (BS.pack "PONG") (IRC.msg_params message)
          lift $ putStrLn "PONG sent"
        else return ()
      Nothing -> return ()
