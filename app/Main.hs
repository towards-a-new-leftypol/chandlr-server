module Main where

import Miso (ToServerRoutes)
import           Data.Proxy
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Servant.API

import qualified Common.FrontEnd.Routes as FE
import qualified Common.FrontEnd.Action as FE

newtype HtmlPage a = HtmlPage a
  -- deriving (Show, Eq)

type FrontEndRoutes = ToServerRoutes FE.Route HtmlPage FE.Action

app :: Wai.Application
app = undefined

port :: Int
port = 8888

main :: IO ()
main = do
    putStrLn $ "Serving front-end on port " ++ show port

    Wai.run port $ Wai.logStdout app
