module Main where

import Data.Monoid
import Control.Applicative

import Options.Applicative

import Network.Socket
import Network.Shed.Httpd

import Core.GameModel


opts :: Parser (String, Int)
opts = (,)
	<$> strOption (mconcat
		[ long "host"
		, short 'l'
		, value "localhost"
		, metavar "ADDR"
		, help "hostname or IP address to bind to"
		])
	<*> argument auto (mconcat
		[ metavar "PORT"
		, help "port number to listen on"
		])

main :: IO ()
main = do
	(_, port) <- execParser $ info (helper <*> opts) mempty
	initServerBind port iNADDR_ANY serve


serve :: Request -> IO Response
serve _ = return Response {resCode=200, resHeaders=[], resBody="hello http\n"}

