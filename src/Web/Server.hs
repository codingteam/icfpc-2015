module Main where

import Data.Monoid
import Control.Applicative

import Options.Applicative

import Network.Shed.Httpd

import Core.GameModel


opts :: Parser (String, String)
opts = (,)
	<$> strOption (mconcat
		[ long "host"
		, short 'l'
		, metavar "ADDR"
		, help "hostname or IP address to bind to"
		])
	<*> strOption (mconcat
		[ long "port"
		, short 'p'
		, metavar "PORT"
		, help "port number to listen on"
		])

main :: IO ()
main = do
	(host, port) <- execParser $ info (helper <*> opts) mempty
	print (host, port)
