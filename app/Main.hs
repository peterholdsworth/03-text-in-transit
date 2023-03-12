module Main where

import Data.List
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Control.Concurrent
import System.Directory.Internal.Prelude (getArgs)

type Bold = Bool
type Underscore = Bool
type CharBUC = (Char, Bold, Underscore, Color)

par :: (String, Bold, Underscore, [Color], [CharBUC]) -> [CharBUC]
par ([],_,_,_ ,out) = reverse out
par (_ ,_,_,[],_  ) = error "too many [/C]s"
par (inp@(x:xs),b,u,col@(c:cs),out)
  | "[B]"         `isPrefixOf` inp = par (drop 3  inp, True,  u,     col,         out)
  | "[/B]"        `isPrefixOf` inp = par (drop 4  inp, False, u,     col,         out)
  | "[U]"         `isPrefixOf` inp = par (drop 3  inp, b,     True,  col,         out)
  | "[/U]"        `isPrefixOf` inp = par (drop 4  inp, b,     False, col,         out)
  | "[C:black]"   `isPrefixOf` inp = par (drop 9  inp, b,     u,     Black:col,   out)
  | "[C:red]"     `isPrefixOf` inp = par (drop 7  inp, b,     u,     Red:col,     out)
  | "[C:green]"   `isPrefixOf` inp = par (drop 9  inp, b,     u,     Green:col,   out)
  | "[C:yellow]"  `isPrefixOf` inp = par (drop 10 inp, b,     u,     Yellow:col,  out)
  | "[C:blue]"    `isPrefixOf` inp = par (drop 8  inp, b,     u,     Blue:col,    out)
  | "[C:magenta]" `isPrefixOf` inp = par (drop 11 inp, b,     u,     Magenta:col, out)
  | "[C:cyan]"    `isPrefixOf` inp = par (drop 8  inp, b,     u,     Cyan:col,    out)
  | "[C:white]"   `isPrefixOf` inp = par (drop 9  inp, b,     u,     White:col,   out)
  | "[/C]"        `isPrefixOf` inp = par (drop 4  inp, b,     u,     cs,          out)
  | otherwise = par (xs,b,u,col,(x,b,u,c):out)

parse :: String -> [CharBUC]
parse inp = par (inp, False, False, [Black], [])

writeChar :: CharBUC -> IO ()
writeChar (char, b, u, c) = do
  if u then setSGR [SetUnderlining SingleUnderline] 
       else setSGR [SetUnderlining NoUnderline]
  if b then setSGR [SetConsoleIntensity BoldIntensity] 
       else setSGR [SetConsoleIntensity NormalIntensity]
  setSGR [SetColor Foreground Vivid c]
  putChar char

scroll :: [CharBUC] -> Int -> Int -> IO ()
scroll str len cps = do
  mapM_ writeChar $ take len str
  hFlush stdout
  threadDelay (div 1000000 cps)
  setCursorColumn 0
  clearLine
  if null str then return () else scroll (tail str) len cps

main :: IO ()
main = do
  args <- getArgs
  let str = args!!0; len = read (args!!1)::Int; cps = read (args!!2)::Int;
  scroll (parse str) len cps
