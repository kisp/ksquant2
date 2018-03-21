module IOHandler (handleIO)

where

import System.IO (hPutStrLn, hPutStr, stderr)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure), exitSuccess)
import Control.Monad (unless)

import System.Console.GetOpt (OptDescr(Option)
                             , ArgDescr(ReqArg, NoArg)
                             , usageInfo
                             , getOpt
                             , ArgOrder(RequireOrder))

import qualified Types as T (Err)
import qualified Options as O (PureMain, PureMultiMain, Options(..))
import qualified Lisp as L (readLisp')

handleIO :: O.PureMain -> IO ()
handleIO = handleIO' . toMulti

handleIO' :: O.PureMultiMain -> IO ()
handleIO' m = do
    args <- getArgs

    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    unless (null errors && length nonOptions <= 2) (handleInvalidOptions errors)

    opts <- foldl (>>=) (return startOptions) actions

    let (nonOptions', inputHandler) = getInputHandler nonOptions
    let outputHandler = getOutputHandler nonOptions'

    input <- inputHandler
    let inputs = [input]

    let outputs = m opts inputs
    let output = fmap head outputs

    outputHandler output

    where
      getInputHandler :: [String] -> ([String], IO String)
      getInputHandler [] = ([], getContents)
      getInputHandler ("-":r) = (r, getContents)
      getInputHandler (i:r) = (r, readFile i)

      getOutputHandler :: [String] -> T.Err String -> IO ()
      getOutputHandler [] (Right s) = putStr s
      getOutputHandler [o] (Right s) = writeFile o s
      getOutputHandler args (Right _) = error $ "getOutputHandler with args " ++ show args ++ "?"
      getOutputHandler _ (Left err) = do
        hPutStrLn stderr $ "ERROR: " ++ err
        exitWith $ ExitFailure 1

collectErrors :: [T.Err String] -> T.Err [String]
collectErrors [] = error "collectErrors on []"
collectErrors [Right x] = Right [x]
collectErrors [Left x] = Left x
collectErrors _ = error "collectErrors on list longer than 1"

toMulti :: O.PureMain -> O.PureMultiMain
toMulti m opts = collectErrors . map (m opts)


startOptions :: O.Options
startOptions = O.Options  { O.optVerbose        = False
                        , O.optInputFormat    = "sf"
                        , O.optOutputFormat   = "enp"
                        , O.optMaxDiv         = [8]
                        , O.optForbiddenDivs  = [[]]
                        , O.optTimeSignatures = L.readLisp' "((4 4))"
                        , O.optMetronomes     = L.readLisp' "((4 60))"
                        , O.optMeasureSiblingMerge = False
                        }

options :: [ OptDescr (O.Options -> IO O.Options) ]
options =
    [ Option "r" ["read", "from"]
        (ReqArg
            (\arg opt -> return opt { O.optInputFormat = arg })
            "FORMAT")
        "Input format"

    , Option "w" ["write", "to"]
        (ReqArg
            (\arg opt -> return opt { O.optOutputFormat = arg })
            "FORMAT")
        "Output format"

    , Option "m" ["max-div"]
        (ReqArg
            (\arg opt -> return opt { O.optMaxDiv = [read arg] })
            "Maximum division")
        "Output format"

    , Option "" ["measure-sibling-merge"]
        (NoArg
            (\opt -> return opt { O.optMeasureSiblingMerge = True }))
        "Enable measureSiblingMerge"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { O.optVerbose = True }))
        "Enable verbose messages"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "KSQuant2 0.2.1"
                exitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr (usageInfo usageHeader options)
                exitSuccess))
        "Show help"
    ]

usageHeader :: String
usageHeader = "Usage: ksquant2 [OPTIONS]"

handleInvalidOptions :: [String] -> IO ()
handleInvalidOptions errors = do
  hPutStrLn stderr $ concat errors
  hPutStr stderr $ usageInfo usageHeader options
  exitWith $ ExitFailure 1
