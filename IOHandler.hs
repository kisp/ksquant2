module IOHandler

where

import System.IO (hPutStrLn, hPutStr, stderr)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure), exitSuccess)
import Control.Monad (liftM, unless)

import System.Console.GetOpt (OptDescr(Option)
                             , ArgDescr(ReqArg, NoArg)
                             , usageInfo
                             , getOpt
                             , ArgOrder(RequireOrder))

import Types (Err)
import Options (PureMain, PureMultiMain, Options(..))
import Lisp (readLisp')

handleIO :: PureMain -> IO ()
handleIO = handleIO' . toMulti

handleIO' :: PureMultiMain -> IO ()
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
    let output = liftM head outputs

    outputHandler output

    where
      getInputHandler :: [String] -> ([String], IO String)
      getInputHandler [] = ([], getContents)
      getInputHandler ("-":r) = (r, getContents)
      getInputHandler (i:r) = (r, readFile i)

      getOutputHandler :: [String] -> Err String -> IO ()
      getOutputHandler [] (Right s) = putStrLn s
      getOutputHandler [o] (Right s) = writeFile o s
      getOutputHandler args (Right _) = error $ "getOutputHandler with args " ++ show args ++ "?"
      getOutputHandler _ (Left err) = do
        hPutStrLn stderr $ "ERROR: " ++ err
        exitWith $ ExitFailure 1

collectErrors :: [Err String] -> Err [String]
collectErrors [] = error "collectErrors on []"
collectErrors [Right x] = Right [x]
collectErrors [Left x] = Left x
collectErrors _ = error "collectErrors on list longer than 1"

toMulti :: PureMain -> PureMultiMain
toMulti m opts = collectErrors . map (m opts)


startOptions :: Options
startOptions = Options  { optVerbose        = False
                        , optInputFormat    = "sf"
                        , optOutputFormat   = "enp"
                        , optMaxDiv         = [8]
                        , optForbiddenDivs  = [[]]
                        , optTimeSignatures = readLisp' "((4 4))"
                        , optMetronomes     = readLisp' "((4 60))"
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "r" ["read", "from"]
        (ReqArg
            (\arg opt -> return opt { optInputFormat = arg })
            "FORMAT")
        "Input format"

    , Option "w" ["write", "to"]
        (ReqArg
            (\arg opt -> return opt { optOutputFormat = arg })
            "FORMAT")
        "Output format"

    , Option "m" ["max-div"]
        (ReqArg
            (\arg opt -> return opt { optMaxDiv = [read arg] })
            "Maximum division")
        "Output format"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
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
