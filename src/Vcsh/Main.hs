-- |
-- Module      : Vcsh.Main
-- License     : GPL-2.0-or-later
--
-- The vcsh program: read the configuration, parse the command line, run
-- the command between the @pre-command@ and @post-command@ hooks and exit
-- with the exit code of the command.

module Vcsh.Main
    ( main
    )
where

import Control.Exception (handle)
import Control.Monad (unless, void, when)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.IO
    ( BufferMode(..), hPutStr, hPutStrLn, hSetBuffering, hSetEncoding, stderr
    , stdin, stdout, utf8)
import Vcsh.Cli
    ( Command(..), Options(..), commandName, commandRepo, parseCommand
    , parseOptions, usage)
import Vcsh.Config
    ( Config(..), GitIgnore(..), Vars, environmentVars, loadConfigFiles
    , mkConfig, sourceIfReadable)
import Vcsh.Context
    (Ctx(..), Repo(..), repoFromName, setVar, withRepo)
import Vcsh.Error (Failure, fatal, orFatal)
import Vcsh.FileSystem ((</>), createDirectories, isDirectory, pathExists)
import Vcsh.Hook (hook, warnOverlays)
import Vcsh.Log (Log(..), debug, failureMsg, verbose)
import Vcsh.Process (inEnv, readStdout, runForeground)

import qualified Data.Map.Strict as Map
import qualified Vcsh.Command.Files as Files
import qualified Vcsh.Command.Repo as Repo
import qualified Vcsh.Command.Run as Run

version :: String
version = "1.20141026"

-- | Run the command, return its exit code.
runCommand :: Ctx -> Command -> IO ExitCode
runCommand ctx = \case
    Clone remote branch _ -> Repo.clone ctx remote branch
    Commit args -> Run.commit ctx args
    Delete _ -> ok (Repo.delete ctx)
    Enter _ -> ok (Run.enter ctx)
    Foreach isGit cmdLine -> ok (Run.foreach ctx isGit cmdLine)
    Help -> return ExitSuccess
    Init _ -> ok (void $ Repo.initRepo ctx)
    List -> ok (Files.list ctx)
    ListTracked _ -> ok (Files.listTracked ctx)
    ListTrackedBy _ -> ok (Files.listTracked ctx)
    ListUntracked allFiles recursive _ ->
        ok (Files.listUntracked allFiles recursive ctx)
    Pull -> Run.pull ctx
    Push -> Run.push ctx
    Rename _ new ->
        ok (Repo.rename ctx (repoFromName (ctxConfig ctx) new))
    Run _ (cmd : args) -> Run.run ctx cmd args
    Run _ [] -> return ExitSuccess
    Status terse _ -> Run.status ctx terse
    Upgrade _ -> ok (void $ Repo.upgrade ctx)
    Version -> return ExitSuccess
    Which str -> ok (Files.which ctx str)
    WriteGitignore _ -> ok (Files.writeGitignore ctx)
    Shortcut _ [] -> ok (Run.enter ctx)
    Shortcut _ args -> Run.run ctx "git" args

    where

    ok action = action >> return ExitSuccess

-- | Variables exported to the processes for some commands, in addition to
-- those set by 'Ctx'.
commandVars :: Config -> Command -> [(String, String)]
commandVars cfg = \case
    Rename _ new ->
        let r = repoFromName cfg new
         in [("VCSH_REPO_NAME_NEW", repoName r), ("GIT_DIR_NEW", repoGitDir r)]
    Status True _ -> [("VCSH_STATUS_TERSE", "1")]
    Which str -> [("VCSH_COMMAND_PARAMETER", str)]
    _ -> []

-- | Create a directory if it does not exist. Throw an exception if the
-- path exists but is not a directory.
checkDir :: Log -> FilePath -> IO ()
checkDir lg dir = do
    isDir <- isDirectory dir
    unless isDir $ do
        exists <- pathExists dir
        when exists $
            fatal ("'" ++ dir ++ "' exists but is not a directory") 13
        verbose lg ("attempting to create '" ++ dir ++ "'")
        createDirectories dir
            `orFatal` ("could not create '" ++ dir ++ "'", 50)

-- | Print the help text on stderr.
printUsage :: String -> IO ()
printUsage self = hPutStr stderr (usage self)

-- | Print the vcsh and git versions.
printVersion :: String -> Vars -> IO ()
printVersion self vars = do
    putStrLn (self ++ " " ++ version)
    void $ runForeground (inEnv (Map.toList vars)) "git" ["version"]

-- | Replace @vcsh <repo> ...@ by @vcsh run <repo> git ...@ or
-- @vcsh enter <repo>@. Print the help text and exit with code 1 if the
-- repository does not exist.
resolveShortcut :: String -> Config -> Command -> IO Command
resolveShortcut self cfg = \case
    Shortcut name args -> do
        exists <- isDirectory (repoGitDir (repoFromName cfg name))
        unless exists $ printUsage self >> exitWith (ExitFailure 1)
        return $
            if null args then Enter name else Run name ("git" : args)
    cmd -> return cmd

-- | Run vcsh with the given options and arguments.
vcsh :: Log -> Options -> [String] -> IO ()
vcsh lg0 opts args = do
    env <- environmentVars
    let flagVars =
               [("VCSH_DEBUG", "1") | optDebug opts]
            ++ [("VCSH_VERBOSE", "1") | optVerbose opts]
    vars <- loadConfigFiles (optConfig opts) (Map.union (Map.fromList flagVars) env)
    cfg <- mkConfig vars
    let self = logSelf lg0
        lg = lg0 { logVerbose = cfgVerbose cfg, logDebug = cfgDebug cfg }
    when (cfgDebug cfg) $ do
        (_, gitVer) <- readStdout (inEnv (Map.toList vars)) "git" ["version"]
        debug lg (takeWhile (/= '\n') gitVer)
    cmd0 <- either (`fatal` 1) return (parseCommand args)
    case cmd0 of
        Help -> printUsage self >> exitSuccess
        Version -> printVersion self vars >> exitSuccess
        _ -> return ()
    cmd <- resolveShortcut self cfg cmd0
    let repo = repoFromName cfg <$> commandRepo cmd
        name = commandName cmd
    checkDir lg (cfgRepoD cfg)
    when (cfgGitIgnore cfg /= GitIgnoreNone) $
        checkDir lg (cfgBase cfg </> ".gitignore.d")
    when (cfgGitAttributes cfg) $
        checkDir lg (cfgBase cfg </> ".gitattributes.d")
    verbose lg (name ++ " begin")
    repoVars <-
        case repo of
            Nothing -> return vars
            Just r ->
                sourceIfReadable vars
                    (cfgConfigHome cfg </> "vcsh/config.d" </> repoName r)
    repoCfg <- mkConfig repoVars
    let ctx0 = Ctx
            { ctxConfig = repoCfg
            , ctxLog = lg
            , ctxVars = repoVars
            , ctxCommand = map (\c -> if c == '-' then '_' else c) name
            , ctxRepo = Nothing
            , ctxInUse = False
            }
        ctx1 = foldr (uncurry setVar) ctx0 (commandVars cfg cmd)
        ctx = maybe ctx1 (`withRepo` ctx1) repo
    warnOverlays ctx
    hook ctx "pre-command"
    code <- runCommand ctx cmd
    hook ctx "post-command"
    verbose lg (name ++ " end, exiting")
    exitWith code

-- | Print the message of a 'Failure' and exit with its exit code.
exitFailure :: Log -> Failure -> IO a
exitFailure lg failure = do
    let (msg, code) = failureMsg lg failure
    hPutStrLn stderr msg
    exitWith (if code == 0 then ExitSuccess else ExitFailure code)

main :: IO ()
main = do
    mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
    hSetBuffering stdout LineBuffering
    self <- getProgName
    (opts, args) <- parseOptions <$> getArgs
    when (optDebug opts) $ do
        putStrLn "debug mode on"
        putStrLn (self ++ " " ++ version)
    when (optVerbose opts) $ do
        putStrLn "verbose mode on"
        putStrLn (self ++ " " ++ version)
    let lg = Log self (optVerbose opts) (optDebug opts)
    handle (exitFailure lg) (vcsh lg opts args)
