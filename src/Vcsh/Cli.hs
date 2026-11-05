-- |
-- Module      : Vcsh.Cli
-- License     : GPL-2.0-or-later
--
-- Command line parsing.

module Vcsh.Cli
    (
    -- * Options
      Options (..)
    , parseOptions

    -- * Commands
    , Command (..)
    , parseCommand
    , commandName
    , commandRepo

    -- * Help
    , usage
    )
where

import Data.Maybe (fromMaybe, listToMaybe)
import Vcsh.FileSystem (baseName, dropSuffix)

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

-- | Options given before the command.
data Options = Options
    { optConfig :: Maybe FilePath -- ^ @-c <file>@
    , optDebug :: Bool            -- ^ @-d@
    , optVerbose :: Bool          -- ^ @-v@
    }

-- | Parse the options before the command, return the options and the
-- remaining arguments.
parseOptions :: [String] -> (Options, [String])
parseOptions = go (Options Nothing False False)

    where

    go opts ("-c" : file : rest) = go opts { optConfig = Just file } rest
    go opts ("-d" : rest) = go opts { optDebug = True } rest
    go opts ("--debug" : rest) = go opts { optDebug = True } rest
    go opts ("-v" : rest) = go opts { optVerbose = True } rest
    go opts rest = (opts, rest)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- | A command with its arguments.
data Command =
      Clone String String String   -- ^ remote, branch, repository
    | Commit [String]              -- ^ arguments for @git commit@
    | Delete String
    | Enter String
    | Foreach Bool [String]        -- ^ git command?, command line
    | Help
    | Init String
    | List
    | ListTracked (Maybe String)
    | ListTrackedBy String
    | ListUntracked Bool Bool (Maybe String) -- ^ all, recursive, repository
    | Pull
    | Push
    | Rename String String         -- ^ repository, new name
    | Run String [String]          -- ^ repository, command line
    | Status Bool (Maybe String)   -- ^ terse, repository
    | Upgrade String
    | Version
    | Which String
    | WriteGitignore String
    | Shortcut String [String]
      -- ^ @vcsh <repo> <git command>@ or @vcsh <repo>@, valid only if the
      -- repository exists.

-- | The name of a command, used for @VCSH_COMMAND@ and messages.
commandName :: Command -> String
commandName = \case
    Clone {} -> "clone"
    Commit {} -> "commit"
    Delete {} -> "delete"
    Enter {} -> "enter"
    Foreach {} -> "foreach"
    Help -> "help"
    Init {} -> "init"
    List -> "list"
    ListTracked {} -> "list-tracked"
    ListTrackedBy {} -> "list-tracked-by"
    ListUntracked {} -> "list-untracked"
    Pull -> "pull"
    Push -> "push"
    Rename {} -> "rename"
    Run {} -> "run"
    Status {} -> "status"
    Upgrade {} -> "upgrade"
    Version -> "version"
    Which {} -> "which"
    WriteGitignore {} -> "write-gitignore"
    Shortcut _ [] -> "enter"
    Shortcut _ _ -> "run"

-- | The repository a command works on, as given on the command line.
commandRepo :: Command -> Maybe String
commandRepo = \case
    Clone _ _ repo -> Just repo
    Delete repo -> Just repo
    Enter repo -> Just repo
    Init repo -> Just repo
    ListTracked repo -> repo
    ListTrackedBy repo -> Just repo
    ListUntracked _ _ repo -> repo
    Rename repo _ -> Just repo
    Run repo _ -> Just repo
    Status _ repo -> repo
    Upgrade repo -> Just repo
    WriteGitignore repo -> Just repo
    Shortcut repo _ -> Just repo
    _ -> Nothing

-- | Abbreviations of the command names.
abbreviations :: [(String, String)]
abbreviations =
    [ (abbr, name)
    | (name, abbrs) <-
        [ ("clone", ["clon", "clo", "cl"])
        , ("commit", ["commi", "comm", "com", "co"])
        , ("delete", ["delet", "dele", "del", "de"])
        , ("enter", ["ente", "ent", "en"])
        , ("help", ["hel", "he"])
        , ("init", ["ini", "in"])
        , ("pull", ["pul"])
        , ("push", ["pus"])
        , ("rename", ["renam", "rena", "ren", "re"])
        , ("run", ["ru"])
        , ("status", ["statu", "stat", "sta", "st"])
        , ("upgrade", ["upgrad", "upgra", "upgr", "up"])
        , ("version", ["versio", "versi", "vers", "ver", "ve"])
        , ("which", ["whi", "wh"])
        , ("write-gitignore", ["write", "writ", "wri", "wr"])
        ]
    , abbr <- abbrs
    ]

-- | The first argument, if it is not empty.
nonEmptyArg :: [String] -> Maybe (String, [String])
nonEmptyArg (x : rest) | not (null x) = Just (x, rest)
nonEmptyArg _ = Nothing

-- | Parse @clone [-b <branch>] <remote> [<repo>]@, where @-b <branch>@ may
-- also follow @<remote>@ or @<repo>@. The default branch is @master@ and
-- the default repository name is the last component of the remote without
-- the @.git@ suffix.
parseClone :: [String] -> Either String Command
parseClone args0 =
    case nonEmptyArg args1 of
        Nothing -> Left "please specify a remote"
        Just (remote, args2) ->
            let (branch2, args3) = branchOpt branch1 args2
             in case nonEmptyArg args3 of
                    Just (repo, args4) ->
                        let (branch3, _) = branchOpt branch2 args4
                         in Right (Clone remote (orMaster branch3) repo)
                    Nothing ->
                        case remoteName remote of
                            "" -> Left "could not determine repository name"
                            repo -> Right (Clone remote (orMaster branch2) repo)

    where

    (branch1, args1) = branchOpt Nothing args0

    branchOpt Nothing ("-b" : rest) = (listToMaybe rest, drop 1 rest)
    branchOpt branch rest = (branch, rest)

    orMaster = fromMaybe "master"

    -- basename "${remote#*:}" .git
    remoteName remote =
        let path =
                case break (== ':') remote of
                    (_, ':' : rest) -> rest
                    _ -> remote
         in dropSuffix ".git" (baseName path)

-- | Parse the arguments of a command that requires a repository.
withRepoArg ::
    [String] -> (String -> [String] -> Either String Command)
    -> Either String Command
withRepoArg args f =
    case nonEmptyArg args of
        Just (repo, rest) -> f repo rest
        Nothing -> Left "please specify repository to work on"

-- | Parse the command and its arguments. On a parse error, return the error
-- message prefixed with the command name.
parseCommand :: [String] -> Either String Command
parseCommand [] = Right Help
parseCommand (cmd0 : args) = either (Left . prefix) Right $
    case cmd of
        "clone" -> parseClone args
        "commit" -> Right (Commit args)
        "delete" -> withRepoArg args $ \r _ -> Right (Delete r)
        "enter" -> withRepoArg args $ \r _ -> Right (Enter r)
        "foreach" ->
            let (isGit, cmdLine) = gitFlag args
             in case nonEmptyArg cmdLine of
                    Nothing -> Left "please specify a command"
                    Just _ -> Right (Foreach isGit cmdLine)
        "help" -> Right Help
        "init" -> withRepoArg args $ \r _ -> Right (Init r)
        "list" -> Right List
        "list-tracked" -> Right (ListTracked (fst <$> nonEmptyArg args))
        "list-tracked-by" ->
            withRepoArg args $ \r _ -> Right (ListTrackedBy r)
        "list-untracked" ->
            let (flags, rest) = span isUntrackedFlag args
                has c = any (c `elem`) flags
             in Right $
                    ListUntracked
                        (has 'a') (has 'r') (fst <$> nonEmptyArg rest)
        "pull" -> Right Pull
        "push" -> Right Push
        "rename" -> withRepoArg args $ \r rest ->
            case nonEmptyArg rest of
                Just (new, _) -> Right (Rename r new)
                Nothing -> Left "please specify a target name"
        "run" -> withRepoArg args $ \r rest ->
            case nonEmptyArg rest of
                Just _ -> Right (Run r rest)
                Nothing -> Left "please specify a command"
        "status" ->
            case args of
                ("--terse" : rest) ->
                    Right (Status True (fst <$> nonEmptyArg rest))
                _ -> Right (Status False (fst <$> nonEmptyArg args))
        "upgrade" -> withRepoArg args $ \r _ -> Right (Upgrade r)
        "version" -> Right Version
        "which" ->
            case args of
                [] -> Left "please specify a filename"
                ("" : _) -> Left "please specify a filename"
                [str] -> Right (Which str)
                _ -> Left "too many parameters"
        "write-gitignore" ->
            withRepoArg args $ \r _ -> Right (WriteGitignore r)
        "" -> Right Help
        repo ->
            case nonEmptyArg args of
                Just _ -> Right (Shortcut repo args)
                Nothing -> Right (Shortcut repo [])

    where

    cmd = fromMaybe cmd0 (lookup cmd0 abbreviations)
    prefix msg = cmd ++ ": " ++ msg

    gitFlag ("-g" : rest) = (False, snd (gitFlag rest))
    gitFlag rest = (True, rest)

    isUntrackedFlag ('-' : flags@(_ : _)) = all (`elem` "ar") flags
    isUntrackedFlag _ = False

-------------------------------------------------------------------------------
-- Help
-------------------------------------------------------------------------------

-- | The help text, for the given program name.
usage :: String -> String
usage self = unlines
    [ "usage: " ++ self ++ " <options> <command>"
    , ""
    , "   options:"
    , "   -c <file>            Source file"
    , "   -d                   Enable debug mode"
    , "   -v                   Enable verbose mode"
    , ""
    , "   commands:"
    , "   clone [-b <branch>] \\"
    , "         <remote> \\"
    , "         [<repo>]       Clone from an existing repository"
    , "   commit               Commit in all repositories"
    , "   delete <repo>        Delete an existing repository"
    , "   enter <repo>         Enter repository; spawn new instance of $SHELL"
    , "   foreach [<-g>]"
    , "     <git command>      Execute a command for every repository"
    , "   help                 Display this help text"
    , "   init <repo>          Initialize a new repository"
    , "   list                 List all repositories"
    , "   list-tracked \\"
    , "        [<repo>]        List all files tracked all or one repositories"
    , "   list-untracked \\"
    , "        [<-a>] [<-r>]"
    , "        [<repo>]        List all files not tracked by all or one \
      \repositories"
    , "   pull                 Pull from all vcsh remotes"
    , "   push                 Push to vcsh remotes"
    , "   rename <repo> \\"
    , "          <newname>     Rename repository"
    , "   run <repo> \\"
    , "       <command>        Use this repository"
    , "   status \\"
    , "     [--terse] [<repo>] Show statuses of all/one vcsh repositories"
    , "   upgrade <repo>       Upgrade repository to currently recommended \
      \settings"
    , "   version              Print version information"
    , "   which <substring>    Find substring in name of any tracked file"
    , "   write-gitignore \\"
    , "   <repo>               Write .gitignore.d/<repo> via git ls-files"
    , ""
    , "   <repo> <git command> Shortcut to run git commands directly"
    , "   <repo>               Shortcut to enter repository"
    ]
