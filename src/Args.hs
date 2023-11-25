module Args where
import Options.Applicative

data Args = Args Flags Command deriving (Eq,Show)

data Command
    = Doctor deriving (Eq, Show)

data Verbosity = Verbose | NotVerbose deriving (Eq,Show)

newtype Flags = Flags Verbosity deriving (Eq,Show)

argsParser :: Parser Args
argsParser = Args <$> flags <*> commands

verbosity :: Parser Verbosity
verbosity = flag NotVerbose Verbose
  ( long "verbose" <> short 'v' <> help "Enable verbose output")

flags :: Parser Flags
flags = Flags <$> verbosity

commands :: Parser Command
commands = hsubparser
  ( command "doctor" (info (pure Doctor) ( progDesc "check the CLI is working" ))
  )

withInfo :: Parser a -> String -> ParserInfo a
withInfo p s = info (helper <*> p) (fullDesc  <> progDesc s)

parseArgs :: [String] -> ParserResult Args
parseArgs = execParserPure preferences parserInfo where
   parserInfo = withInfo argsParser "CLI title"
   preferences = prefs (disambiguate <> showHelpOnEmpty <> showHelpOnError)