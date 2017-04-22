{-# LANGUAGE OverloadedStrings #-}
import           Turtle hiding (need, (</>), (<.>), FilePath)
import qualified Turtle as Turtle
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Text.LaTeX hiding (empty)
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax
import           System.Directory (getCurrentDirectory)
import           Debug.Trace (traceShowM)

main :: IO ()
main = shakeArgs shakeOptions $ do
    want [ "tex" </> "functional-web-services.pdf" ]

    "graphs/*.png" %> \out -> do
      let inp = out -<.> "dot"
          dir = fromString (takeDirectory inp)
      need [inp]
      void $ withCwd dir $ graphviz (".." </> out) (T.pack (takeFileName inp))

    "//*.pdf" %> \out -> do
      let inp = out -<.> "tex"
          dir = fromString (takeDirectory inp)
      tdeps <- liftIO (texDeps inp)
      needsGraphics <- liftIO (graphicDeps inp)
      need ([inp] ++ map (takeDirectory inp </>) tdeps ++ needsGraphics)
      void $ withCwd dir $ latexmk (T.pack (takeFileName inp))

latexmk :: Text -> Action ExitCode
latexmk file = shell ("latexmk -shell-escape -pdf -g " <> file) empty

graphviz :: FilePath -> Text -> Action ExitCode
graphviz out inp = do
  let cmd = T.intercalate " " $ ["dot -s100 -Tpng","-o",T.pack out,inp]
  cwd <- liftIO getCurrentDirectory
  putNormal $ "Running command: '" <> T.unpack cmd <> "' in: " <> cwd
  shell cmd empty

withCwd :: MonadIO m => Turtle.FilePath -> m a -> m a
withCwd dir act = do
  cwd <- pwd
  cd dir
  r <- act
  cd cwd
  return r

cmdArgs :: TeXArg -> Maybe Text
cmdArgs (FixArg (TeXRaw arg)) = Just arg
cmdArgs _ = Nothing

commandDeps :: [String] -> FilePath -> IO [FilePath]
commandDeps cmds file = do
  etex <- parseLaTeXFile file
  case etex of
    Left err -> error ("Parsing of file " <> file <> " failed: " <> show err)
    Right t ->
      return . map T.unpack .
        mapMaybe cmdArgs .
        concatMap snd .
        matchCommand (`elem` cmds) $
        t

graphicDeps :: FilePath -> IO [FilePath]
graphicDeps file = do
  deps <- commandDeps ["includegraphics"] file
  return $ fmap dropDirectory1 deps

texDeps :: FilePath -> IO [FilePath]
texDeps file = do
  deps <- commandDeps ["include","input"] file
  return $ fmap (\dep -> if null (takeExtension dep)
                            then dep <.> "tex"
                            else dep) deps
