{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative
import           Control.Exception                   (bracket)
import           Control.Monad.IO.Class              (liftIO)
import qualified Control.Monad.Trans.Cont            as Cont
import qualified Control.Monad.Trans.Reader          as Reader
import           Data.List                           (intersperse, sort)
import           Data.Maybe                          (mapMaybe)
import           Data.Version                        (Version)
import qualified Distribution.InstalledPackageInfo   as DIPI
import qualified Distribution.Package                as DP
import qualified Distribution.PackageDescription     as DPD
import qualified Distribution.Simple.Configure       as DSC
import qualified Distribution.Simple.LocalBuildInfo  as DSLBI
import qualified Distribution.Simple.PackageIndex    as DSPI
import qualified Distribution.Simple.Program.Builtin as DSPB
import qualified Distribution.Simple.Program.Run     as DSPR
import qualified Distribution.Simple.Program.Types   as DSPT
import qualified Distribution.Verbosity              as DV
import qualified System.Directory                    as Dir
import           System.FilePath                     ((</>))
import qualified System.IO                           as IO

type ContIO r = Cont.ContT r (Reader.ReaderT DV.Verbosity IO)
type HaddockInfo = (FilePath, FilePath)

main :: IO ()
main = Reader.runReaderT (Cont.runContT main' return) DV.verbose
  where
    main' = do
      docdir    <- liftIO $ (</> "dist/doc/html") <$> Dir.getCurrentDirectory
      buildInfo <- liftIO $ DSC.getPersistBuildConfig "dist"
      deps      <- mapM (\d -> (d,) <$> haddockInfos d) $ dependencies buildInfo
      buildArgs (mkTitle buildInfo, deps) >>= runHaddock docdir

--
-- CONTINUATIONS
--

haddockInfos :: DIPI.InstalledPackageInfo -> ContIO r [HaddockInfo]
haddockInfos pkgInfo = Cont.ContT $ \k -> do
  let canonOf f = mapM Dir.canonicalizePath $ f pkgInfo
  liftIO (zip <$> canonOf DIPI.haddockInterfaces <*> canonOf DIPI.haddockHTMLs)
    >>= k

buildArgs :: (String, [(DIPI.InstalledPackageInfo, [HaddockInfo])])
          -> ContIO r [String]
buildArgs (title, deps) = Cont.ContT $ \k -> do
  verbosity <- Reader.ask
  liftIO $ bracket
    (Dir.getTemporaryDirectory >>= flip IO.openTempFile "gendocidx")
    (Dir.removeFile . fst)
    $ \(tmp, h) -> flip Reader.runReaderT verbosity $ do
       let pkgName = takePkgName . DP.pkgName . DIPI.sourcePackageId
           formatLine dep infos =
             "["++pkgName dep++"] " ++
             "<file://"++unwords (map snd infos)++"/index.html>"
           outData = unlines . intersperse "" . sort
                   $ map (uncurry formatLine) deps
       liftIO $ IO.hPutStr h outData >> IO.hFlush h
       k $ ["--title="++title, "--prologue="++tmp] ++
           [ "--read-interface="++dir++","++iface
           | (iface,dir) <- concatMap snd deps
           ]

runHaddock :: FilePath -> [String] -> ContIO () ()
runHaddock docdir args = Cont.ContT $ \_ -> do
  verbosity <- Reader.ask
  let getLoc = liftIO $ DSPT.programFindLocation DSPB.haddockProgram verbosity
      getVer = liftIO . DSPT.programFindVersion  DSPB.haddockProgram verbosity
      noHaddock = liftIO $ ioError $ userError "haddock not found"
  getLoc >>= maybe noHaddock (\l -> do
    ver <- getVer l
    liftIO . DSPR.runProgramInvocation verbosity $ invocation docdir l ver args)

--
-- PURE CODE
--

invocation :: String -> FilePath -> Maybe Version -> [String]
           -> DSPR.ProgramInvocation
invocation docdir loc versM = DSPR.programInvocation configuredProgram
  where
    pname = DSPT.programName DSPB.haddockProgram
    defaultArgs = ["--gen-contents", "--gen-index", "--odir="++docdir]
    configuredProgram =
      (DSPT.simpleConfiguredProgram pname (DSPT.FoundOnSystem loc))
        { DSPT.programVersion = versM
        , DSPT.programDefaultArgs = defaultArgs
        }

dependencies :: DSLBI.LocalBuildInfo -> [DIPI.InstalledPackageInfo]
dependencies buildInfo =
  mapMaybe (DSPI.lookupInstalledPackageId pkgIdx . fst) depends
  where
    pkgIdx = DSLBI.installedPkgs buildInfo
    depends = DSLBI.externalPackageDeps buildInfo

takePkgName :: DP.PackageName -> String
takePkgName (DP.PackageName name) = name

mkTitle :: DSLBI.LocalBuildInfo -> String
mkTitle buildInfo =
  (prefix++) . takePkgName . DP.pkgName $ DPD.package pkgDescr
  where
    prefix = "Dependencies for "
    pkgDescr = DSLBI.localPkgDescr buildInfo
