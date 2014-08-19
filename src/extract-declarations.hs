{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Version (showVersion,Version(Version),versionBranch)

import Distribution.Hackage.DB (readHackage')

import Distribution.PackageDescription (
    GenericPackageDescription,PackageDescription,FlagAssignment,
    library,libBuildInfo,
    targetBuildDepends,buildDepends)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import qualified Distribution.Package as Cabal (Dependency(Dependency))
import Distribution.Version (withinRange)

import Data.Aeson (ToJSON(toJSON),object,(.=),encode)
import Distribution.Text (display)
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import System.Directory (
    doesFileExist,doesDirectoryExist)
import System.Process (rawSystem)

import Control.Monad (when,void,guard)
import Control.Applicative (Applicative)
import Data.Map (Map)
import qualified Data.Map as Map (
    map,filterWithKey,traverseWithKey,toList,lookup)
import Data.List (nub)
import Data.Function (on)


main :: IO ()
main = do
    allpackages <- availablePackagesOnHackage
    let packages = pruneIndex fewPackages allpackages
    saveDependencies (resolveDependencyRanges allpackages (Map.map (Map.map packageDependencyRanges) packages))
    extractDeclarations packages

extractDeclarations :: Index a -> IO ()
extractDeclarations packages = do
    forPackages packages (\packagename packageversion _ -> do
        let packagequalifier = packagename ++ "-" ++ showVersion packageversion
        exists <- doesDirectoryExist ("/home/pschuster/Projects/symbols/packages/lib/x86_64-linux-haskell-declarations-0.1/" ++ packagequalifier)
        if exists
            then do
                putStrLn (packagequalifier ++ " already there!")
            else do
                rawSystem "cabal" [
                    "install","--force-reinstalls",
                    "--package-db=/home/pschuster/.haskell-packages/haskell-declarations.db",
                    "--package-db=/home/pschuster/Projects/symbols/packages.db",
                    "--prefix=/home/pschuster/Projects/symbols/packages",
                    "--gcc-option=-I/usr/lib/ghc/include",
                    "--haskell-suite","-w","haskell-declarations",
                    packagequalifier]
                return ())
    return ()

forPackages :: (Applicative m) => Index a -> (PackageName -> PackageVersion -> a -> m b) -> m (Index b)
forPackages packages action = do
    flip Map.traverseWithKey packages (\packagename packageversions -> do
        flip Map.traverseWithKey packageversions (\packageversion packageinformation -> do
            action packagename packageversion packageinformation))

data Package = Package PackageName PackageVersion [Dependency] (Maybe NextVersion)
type PackageName   = String
type PackageVersion = Version
data NextVersion = NextVersion Change PackageVersion
data Change = Minor | Major
type DependencyName = PackageName
type DependencyVersion = PackageVersion
data Dependency = Dependency DependencyName DependencyVersion
type Index a = Map PackageName (Map PackageVersion a)
type DependencyRange = Cabal.Dependency

availablePackagesOnHackage :: IO (Index GenericPackageDescription)
availablePackagesOnHackage = do
    putStrLn "Downloading Index ..."
    exists <- doesFileExist "index.tar"
    when (not exists) (void (do
        void (rawSystem "wget" [
            "-nv",
            "-O","index.tar.gz",
            "hackage.haskell.org/packages/index.tar.gz"])
        rawSystem "gunzip" ["-f","index.tar.gz"]))
    readHackage' "index.tar"

packageDependencyRanges :: GenericPackageDescription -> [DependencyRange]
packageDependencyRanges genericpackagedescription = do
    case simpleConfigure genericpackagedescription of
        Left _ -> []
        Right (packagedescription,_) -> do
            case library packagedescription of
                Nothing -> []
                Just librarysection -> nub (
                    targetBuildDepends (libBuildInfo librarysection) ++
                    buildDepends packagedescription)

resolveDependencyRanges :: Index a -> Index [DependencyRange] -> Index [Dependency]
resolveDependencyRanges allpackages packages = Map.map (Map.map (concatMap (allDependenciesInRange allpackages))) packages

allDependenciesInRange :: Index a -> DependencyRange -> [Dependency]
allDependenciesInRange packages (Cabal.Dependency dependencyname versionrange) = do
    let packagename = display dependencyname
    versions <- maybe [] (:[]) (Map.lookup packagename packages)
    (packageversion,_) <- Map.toList versions
    guard (withinRange packageversion versionrange)
    return (Dependency packagename packageversion)

defaultPlatform :: Platform
defaultPlatform = Platform I386 Linux

defaultCompiler :: CompilerId
defaultCompiler = CompilerId GHC (Version [7,6,3] [])

simpleConfigure :: GenericPackageDescription -> Either [DependencyRange] (PackageDescription,FlagAssignment)
simpleConfigure = finalizePackageDescription [] (const True) defaultPlatform defaultCompiler []

saveDependencies :: Index [Dependency] -> IO ()
saveDependencies = ByteString.writeFile "packageinfo" . encode . flattenIndex

flattenIndex :: Index [Dependency] -> [Package]
flattenIndex packages = do
    (packagename,versions) <- Map.toList packages
    let versionlist = Map.toList versions
        nextversionlist = map (Just . fst) (drop 1 versionlist) ++ [Nothing]
    ((packageversion,dependencies),maybenextversion) <- zip versionlist nextversionlist
    let nextversion = fmap (\x -> NextVersion (change packageversion x) x) maybenextversion
        change v1 v2 = if ((==) `on` take 2 . versionBranch) v1 v2
            then Minor
            else Major
    return (Package packagename packageversion dependencies nextversion)

instance ToJSON Package where
    toJSON (Package packagename packageversion dependencies nextversion) = object [
        "packagename" .= packagename,
        "packageversion" .= display packageversion,
        "dependencies" .= dependencies,
        "nextversion" .= nextversion]

instance ToJSON Dependency where
    toJSON (Dependency dependencyname dependencyversion) = object [
        "dependencyname" .= dependencyname,
        "dependencyversion" .= display dependencyversion]

instance ToJSON NextVersion where
    toJSON (NextVersion change packageversion) = object [
        "change" .= change,
        "nextversion" .= display packageversion]

instance ToJSON Change where
    toJSON Minor = "minor"
    toJSON Major = "major"

pruneIndex :: [PackageName] -> Index a -> Index a
pruneIndex packagenames = Map.filterWithKey (\key _ -> key `elem` packagenames)

fewPackages :: [PackageName]
fewPackages = ["network"]

packagesThatMightComeWithGHC :: [PackageName]
packagesThatMightComeWithGHC = [
    "bytestring","containers","deepseq","directory","filepath",
    "haskell2010","haskell98","hpc","old-locale","pretty","process",
    "template-haskell","time","unix"]

disregardedPackages :: [PackageName]
disregardedPackages = ["Cabal","Win32","syb","old-time"]

manyReverseDependenciesPackages :: [PackageName]
manyReverseDependenciesPackages = [
    "containers","bytestring","mtl","directory","text","transformers",
    "filepath","time","QuickCheck","process","network","parsec",
    "random","HUnit","template-haskell","vector","binary","test-framework",
    "deepseq","utf8-string","unix","old-locale","aeson","attoparsec","pretty",
    "stm","test-framework-hunit","test-framework-quickcheck2",
    "unordered-containers","hspec","old-time","data-default",
    "haskell98","split","syb","conduit","http-types"]

packagesThatMightBeInThePlatform :: [PackageName]
packagesThatMightBeInThePlatform = packagesThatMightComeWithGHC ++ [
    "async","attoparsec","case-insensitive","cgi","fgl","GLUT","GLURaw",
    "hashable","haskell-src","html","HTTP","HUnit","mtl","network","OpenGL",
    "OpenGLRaw","parallel","parsec","QuickCheck","random","regex-base",
    "regex-compat","regex-posix","split","stm","syb","text","transformers",
    "unordered-containers","vector","xhtml","zlib","cabal-install","alex","happy","haddock"]

packagesOnStackage :: [PackageName]
packagesOnStackage = [
    "accelerate","active","ad","adjunctions","async","aws","base-compat","base-unicode-symbols",
    "base16-bytestring","basic-prelude","bifunctors","bindings-DSL","blaze-html","blaze-markup",
    "BlogLiterately","BlogLiterately-diagrams","bound","bytedump","byteorder","bzlib-conduit",
    "cairo","case-insensitive","cassava","categories","certificate","charset","cipher-aes",
    "cipher-rc4","classy-prelude-yesod","comonad","comonad-extras","comonad-transformers",
    "comonads-fd","compdata","composition","compressed","concurrent-supply","conduit-combinators",
    "configurator","connection","constraints","containers-unicode-symbols","contravariant",
    "convertible","cprng-aes","cpu","crypto-pubkey-types","crypto-random-api","cryptocipher",
    "cryptohash","csv-conduit","derive","diagrams","diagrams-builder","diagrams-cairo",
    "diagrams-contrib","diagrams-core","diagrams-haddock","diagrams-lib","diagrams-postscript",
    "diagrams-svg","dimensional","distributive","doctest","dual-tree","either","eq","esqueleto",
    "fay","fay-base","fay-dom","fay-jquery","fay-text","fay-uri","fb","fb-persistent","fclabels",
    "FenwickTree","filesystem-conduit","fixed-list","force-layout","fpco-api","free","ghc-mtl",
    "github","gitlib","gitlib-cmdline","gitlib-libgit2","gitlib-s3","gitlib-test","graphs",
    "gravatar","groundhog","groundhog-mysql","groundhog-postgresql","groundhog-sqlite",
    "groundhog-th","groupoids","hackage-proxy","HandsomeSoup","haskell-names","haskell-packages",
    "HaTeX","haxr","hdaemonize","heaps","hebrew-time","hint","hit","hjsmin","hlibgit2","hlint",
    "hoogle","hPDB","hscurses","hse-cpp","hspec","hsyslog","HTF","hweblib","hxt","hxt-relaxng",
    "hyphenation","indents","integration","intervals","io-memoize","iterable","judy","kan-extensions",
    "keter","kure","language-c","language-ecmascript","language-java","language-javascript","lca",
    "lens","libgit","linear","machines","markdown","MFlow","mime-mail-ses","monad-coroutine",
    "monad-extras","monad-parallel","monad-products","monad-st","MonadCatchIO-mtl",
    "MonadCatchIO-transformers","monadic-arrays","MonadRandom","monoid-extras","mtl","network",
    "network-conduit-tls","numbers","numeric-extras","Octree","parseargs","parsers","pem",
    "persistent","persistent-mongoDB","persistent-sqlite","persistent-template","pipes",
    "pipes-concurrency","pipes-parse","pointed","prelude-extras","pretty-class","pretty-show",
    "process-conduit","profunctor-extras","profunctors","random-shuffle","recursion-schemes",
    "reducers","reflection","RefSerialize","regex-applicative","rev-state","runmemo","safe-failure",
    "semigroupoid-extras","semigroupoids","semigroups","shake","shakespeare-text","shelly","siphash",
    "smallcheck","smtLib","snaplet-fay","socks","speculation","sqlite-simple","statistics","stm-conduit",
    "stm-stats","streams","stylish-haskell","syb-extras","tagged","tardis","tasty","tasty-golden",
    "tasty-hunit","tasty-quickcheck","tasty-smallcheck","TCache","text","th-expand-syns","these",
    "threepenny-gui","thyme","time-lens","tls","tls-debug","traverse-with-class","udbus","unification-fd",
    "uuid","vector-instances","vector-space-points","vhd","void","wai-websockets","warp-tls","web-fpco",
    "wl-pprint-extras","wl-pprint-terminfo","Workflow","xenstore","xmlgen","yackage","yesod","yesod-auth-fb",
    "yesod-auth-oauth","yesod-bin","yesod-eventsource","yesod-fay","yesod-fb","yesod-newsfeed","yesod-sitemap",
    "yesod-static","yesod-test","yesod-websockets","abstract-deque","abstract-par","AC-Vector","aeson",
    "aeson-pretty","ansi-terminal","ansi-wl-pprint","arithmoi","array","asn1-data","asn1-encoding",
    "asn1-parse","asn1-types","atomic-primops","attempt","attoparsec","attoparsec-conduit",
    "attoparsec-enumerator","authenticate","authenticate-oauth","base","base64-bytestring",
    "base64-conduit","binary","bits-atomic","blaze-builder","blaze-builder-conduit",
    "blaze-builder-enumerator","blaze-svg","blaze-textual","bool-extras","Boolean","bson","byteable",
    "bytes","bytestring","bytestring-mmap","Cabal","cautious-file","cereal","cereal-conduit","cgi",
    "chaselev-deque","chunked-data","cipher-blowfish","cipher-camellia","cipher-des","circle-packing",
    "classy-prelude","classy-prelude-conduit","clientsession","cmdargs","colour","concatenative",
    "conduit","containers","control-monad-loop","cookie","cpphs","crypto-api","crypto-cipher-types",
    "crypto-conduit","crypto-numbers","crypto-pubkey","crypto-random","cryptohash-cryptoapi",
    "css-text","data-binary-ieee754","data-default","data-default-class","data-default-instances-base",
    "data-default-instances-containers","data-default-instances-dlist",
    "data-default-instances-old-locale","data-lens","data-lens-template","data-reify","deepseq",
    "diagrams-gtk","Diff","digest","direct-sqlite","directory","directory-tree","dlist","double-conversion",
    "email-validate","enclosed-exceptions","entropy","enumerator","erf","errorcall-eq-instance",
    "errors","exceptions","extensible-exceptions","failure","fast-logger","file-embed","filepath",
    "fingertree","fsnotify","generic-deriving","ghc","ghc-paths","ghc-prim","gio","glib","groom",
    "groups","gtk","hamlet","happstack-server","hashable","hashable-extras","hashtables","haskell-lexer",
    "haskell-src-exts","HaXml","heist","hexpat","hfsevents","highlighting-kate","hinotify","hostname",
    "hs-bibutils","hscolour","hslogger","hslua","HsOpenSSL","hspec-expectations","html","html-conduit",
    "HTTP","http-client","http-client-conduit","http-client-tls","http-conduit","http-date",
    "http-reverse-proxy","http-types","HUnit","hxt-charproperties","hxt-http","hxt-regex-xmlschema",
    "hxt-unicode","idna","ini","integer","integer-gmp","integer-simple","io-streams","json",
    "JuicyPixels","keys","language-haskell-extract","libxml","lifted-async","lifted-base","List",
    "list-extras","lists","logict","math-functions","matrix","MaybeT","MemoTrie","mime-mail",
    "mime-types","missing-foreign","mmap","mmorph","monad-control","monad-logger","monad-loops",
    "monad-par","monad-par-extras","monadcryptorandom","monadloc","monads-tf","mongoDB","mono-traversable",
    "mwc-random","mysql","mysql-simple","nats","network-bytestring","network-conduit","network-info",
    "newtype","NumInstances","numtype","old-locale","old-time","optparse-applicative","pandoc",
    "pandoc-citeproc","pandoc-types","pango","par-classes","parallel","parsec","path-pieces","patience",
    "pcre-light","polyparse","pool-conduit","postgresql-libpq","postgresql-simple","pretty","primitive",
    "process","process-extras","project-template","publicsuffixlist","punycode","pureMD5","pwstore-fast",
    "QuickCheck","quickcheck-io","random","ReadArgs","regex-base","regex-compat","regex-pcre-builtin",
    "regex-posix","regex-tdfa","resource-pool","resourcet","retry","rfc5051","RSA","rts","safe",
    "scientific","securemem","sendfile","setenv","SHA","shakespeare","shakespeare-css","shakespeare-i18n",
    "shakespeare-js","silently","simple-sendfile","skein","snap","snap-core","snap-server","sourcemap",
    "spawn","special-functors","split","statestack","stm","stm-chans","storable-endian","strict",
    "stringprep","stringsearch","syb","system-fileio","system-filepath","tagsoup","tagstream-conduit",
    "tar","template-haskell","temporary","terminfo","test-framework","test-framework-hunit",
    "test-framework-quickcheck2","test-framework-th","testpack","texmath","text-format","text-icu",
    "text-stream-decode","texts","threads","time","time-compat","transformers","transformers-base",
    "transformers-compat","type-eq","unbounded-delays","uniplate","unix","unix-compat",
    "unix-process-conduit","unix-time","unordered-containers","utf8-light","utf8-string","vault",
    "vector","vector-algorithms","vector-binary-instances","vector-space","vector-th-unbox","wai",
    "wai-app-static","wai-eventsource","wai-extra","wai-logger","wai-test","warp","websockets",
    "websockets-snap","Win32","Win32-notify","wl-pprint","word8","x509","x509-store","x509-system",
    "x509-validation","xhtml","xml","xml-conduit","xml-types","xmlhtml","xss-sanitize","yaml",
    "yesod-auth","yesod-core","yesod-form","yesod-persistent","yesod-routes","zip-archive",
    "zlib","zlib-bindings","zlib-conduit","zlib-enum"]

