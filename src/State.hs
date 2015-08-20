{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell
           , RecordWildCards, TypeFamilies #-}

module State
    ( Id(..)
    -- * Binaries
    , Binary(..)
    , Binaries
    , AddBinary(..)
    , GetBinaryById(..)
    , GetBinaryByPath(..)
    , GetBinariesByName(..)
    -- * Immutable files
    , File(..)
    , getFileText
    , getFileBS
    , FileMap
    -- * I/O
    , Input(..)
    , Output(..)
    , Parameters(..)
    , Limits(..)
    , TaskConfig(..)
    ) where

import Control.Monad.Reader ( ask )
import Control.Monad.State ( get, put )
import System.FilePath
import System.IO.Unsafe ( unsafePerformIO )

import Data.Acid
import Data.IxSet as IxSet
import Data.SafeCopy
import Data.Data ( Data, Typeable )
import Data.Default

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Encoding
import qualified Data.ByteString as BS

import qualified Data.Map as Map

-- | Unqie identifier of record, useful for cross-referrencing
newtype Id a = Id { getId :: Integer }
           deriving ( Eq, Ord, Show, Read, Data, Typeable, Enum )
$(deriveSafeCopy 0 'base ''Id)

-- | Binary describtion
data Binary = Binary
    { binaryId          :: Id Binary
    , binaryDir         :: FilePath
    , binaryPath        :: FilePath -- ^ relative to binaryDir
    , binaryName        :: String
    , binaryVersion     :: String
    , binaryDescription :: String
    , binaryTags        :: [String]
    } deriving ( Eq, Ord, Show, Read, Data, Typeable )
$(deriveSafeCopy 0 'base ''Binary)

newtype Name = Name String
             deriving ( Eq, Ord, Show, Read, Data, Typeable )
newtype Path = Path String
             deriving ( Eq, Ord, Show, Read, Data, Typeable )
newtype Version = Version String
                deriving ( Eq, Ord, Show, Read, Data, Typeable )
newtype Tag = Tag String
            deriving ( Eq, Ord, Show, Read, Data, Typeable )

instance Indexable Binary where
    empty = ixSet
        [ ixFun $ (:[]) . binaryId
        , ixFun $ \Binary {..} -> [ Path $ binaryDir </> binaryPath ]
        , ixFun $ (:[]) . Name . binaryName
        , ixFun $ (:[]) . Version . binaryVersion
        , ixFun $ fmap Tag . binaryTags
        ]

-- | 'Binary' storage
data Binaries = Binaries
    { nextBinaryId :: Id Binary
    , binaries :: IxSet Binary
    } deriving ( Eq, Ord, Show, Read, Data, Typeable )
$(deriveSafeCopy 0 'base ''Binaries)

instance Default Binaries where
    def = Binaries { nextBinaryId = Id 1, binaries = IxSet.empty }

addBinary :: Binary -> Update Binaries Binary
addBinary bin0 = do
    bs@Binaries {..} <- get
    let bin = bin0 { binaryId = nextBinaryId }
    put $ bs { nextBinaryId = succ nextBinaryId
             , binaries = IxSet.insert bin binaries
             }
    return bin

getAllBinaries :: Query Binaries (IxSet Binary)
getAllBinaries = binaries <$> ask

getBinariesBy :: Typeable k => k -> Query Binaries (IxSet Binary)
getBinariesBy k = do
    Binaries {..} <- ask
    return $ binaries @= k

getBinaryById :: Id Binary -> Query Binaries (Maybe Binary)
getBinaryById = fmap getOne . getBinariesBy

getBinariesByName :: String -> Query Binaries (IxSet Binary)
getBinariesByName = getBinariesBy . Name

getBinaryByPath :: FilePath -> Query Binaries (Maybe Binary)
getBinaryByPath = fmap getOne . getBinariesBy . Path

$(makeAcidic ''Binaries [ 'addBinary, 'getAllBinaries, 'getBinaryById
                        , 'getBinariesByName, 'getBinaryByPath
                        ])

-- | Immutable file store either in memory or on filesystem
data File = FileRef FilePath
          | TextFile Text.Text
          | BinaryFile BS.ByteString
          deriving ( Eq, Show, Read, Data, Typeable )
$(deriveSafeCopy 0 'base ''File)


-- | materialize 'File' into 'Text.Text', note that as 'File' always represents
-- immutable file, even if it is store on filesystem as 'FileRef', this function
-- can be pure even though it performs 'IO' inside.
--
-- 'BinaryFile' is treated as UTF8 (using 'decodeUtf8')
getFileText :: File -> Text.Text
getFileText (TextFile t)     = t
getFileText (BinaryFile bin) = decodeUtf8 bin
getFileText (FileRef ref)    = unsafePerformIO $ Text.readFile ref

-- | materialize 'File' into 'BS.ByteString', note that as 'File' always represents
-- immutable file, even if it is store on filesystem as 'FileRef', this function
-- can be pure even though it performs 'IO' inside.
getFileBS :: File -> BS.ByteString
getFileBS (TextFile t)   = encodeUtf8 t
getFileBS (BinaryFile b) = b
getFileBS (FileRef ref)  = unsafePerformIO $ BS.readFile ref

-- | map from file names to 'File'
type FileMap = Map.Map String File

-- | input to task
data Input = Input
    { stdin :: File
    , extraInputs :: FileMap
    } deriving ( Eq, Show, Read, Data, Typeable )
$(deriveSafeCopy 0 'base ''Input)

-- | output from task
data Output = Output
    { stdout       :: File
    , stderr       :: File
    , extraOutputs :: FileMap
    } deriving ( Eq, Show, Read, Data, Typeable )
$(deriveSafeCopy 0 'base ''Output)

data Parameters = ShellParams String
                | RawParams [String]
                deriving ( Eq, Show, Read, Data, Typeable )
$(deriveSafeCopy 0 'base ''Parameters)

data Limits = Limits
    { maxCpus   :: Maybe Integer
    , maxMemory :: Maybe Integer -- ^ bytes
    , maxTime   :: Maybe Integer -- ^ seconds
    , maxCore   :: Maybe Integer -- ^ bytes (@Just 0@ ~ disalowed)
    } deriving ( Eq, Show, Read, Data, Typeable )
$(deriveSafeCopy 0 'base ''Limits)

-- | Everythign is unlimited by default, except for core wich is not allowed
instance Default Limits where
    def = Limits { maxCpus = Nothing
                 , maxMemory = Nothing
                 , maxTime = Nothing
                 , maxCore = Just 0
                 }

-- | Task configuration
data TaskConfig = TaskConfig
    { binary     :: Id Binary
    , parameters :: Parameters
    , inputs     :: Input
    , limits     :: Limits
    } deriving ( Eq, Show, Read, Data, Typeable )
$(deriveSafeCopy 0 'base ''TaskConfig)
