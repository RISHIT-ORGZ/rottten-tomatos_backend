import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson as JSON
import           Data.Int                     (Int64 (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Database.Persist
import           Database.Persist.MySQL     (ConnectInfo (..),
                                               SqlBackend (..),
                                               defaultConnectInfo, fromSqlKey, runMigration,
                                               runSqlPool, toSqlKey, withMySQLConn)
import           Database.Persist.Sql         (SqlPersistT, runSqlConn)
import           Database.Persist.TH          (mkMigrate, mkPersist,
                                               persistLowerCase, share,
                                               sqlSettings)
import           Database.Persist.Types       (PersistValue(PersistInt64))
import           Servant                      (Handler, throwError)

import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           System.Environment           (getArgs)

import Lib


main :: IO ()
main = do
    args <- getArgs
    let arg1 = if not (null args) then Just (head args) else Nothing
    case arg1 of
        Just "migrate" -> doMigration
        _              -> run 8080 app
