{-# LANGUAGE TypeFamilies #-}
module Web.Utils where

import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlConn)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.Spock.Shared
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

blaze :: MonadIO m => Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml
{-# INLINE blaze #-}

makeHex :: BS.ByteString -> T.Text
makeHex = T.decodeUtf8 . B16.encode
{-# INLINE makeHex #-}

decodeHex :: T.Text -> BS.ByteString
decodeHex = fst . B16.decode . T.encodeUtf8
{-# INLINE decodeHex #-}

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}
