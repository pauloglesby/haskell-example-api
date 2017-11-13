{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Wiggly.Data.Markdown
       (
         Markdown(..)
       ) where

import Prelude ()
import Prelude.Compat

import Servant

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Media as Media

data Markdown

instance Accept Markdown where
  contentType _ = "text" Media.// "markdown" Media./: ("charset", "utf-8")

instance MimeRender Markdown T.Text where
  mimeRender = markdownMimeRender

-- the horror
markdownMimeRender :: Proxy Markdown -> T.Text -> LBS.ByteString
markdownMimeRender _ val = lazy_encoded (encoded val)
  where encoded :: T.Text -> BS.ByteString
        encoded = TE.encodeUtf8
        lazy_encoded :: BS.ByteString -> LBS.ByteString
        lazy_encoded = LBS.fromStrict
