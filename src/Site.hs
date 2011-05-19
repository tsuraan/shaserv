{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text.Encoding as T
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           Snap.Types
import           Snap.Iteratee
import           Text.Templating.Heist
import qualified Data.ByteString as BS
import           Control.Monad (forM_)

import           Application

import           Data.Hex (hex)
import qualified Crypto.Hash.SHA256 as Hash

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | Handles file uploads; this will stream the uploaded files through a
-- | SHA256 calculator, and once all the files are done, it will display their
-- | names and hashes in a table
upload :: Application ()
upload = do
  req <- getRequest
  -- There has to be a cleaner method to get the content-type, but I didn't
  -- see it.  n.b. 59 is ';' as a Word8
  let tpe = BS.takeWhile (/= 59) $ fromMaybe "" $ getHeader "Content-Type" req

  if (rqMethod req /= POST) || (tpe /= "multipart/form-data")
    then pass
    else doUpload

  where
    doUpload = do
      parts <- handleMultipart defaultUploadPolicy processor
      genResponse parts

    -- generate the initial Iteratee for the given file
    processor info =
      go (fromMaybe "*unnamed*" $ partFileName info)
         Hash.init
         (Chunks [])

    -- get the next Step based on the current stream state
    go name !hash (Chunks []) = returnI $ Continue $ go name hash
    go name !hash (Chunks (x:xs)) = go name
                                      (Hash.update hash x)
                                      (Chunks xs)
    go name !hash EOF = returnI $ Yield (name, Hash.finalize hash) EOF


    -- This really ought to use heist, but I don't see how it does list
    -- iteration, so I'm just generating (really broken) HTML right now
    genResponse parts = do
      putResponse $ (setContentType "text/html")
                  $ (setResponseCode 200)
                  $ emptyResponse
      writeBS "You Uploaded:<br>\n"
      writeBS "<table><tr><td>filename</td><td>hash</td></tr>"
      forM_ parts logFileHtml
      writeBS "</table>"
      writeBS "<a href=\"/\">back</a>"

    logFileHtml (name, hash) = do
      writeBS "<tr><td>"
      writeBS name
      writeBS "</td><td>"
      writeBS $ hex hash
      writeBS "</td></tr>\n"
      

------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             -- if we don't get any POST-ed files, we just redirecto to the
             -- sample's echo page as our error handler...
             , ("/upload", upload <|> redirect "/echo/nopost")
             ]
       <|> serveDirectory "resources/static"

