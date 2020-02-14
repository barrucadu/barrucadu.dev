{-# LANGUAGE DataKinds #-}

module Server where

import           Data.Text           (Text)
import           Data.UUID           (UUID)
import           Servant.API
import           Servant.Auth.Server
import           Servant.Server

import           API

apiServer :: Server Api
apiServer = getEvents :<|> getProjects :<|> projectApiServer where
  projectApiServer name =
    getProject name :<|> getProjectEvents name :<|> getProjectEvent name :<|> postProjectEvent name

getEvents :: Maybe Int -> Server (Get '[JSON] [Event])
getEvents _count = undefined

getProjects :: Server (Get '[JSON] [Project])
getProjects = undefined

getProject :: Text -> Server (Get '[JSON] Project)
getProject _name = undefined

getProjectEvents :: Text -> Maybe Int -> Server (Get '[JSON] [Event])
getProjectEvents _name _count = undefined

getProjectEvent :: Text -> UUID -> Server (Get '[JSON] Event)
getProjectEvent _name _uuid = undefined

postProjectEvent :: Text -> AuthResult Token -> Event -> Server (Post '[JSON] Event)
postProjectEvent _name _auth _event = undefined
