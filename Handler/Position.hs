module Handler.Position where

import Import
import Database.MongoDB.Query (runCommand)
import Data.Bson ((=:))

optionsPositionR :: Handler RepPlain
optionsPositionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, OPTIONS"
    return $ RepPlain $ toContent ("" :: Text)


data FormPosition = FormPosition
  {  fbId       :: Text
  ,  longitude  :: Double
  ,  latitude   :: Double
  }
  deriving Show

positionForm :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => FormInput m FormPosition
positionForm = FormPosition
    <$> ireq textField   "fb_id"
    <*> ireq doubleField "longitude"
    <*> ireq doubleField "latitude"

geoNear :: Double -> Double -> Handler Value
geoNear lon lat = runDB $ runCommand q >>= (return . fromDocument)
  where
    q = [ "geoNear"   =: ( "position"::Text ),
          "near"      =: [ "type"        =: ( "Point"::Text ),
                           "coordinates" =: ( [ lon , lat ]::[Double] ) ],
          "spherical" =: True
        ]

postPositionR :: Handler Value
postPositionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    (FormPosition fbId lon lat) <- runInputPost positionForm
    now <- liftIO $ getCurrentTime
    _ <- createOrUpdate $ Position fbId [lon,lat] now
    geoNear lon lat

putPositionR :: Handler Value
putPositionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    (FormPosition fbId lon lat) <- runInputPost positionForm
    now <- liftIO $ getCurrentTime
    _ <- createOrUpdate $ Position fbId [lon,lat] now
    geoNear lon lat

createOrUpdate :: Position -> Handler (Key Position)
createOrUpdate position@(Position fbId coords now) = do
    maybeDocument <- runDB $ getBy $ UniqueFbId fbId
    case maybeDocument of
        Nothing -> (runDB $ insert $ position)
        Just entity -> (runDB $ update key q) >> return key
          where
            key = entityKey entity
            q = [PositionPosition =. coords, PositionTimestamp =. now]

getPositionR :: Handler Value
getPositionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    (FormPosition fbId lon lat) <- runInputGet positionForm
    now <- liftIO $ getCurrentTime
    _ <- createOrUpdate $ Position fbId [lon,lat] now
    geoNear lon lat
