module Handler.Position where

import Import
import Database.MongoDB.Query (runCommand)
import Data.Bson ((=:))

optionsPositionR :: Handler RepPlain
optionsPositionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, OPTIONS"
    return $ RepPlain $ toContent ("" :: Text)

postPositionR :: Handler Value
postPositionR = respondPositionR runInputPost

putPositionR :: Handler Value
putPositionR = postPositionR

getPositionR :: Handler Value
getPositionR = respondPositionR runInputGet

respondPositionR :: (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
                    (FormInput m FormPosition -> Handler FormPosition) -> Handler Value
respondPositionR formT = do
    addHeader "Access-Control-Allow-Origin" "*"
    form <- formT positionForm
    now <- liftIO $ getCurrentTime
    _ <- createOrUpdate $ (toPosition form now)
    geoNear (longitude form) (latitude form)

data FormPosition = FormPosition
  {  fbId       :: Text
  ,  longitude  :: Double
  ,  latitude   :: Double
  ,  firstName  :: Text
  ,  username   :: Text
  ,  gender     :: Maybe Text
  ,  timezone   :: Maybe Int
  ,  locale     :: Maybe Text
  }
  deriving Show

positionForm :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => FormInput m FormPosition
positionForm = FormPosition
    <$> ireq textField   "fb_id"
    <*> ireq doubleField "longitude"
    <*> ireq doubleField "latitude"
    <*> ireq textField "first_name"
    <*> ireq textField "username"
    <*> iopt textField "gender"
    <*> iopt intField "timezone"
    <*> iopt textField "locale"

toPosition :: FormPosition -> UTCTime -> Position
toPosition (FormPosition fbId lon lat fname uname gender timezone locale) timestamp =
  (Position fbId uname fname gender timezone locale [lon,lat] timestamp)

geoNear :: Double -> Double -> Handler Value
geoNear lon lat = runDB $ runCommand q >>= (return . fromDocument)
  where
    q = [ "geoNear"   =: ( "position"::Text ),
          "near"      =: [ "type"        =: ( "Point"::Text ),
                           "coordinates" =: ( [ lon , lat ]::[Double] ) ],
          "spherical" =: True
        ]

createOrUpdate :: Position -> Handler (Key Position)
createOrUpdate position@(Position fbId _ _ _ _ _ coords now) = do
    maybeDocument <- runDB $ getBy $ UniqueFbId fbId
    case maybeDocument of
        Nothing -> (runDB $ insert $ position)
        Just entity -> (runDB $ update key q) >> return key
          where
            key = entityKey entity
            q = [PositionPosition =. coords, PositionTimestamp =. now]


