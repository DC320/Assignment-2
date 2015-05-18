{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Control.Monad    (msum)
import Happstack.Server
    ( Response, ServerPart, Method(POST)
    , BodyPolicy(..), decodeBody, defaultBodyPolicy
    , dir, look, nullConf, ok, simpleHTTP
    , toResponse, methodM
    )
import Text.Blaze                         as B
import Text.Blaze.Html4.Strict            as B hiding (map)
import Text.Blaze.Html4.Strict.Attributes as B hiding ( dir, label
                                                      , title)
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics

data Temperatures = Temperatures { 
	date :: String,
	temp :: Int
      }deriving (Show, Generic)

data AvTemperatures = AvTemperatures { 
	tempAv :: Float
      }deriving (Show, Generic)

data HiTemperatures = HiTemperatures { 
	tempHi :: Integer
      }deriving (Show, Generic)

data LoTemperatures = LoTemperatures { 
	tempLo :: Integer
      }deriving (Show, Generic)

instance FromRow Temperatures where
	fromRow = Temperatures <$> field <*> field

instance FromRow AvTemperatures where
	fromRow = AvTemperatures <$> field

instance FromRow HiTemperatures where
	fromRow = HiTemperatures <$> field

instance FromRow LoTemperatures where
	fromRow = LoTemperatures <$> field

main :: IO ()
main = simpleHTTP nullConf $ handlers

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

handlers :: ServerPart Response
handlers =
    do decodeBody myPolicy
       msum [ dir "everything" $ allResponsePage
	    , dir "average" $ avResponsePage
	    , dir "highest" $ hiResponsePage
	    , dir "lowest" $ loResponsePage
            , mainForm
            ]

mainForm :: ServerPart Response
mainForm = ok $ toResponse $
    html $ do
      B.head $ do
        title "Temperature"
	h1 "Temperatures"
      B.body $ do
        form ! enctype "multipart/form-data"
             ! action "everything" $ do
              B.label "Get everything: "
	      input ! type_ "submit"
                    ! name "All"

	form ! enctype "multipart/form-data"
             ! action "average" $ do
              B.label "Get average: "
	      input ! type_ "submit"
                    ! name "Av"

	form ! enctype "multipart/form-data"
             ! action "highest" $ do
              B.label "Get highest: "
	      input ! type_ "submit"
                    ! name "Hi"

	form ! enctype "multipart/form-data"
             ! action "lowest" $ do
              B.label "Get lowest: "
	      input ! type_ "submit"
                    ! name "Lo"


queryDbAll :: IO [Temperatures]
queryDbAll = do
	
	conn <- open "tempd.db"
	r <- query_ conn "SELECT * FROM table1"
	return r

queryDbAv :: IO [AvTemperatures]
queryDbAv = do
	conn <- open "tempd.db"
	r <- query_ conn "SELECT AVG(temp) FROM table1"
	return r

queryDbHi :: IO [HiTemperatures]
queryDbHi = do
	conn <- open "tempd.db"
	r <- query_ conn "SELECT MAX(temp) FROM table1"
	return r

queryDbLo :: IO [LoTemperatures]
queryDbLo = do
	conn <- open "tempd.db"
	r <- query_ conn "SELECT MIN(temp) FROM table1"
	return r



allResponsePage :: ServerPart Response
allResponsePage = ok $ toResponse $
    html $ do
      B.head $ do
        title "AllTemperatures"
	h1 "All Temperatures"
      B.body $ do
	p $ toHtml $ stringToString $ tempToString $ unsafePerformIO $ queryDbAll

avResponsePage :: ServerPart Response
avResponsePage = ok $ toResponse $
    html $ do
      B.head $ do
        title "AvTemperature"
	h1 "Average Temperature"
      B.body $ do
	p $ toHtml $ stringToString $ avTempToString $ unsafePerformIO $ queryDbAv

hiResponsePage :: ServerPart Response
hiResponsePage = ok $ toResponse $
    html $ do
      B.head $ do
        title "HiTemperature"
	h1 "Highest temperatures"
      B.body $ do
	p $ toHtml $ stringToString $ hiTempToString $ unsafePerformIO $ queryDbHi

loResponsePage :: ServerPart Response
loResponsePage = ok $ toResponse $
    html $ do
      B.head $ do
        title "LoTemperature"
	h1 "Lowest temperatures"
      B.body $ do
	p $ toHtml $ stringToString $ loTempToString $ unsafePerformIO $ queryDbLo


selectDate :: Temperatures -> String
selectDate (Temperatures d _) = d

selectTemp :: Temperatures -> Int
selectTemp (Temperatures _ t) = t


tempToString :: [Temperatures] -> [String]
tempToString (x:xs) = ( (selectDate x) ++ " - " ++ show(selectTemp x)) : tempToString xs
tempToString [] = []

stringToString :: [String] -> String
stringToString (x:xs) = x ++ ", " ++ stringToString xs
stringToString [] = ""

avTempToString :: [AvTemperatures] -> [String]
avTempToString (x:xs) = show(x) : avTempToString xs
avTempToString [] = []

hiTempToString :: [HiTemperatures] -> [String]
hiTempToString (x:xs) = show(x) : hiTempToString xs
hiTempToString [] = []

loTempToString :: [LoTemperatures] -> [String]
loTempToString (x:xs) = show(x) : loTempToString xs
loTempToString [] = []




