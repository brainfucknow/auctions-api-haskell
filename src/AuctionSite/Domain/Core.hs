{-# LANGUAGE OverloadedStrings #-}
module AuctionSite.Domain.Core where
import AuctionSite.Money
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as ATyp

type UserId = T.Text
data User =
  BuyerOrSeller UserId T.Text
  | Support UserId
  deriving (Eq, Generic, Show)
userId :: User -> UserId
userId (BuyerOrSeller userId' _) = userId'
userId (Support userId') = userId'

instance ToJSON User where
  toJSON (BuyerOrSeller uid name) = object ["type" .= ("BuyerOrSeller" :: T.Text), "id" .= uid, "name" .= name]
  toJSON (Support uid)            = object ["type" .= ("Support" :: T.Text), "id" .= uid]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    typ' <- o .: "type"
    case (typ' :: T.Text) of
      "BuyerOrSeller" -> BuyerOrSeller <$> o .: "id" <*> o .: "name"
      "Support"       -> Support <$> o .: "id"
      _               -> ATyp.prependFailure "parsing User failed, " (fail $ "unknown type: " ++ T.unpack typ')

type AuctionId = Integer

data Errors =
  AuctionNotFound AuctionId
  | AuctionAlreadyExists AuctionId
  | AuctionHasEnded AuctionId
  | AuctionHasNotStarted AuctionId
  | SellerCannotPlaceBids (UserId , AuctionId)
  | InvalidUserData String
  | MustPlaceBidOverHighestBid AmountValue
  | AlreadyPlacedBid
  | InvalidAuctionDates AuctionId
  deriving (Eq,Show)

instance ToJSON Errors where
  toJSON (AuctionNotFound a)             = object ["type" .= String "AuctionNotFound", "auctionId" .= a]
  toJSON (AuctionAlreadyExists a)       = object ["type" .= String "AuctionAlreadyExists", "auctionId" .= a]
  toJSON (AuctionHasEnded a)            = object ["type" .= String "AuctionHasEnded", "auctionId" .= a]
  toJSON (AuctionHasNotStarted a)       = object ["type" .= String "AuctionHasNotStarted", "auctionId" .= a]
  toJSON (SellerCannotPlaceBids (u, a)) = object ["type" .= String "SellerCannotPlaceBids", "userId" .= u, "auctionId" .= a]
  toJSON (InvalidUserData u)            = object ["type" .= String "InvalidUserData", "user" .= u]
  toJSON (MustPlaceBidOverHighestBid a) = object ["type" .= String "MustPlaceBidOverHighestBid", "amount" .= a]
  toJSON AlreadyPlacedBid               = object ["type" .= String "AlreadyPlacedBid"]
  toJSON (InvalidAuctionDates a)        = object ["type" .= String "InvalidAuctionDates", "auctionId" .= a]
