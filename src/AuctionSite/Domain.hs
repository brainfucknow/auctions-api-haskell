-- | Public API for the auction domain.
--
-- This module is a thin /facade/: it curates and re-exports the subset of the
-- @AuctionSite.Domain.*@ submodules that callers (the web layer, the test
-- suite) are meant to depend on, and adds the top-level 'Repository' together
-- with the 'handle' command processor.
--
-- The dependency direction is strictly one-way and acyclic:
--
-- > Core  <-  Bids  <-  States  <-  {SingleSealedBid, TimedAscending}  <-  Auctions  <-  Commands  <-  Domain (this module)
--
-- Submodules never import this facade, so importing @AuctionSite.Domain@
-- introduces no import cycle. To keep the re-exported surface intentional
-- rather than wholesale, every submodule below is imported with an explicit
-- import list naming exactly what this facade exposes or uses internally.
module AuctionSite.Domain (
  -- * Auctions
  AuctionType (..),
  Auction (..),
  AuctionState,
  emptyState,
  validateBid,
  -- * Bids
  Bid (..),
  -- * Core types
  Errors (..),
  UserId,
  AuctionId,
  User (..),
  userId,
  -- * Auction state
  State (..),
  -- * Commands and events
  Command (..),
  Event (..),
  -- * Repository
  Repository,
  auctions,
  handle
) where
import qualified Data.Map as Map
import qualified Data.List as List
import AuctionSite.Domain.Core (Errors (..), UserId, AuctionId, User (..), userId)
import AuctionSite.Domain.Auctions (AuctionType (..), Auction (..), AuctionState, emptyState, validateBid)
import AuctionSite.Domain.Bids (Bid (..))
import AuctionSite.Domain.Commands (Command (..), Event (..))
import AuctionSite.Domain.States (State (..))

type Repository = Map.Map AuctionId (Auction, AuctionState)

auctions::Repository -> [Auction]
auctions r = List.map fst (Map.elems r)
handle:: Command -> Repository -> (Either Errors Event, Repository)
handle state repository =
  case state of
  AddAuction time auction ->
    let aId = auctionId auction
    in
    if Map.member aId repository then
      failureOf $ AuctionAlreadyExists aId
    else if expiry auction <= time then
      failureOf $ AuctionHasEnded aId
    else
      let empty = emptyState auction
          nextRepository= Map.insert aId (auction, empty) repository
      in successOf (AuctionAdded time auction) nextRepository
  PlaceBid time bid ->
    let aId = forAuction bid
    in
    case Map.lookup aId repository of
    Just (auction,state') ->
      case validateBid bid auction of
      Right _ ->
        let (nextAuctionState, bidResult) = addBid bid state'
            nextRepository = Map.insert aId (auction, nextAuctionState) repository
        in
          case bidResult of
          Right _ -> successOf (BidAccepted time bid) nextRepository
          Left err -> failureOf err
      Left err -> failureOf err
    Nothing ->
      failureOf $ AuctionNotFound aId
  where
    failureOf failure = (Left failure, repository)
    successOf success nextRepo = (Right success, nextRepo)

