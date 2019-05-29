{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Generators for the 'Ledger.Update' values.
module Ledger.Update.Generators
  ( pparams
  , protVer
  -- PVBUMP judgement context generators
  , emptyPVUpdateJC
  , beginningsNoUpdateJC
  , lastProposalJC
  -- UPIState-related generators
  , apName
  , apVer
  , metadata
  -- UPIEC judgement context generators
  , noProtVerChangeJC
  , protVerChangeJC
  )
where

import           Control.State.Transition (Environment, State, Signal)
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Word (Word64)
import           Hedgehog
import           Hedgehog.Gen.Aux (doubleInc)
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range
import           Ledger.Core (Slot(..), SlotCount(..), BlockCount(..), VKeyGenesis(..), minusSlot)
import qualified Ledger.Core.Generators as CG
import           Ledger.GlobalParams (k)
import           Ledger.Update (ProtVer(..), PParams(..), PVBUMP, ApName(..), ApVer(..), Metadata(..), UpId(..), UPIState, UPIEC)
import           Numeric.Natural (Natural)


-- | A judgement context of an STS
type JC sts = (Environment sts, State sts, Signal sts)

-- | Generates a 'ProtVer'
protVer :: Gen ProtVer
protVer =
  (\a b alt -> ProtVer a b alt)
    <$> Gen.integral (Range.linear (0 :: Natural) 100)
    <*> Gen.integral (Range.linear (0 :: Natural) 100)
    <*> Gen.integral (Range.linear (0 :: Natural) 100)

-- | Generates valid protocol parameters
--
-- TODO: The protocol parameters still need to be aligned with the formal
-- spec.
pparams :: Gen PParams
pparams =
  (\((maxBkSz, maxHdrSz, maxTxSz, maxPropSz) :: (Natural, Natural, Natural, Natural))
    (bkSgnCntT :: Double)
    ((bkSlotsPerEpoch, upTtl, stableAfter) :: (SlotCount, SlotCount, BlockCount))
    (scriptVersion :: Natural)
    (cfmThd :: Int)
    (upAdptThd :: Double)
    (factorA :: Int)
    (factorB :: Int)
    -> PParams
      maxBkSz
      maxHdrSz
      maxTxSz
      maxPropSz
      bkSgnCntT
      bkSlotsPerEpoch
      upTtl
      scriptVersion
      cfmThd
      upAdptThd
      stableAfter
      factorA
      factorB
  )
    <$> sz
    <*> doubleInc                                       -- bkSgnCntT
    <*> slotBlock
    <*> Gen.integral (Range.linear (0 :: Natural) 1000) -- scriptVersion
    <*> Gen.integral (Range.linear 0 1000)              -- cfmThd
    <*> Gen.double (Range.constant 0 1)                 -- upAdptThd
    <*> pure 0                                          -- factor @a@
    <*> pure 0                                          -- factor @b@
 where
  -- | Generates maxBkSz, maxHdrSz, maxTxSz and maxPropSz
  sz :: Gen (Natural, Natural, Natural, Natural)
  sz = do
    bkSize <- Gen.integral (Range.linear 1 hi)
    (bkSize,,,)
      <$> gRange bkSize
      <*> gRange bkSize
      <*> gRange bkSize
   where
    lo = 1       :: Natural
    -- In mainnet the maximum header size is set to 2000000 and the maximum
    -- block size is also set to 2000000, so we have to make sure we cover
    -- those values here. The upper bound is arbitrary though.
    hi = 4000000 :: Natural
    gRange :: Natural -> Gen Natural
    gRange upper = Gen.integral (Range.linear lo upper)

  -- | Generates bkSlotsPerEpoch, upTtl and stableAfter
  slotBlock :: Gen (SlotCount, SlotCount, BlockCount)
  slotBlock =
    -- The number of slots per epoch is computed from 'k':
    -- slots per-epoch = k * 10
    let perEpoch = SlotCount $ unBlockCount k *  10 in
    (perEpoch,,)
      <$> (SlotCount  <$> gRange perEpoch)
      <*> (BlockCount <$> gRange perEpoch)
   where
    gRange :: SlotCount -> Gen Word64
    gRange hi = Gen.word64 (Range.linear 1 (unSlotCount hi))

listFads :: Word64 -> Word64 -> Int -> Int -> Gen [(Slot, (ProtVer, PParams))]
listFads loSl hiSl loLen hiLen = Gen.list (Range.linear loLen hiLen) inOneSlot
 where
  inOneSlot :: Gen (Slot, (ProtVer, PParams))
  inOneSlot = (\s v p -> (s, (v, p)))
    <$> CG.slot loSl hiSl
    <*> protVer
    <*> pparams

-- | Generates an environment for the PVBUMP STS with an empty list of
-- updates
pvbumpEmptyListEnv :: Gen (Environment PVBUMP)
pvbumpEmptyListEnv = (, []) <$> CG.slot 0 100000

-- | Generates an environment for the PVBUMP STS such that s_n <= 2 *
-- k
pvbumpBeginningsEnv :: Gen (Environment PVBUMP)
pvbumpBeginningsEnv =
  (,)
    <$> (CG.slot 0 $ 2 * (unBlockCount k))
    <*> listFads 0 100000 0 10

-- | Generates an environment for the PVBUMP STS such that s_n >= 2 *
-- k
pvbumpAfter2kEnv :: Gen (Environment PVBUMP)
pvbumpAfter2kEnv =
  let kv = unBlockCount k in (,)
    <$> CG.slot (2 * kv + 1) (10 * kv)
    <*> ((++)
         <$> listFads 0 1 1 1 -- to ensure there is at least one element
                          -- left after domain restriction in the
                          -- lastProposal property
         <*> listFads 0 (10 * kv) 1 10
        )

-- | Generates a state value for the PVBUMP STS
pvbumpState :: Gen (State PVBUMP)
pvbumpState = (,) <$> protVer <*> pparams

-- | Generates a judgement context for the PVBUMP STS for its property #1
emptyPVUpdateJC :: Gen (JC PVBUMP)
emptyPVUpdateJC = (,,)
  <$> pvbumpEmptyListEnv
  <*> pvbumpState
  <*> pure ()

-- | Generates a judgement context for the PVBUMP STS for its property #2
beginningsNoUpdateJC :: Gen (JC PVBUMP)
beginningsNoUpdateJC = (,,)
  <$> pvbumpBeginningsEnv
  <*> pvbumpState
  <*> pure ()

-- | Generates a judgement context for the PVBUMP STS for its property #3
lastProposalJC :: Gen (JC PVBUMP)
lastProposalJC = (,,)
  <$> pvbumpAfter2kEnv
  <*> pvbumpState
  <*> pure ()


-- | Generates an @ApName@
apName :: Gen ApName
apName = ApName <$> Gen.element ["byron", "shelley", "praos"]

-- | Generates an @ApVer@
apVer :: Gen ApVer
apVer = ApVer <$> Gen.integral (Range.linear 0 100)

-- | Generates @Metadata@
metadata :: Gen Metadata
metadata = pure Metadata

-- | Generates a map for the 'avs' field of an @UPIState@, i.e.,
-- application versions
avs :: Gen (Map ApName (ApVer, Slot, Metadata))
avs = Gen.map (Range.linear 0 10) forAppName where
  forAppName :: Gen (ApName, (ApVer, Slot, Metadata))
  forAppName = (\n v s m -> (n, (v, s, m)))
    <$> apName
    <*> apVer
    <*> CG.slot 0 100000
    <*> metadata

-- | Generates an update proposal id
upId :: Gen UpId
upId = UpId <$> Gen.integral (Range.linear 0 10000)

-- | Generates a map for the 'rpus' field of an @UPIState@, i.e.,
-- registered protocol update proposals
rpus :: Gen (Map UpId (ProtVer, PParams))
rpus = Gen.map (Range.linear 0 10) forUpId where
  forUpId :: Gen (UpId, (ProtVer, PParams))
  forUpId = (\i v p -> (i, (v, p)))
    <$> upId
    <*> protVer
    <*> pparams

-- | Generates a map for the 'raus' field of an @UPIState@, i.e.,
-- registered software update proposals
raus :: Gen (Map UpId (ApName, ApVer, Metadata))
raus = Gen.map (Range.linear 0 10) forUpId where
  forUpId :: Gen (UpId, (ApName, ApVer, Metadata))
  forUpId = (\i n v m -> (i, (n, v, m)))
    <$> upId
    <*> apName
    <*> apVer
    <*> metadata

-- | Generates a map for the 'cps' field of an @UPIState@, i.e.,
-- confirmed proposals
cps :: Gen (Map UpId Slot)
cps = Gen.map (Range.linear 0 10) m where
  m :: Gen (UpId, Slot)
  m = (,) <$> upId <*> CG.slot 0 100000

-- | Generates a set for the 'vts' field of an @UPIState@, i.e.,
-- proposal votes
vts :: Gen (Set (UpId, VKeyGenesis))
vts = Gen.set (Range.linear 0 10) ((,) <$> upId <*> CG.vkgenesis)

-- | Generates a set for the 'bvs' field of an @UPIState@, i.e.,
-- endorsement-key pairs
bvs :: Gen (Set (ProtVer, VKeyGenesis))
bvs = Gen.set (Range.linear 0 10) ((,) <$> protVer <*> CG.vkgenesis)

-- | Generates a map for the 'pws' field of an @UPIState@, i.e.,
-- proposal timestamps
pws :: Gen (Map UpId Slot)
pws = Gen.map (Range.linear 0 10) ((,) <$> upId <*> CG.slot 0 100000)

-- | Generates an @UPIState@ for the UPIEC STS given a protocol
-- version, protocol parameters and future protocol version adoptions
upiecState
  :: ProtVer
  -> PParams
  -> [(Slot, (ProtVer, PParams))]
  -> Gen UPIState
upiecState pv pps fads = (,,,,,,,,)
  <$> pure (pv, pps)
  <*> pure fads
  <*> avs
  <*> rpus
  <*> raus
  <*> cps
  <*> vts
  <*> bvs
  <*> pws

-- | Generates a judgement context for property #1 of the UPIEC STS
noProtVerChangeJC :: Gen (JC UPIEC)
noProtVerChangeJC = do
  ((s_n, fads), (pv, pps), ()) <- pvbumpNoChangeJC
  st                           <- upiecState pv pps fads
  pure (s_n, st, ())
 where
  pvbumpNoChangeJC :: Gen (JC PVBUMP)
  pvbumpNoChangeJC = Gen.choice [emptyPVUpdateJC, beginningsNoUpdateJC]


-- | Generates a judgement context for properties #2, #3 and #4 of the
-- UPIEC STS
protVerChangeJC :: Gen (JC UPIEC)
protVerChangeJC = do
  ((s_n, fads), (pv, pps), ()) <- lastProposalJC
  s'                           <- CG.slot 0 $ upper s_n -- so that it
                                                        -- passes
                                                        -- domain
                                                        -- restriction
                                                        -- on fads
  let
    fad = ( s'
          , ( pv { _pvMin = _pvMin pv + 1 } -- so that pv != pv'
            , pps
            )
          )
    fads' = fads ++ [fad]
  st                           <- upiecState pv pps fads'
  pure (s_n, st, ())
 where
  upper :: Slot -> Word64
  upper s = unSlot (minusSlot s (SlotCount . (2 *) . unBlockCount $ k))
