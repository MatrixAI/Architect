import SessionType
import qualified Test.Hspec as Hspec
import Test.Hspec (hspec, it, describe, shouldSatisfy, shouldNotSatisfy,
    shouldBe)
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, arbitrary, forAll, (==>), (===), (.&&.))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing)
import Control.Applicative (liftA2)

instance Arbitrary Type where
    arbitrary = QC.elements [INT, STRING, BOOL]

-- Takes a max reference and a max size to make arbitrary branches
sizedRefMap :: Integer -> Int -> QC.Gen Branches
sizedRefMap maxref size = do
    branches <- QC.choose (1,size)
    list <- QC.vectorOf branches $ do
        key <- liftA2 (:) arbitrary arbitrary
        value <- sizedRefSession maxref $ (size-1) `quot` branches
        return (key, value)
    return $ Map.fromList list

-- Takes a max reference and a max size to make an arbitrary session type
sizedRefSession :: Integer -> Int -> QC.Gen SessionType
sizedRefSession _ 0 = return end
sizedRefSession 0 1 = QC.oneof
    [ send <$> arbitrary
    , recv <$> arbitrary
    , return end
    ]
sizedRefSession maxref 1 = QC.oneof
    [ sizedRefSession 0 1
    , ref <$> QC.choose (1,maxref)
    ]
sizedRefSession maxref size = QC.oneof
    [ liftA2 join (sizedRefSession maxref 1) (sizedRefSession maxref (size-1))
    , choose <$> sizedRefMap maxref size
    , offer <$> sizedRefMap maxref size
    , mu <$> sizedRefSession (maxref+1) (size-1)
    ]

sizedSession :: Int -> QC.Gen SessionType
sizedSession = sizedRefSession 0

validSession :: QC.Gen SessionType
validSession = QC.sized sizedSession

invalidSession :: QC.Gen SessionType
invalidSession = QC.elements
    [ ref (-1)
    , choose Map.empty
    , offer Map.empty
    , choose $ Map.singleton "" end
    , offer $ Map.singleton "" end
    ]

anySession :: QC.Gen SessionType
anySession = QC.oneof [validSession, invalidSession]

{-
    Boolean Implication, (False --> x) == True, (x --> True) == True,
    (True --> False) == False
-}
infix 4 -->
(-->) :: Bool -> Bool -> Bool
(-->) = (<=)

main :: IO ()
main = hspec $ do
    describe "SessionType.isValid" $ do
        it "checks if a session type is valid" $ do
            end `shouldSatisfy` isValid
            send INT `shouldSatisfy` isValid
            recv STRING `shouldSatisfy` isValid
            (choose $ Map.singleton "a" end) `shouldSatisfy` isValid
            (offer $ Map.singleton "get" end) `shouldSatisfy` isValid
            mu end `shouldSatisfy` isValid
            mu (send BOOL `join` ref 1) `shouldSatisfy` isValid

            -- Choosing from no options is invalid
            choose Map.empty `shouldNotSatisfy` isValid

            -- Options must have a name
            (offer $ Map.singleton "" end) `shouldNotSatisfy` isValid

            -- References should be in bounds
            ref 1 `shouldNotSatisfy` isValid

        it "passes valid SessionTypes" $
            forAll validSession $ \a ->
                isValid a

        it "fails invalid SessionTypes" $
            forAll invalidSession $ \a ->
                not $ isValid a

        it "checks concatenated SessionTypes are valid" $
            forAll anySession $ \a ->
            forAll anySession $ \b ->
                isValid (a `join` b) === (isValid a && isValid b)

        it "checks a SessionType in Choose is valid" $
            forAll anySession $ \a ->
                \c s -> isValid (choose $ Map.singleton (c:s) a) === isValid a

        it "checks a SessionType in Offer is valid" $
            forAll anySession $ \a ->
                \c s -> isValid (offer $ Map.singleton (c:s) a) === isValid a

        it "won't fail a valid SessionType in Mu" $
            forAll validSession $ \a ->
                isValid (mu a)

    describe "SessionType.isSubType" $ do
        it "checks if one SessionType is a subtype of another" $ do
            end `shouldSatisfy` (<: end)
            send STRING `shouldSatisfy` (<: send STRING)
            recv INT `shouldSatisfy` (<: recv INT)

            -- Choosing from fewer options is more general
            (choose $ Map.singleton "b" end) `shouldSatisfy`
                (<: choose (Map.fromList [("a",end),("b",end)]))

            -- Offering more options is more general
            (offer $ Map.fromList [("get",end),("post",end)]) `shouldSatisfy`
                (<: offer (Map.singleton "post" end))

            -- Choosing from more options is not more general
            (choose $ Map.fromList [("a",end),("b",end)]) `shouldNotSatisfy`
                (<: choose (Map.singleton "a" end))

            -- Offering fewer options is not more general
            (offer $ Map.singleton "get" end) `shouldNotSatisfy`
                (<: offer (Map.fromList [("get",end),("post",end)]))

            -- Checking that nesting Offer inside Choose works as expected
            (choose $ Map.singleton "a" (offer $ Map.fromList [("c",end),
                ("d",end)])) `shouldSatisfy` (<: choose (Map.fromList
                    [("a",offer $ Map.singleton "c" end),("b",end)]))

            -- Checking that nesting Choose inside Offer works as expected
            (offer $ Map.fromList [("a",choose $ Map.singleton "c" end),
                ("b",end)]) `shouldSatisfy` (<: offer (Map.singleton "a"
                (choose $ Map.fromList [("c",end), ("d",end)])))

            -- Longer protocol should not be subtype of shorter protocol
            (send STRING `join` recv INT) `shouldNotSatisfy` (<: send STRING)
            (send STRING `join` recv INT) `shouldNotSatisfy` (<: recv INT)
            send STRING `shouldNotSatisfy` (<: (send STRING `join` recv INT))
            recv INT `shouldNotSatisfy` (<: (send STRING `join` recv INT))

            -- TODO: include basic loop example

        -- Relation properties

        it "is reflexive" $
            forAll validSession $ \a ->
                a <: a

        it "is antisymmetric" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                (a == b) === (a <: b && b <: a)

        it "is transitive" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
            forAll validSession $ \c ->
                (a <: b && b <: c) --> a <: c

        -- Nesting properties

        it "matches end to end only" $
            forAll validSession $ \a ->
                (a <: end || end <: a) === (a == end)

        it "determines subtype of concatenated sessiontypes" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
            forAll validSession $ \c ->
                ((a <: b) === (a `join` c <: b `join` c)) .&&.
                ((a <: b) === (c `join` a <: c `join` b))

        it "determines subtype inside a Choose" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                \s -> (a <: b) ===
                (choose $ Map.singleton s a) <: (choose $ Map.singleton s b)

        it "determines subtype inside an Offer" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                \s -> (a <: b) ===
                (offer $ Map.singleton s a) <: (offer $ Map.singleton s b)

        it "determines subtype inside a Mu" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                (a <: b) === (mu a <: mu b)

    describe "SessionType.dual" $ do
        it "constructs the dual of a SessionType" $ do
            dual end `shouldBe` end
            dual (send STRING) `shouldBe` recv STRING
            dual (recv BOOL) `shouldBe` send BOOL
            dual (choose $ Map.singleton "post" (send INT)) `shouldBe`
                (offer $ Map.singleton "post" (recv INT))
            dual (offer $ Map.singleton "get" (recv INT)) `shouldBe`
                (choose $ Map.singleton "get" (send INT))

    --     it "is its own inverse" $ property $
    --         \a -> dual (dual a) == a
    --
    --     it "is bijective" $ property $
    --         \a b -> (dual a == dual b) == (a == b)
    --
    --     it "constructs the dual of a SessionType in Send" $ property $
    --         \a t -> dual (Send t a) == Recv t (dual a)
    --
    --     it "constructs the dual of a SessionType in Recv" $ property $
    --         \a t -> dual (Recv t a) == Send t (dual a)
    --
    --     it "constructs the dual of a SessionType in Choose" $ property $
    --         \a s -> dual (Choose $ Map.singleton s a) ==
    --             Offer (Map.singleton s $ dual a)
    --
    --     it "constructs the dual of a SessionType in Option" $ property $
    --         \a s -> dual (Offer $ Map.singleton s a) ==
    --             Choose (Map.singleton s $ dual a)
    --
    --     it "inverts subtypes" $ property $
    --         \a b -> a <: b == dual b <: dual a
    --
    describe "SessionType.isCompatible" $ do
        it "checks if two session types can communicate" $ do
            end `shouldSatisfy` (<=> end)
            send BOOL `shouldSatisfy` (<=> recv BOOL)
            recv INT `shouldSatisfy` (<=> send INT)

            (choose $ Map.singleton "a" end) `shouldSatisfy`
                (<=> offer (Map.fromList [("a",end),("b",end)]))

            (offer $ Map.singleton "a" end) `shouldNotSatisfy`
                (<=> choose (Map.fromList [("a",end),("b",end)]))

    --     -- Relation property
    --     it "is symmetric" $ property $
    --         \a b -> a <=> b == b <=> a
    --
    --     it "is always satisfied by duals" $ property $
    --         \a -> a <=> dual a && dual a <=> a
    --
    --     -- Nesting properties
    --
    --     it "determines compatibility inside a Send/Recv" $ property $
    --         \a b t -> a <=> b == Send t a <=> Recv t b
    --
    --     it "determines compatibility inside a Choose/Offer" $ property $
    --         \a b s -> a <=> b ==
    --             Offer (Map.singleton s a) <=> Choose (Map.singleton s b)
    --
    --
    --
    --     it "matches Wait to Kill only" $ property $
    --         \a -> a <=> Wait == (a == Kill)
    --
    --     it "matches Kill to Wait only" $ property $
    --         \a -> a <=> Kill == (a == Wait)
    --
    --     it "matches subtypes when supertypes match" $ property $
    --         \a b c -> (a <: b && b <=> c) --> a <=> c
    --
    -- describe "SessionType.strictUnion" $ do
    --     it "combines two protocols if possible and unambiguous" $ do
    --         strictUnion (Choose $ Map.singleton "a" Wait)
    --             (Choose $ Map.singleton "b" Kill) `shouldBe`
    --             (Just $ Choose (Map.fromList [("a",Wait),("b",Kill)]))
    --         strictUnion (Offer $ Map.singleton "get" Kill)
    --             (Offer $ Map.singleton "post" Wait) `shouldBe`
    --             (Just $ Offer (Map.fromList [("get",Kill),("post",Wait)]))
    --
    --         strictUnion (Choose $ Map.singleton "a" Wait)
    --             (Choose $ Map.singleton "a" Wait) `shouldBe` Nothing
    --         strictUnion (Offer $ Map.singleton "get" Kill)
    --             (Offer $ Map.singleton "get" Wait) `shouldBe` Nothing
    --
    --     it "never combines Wait" $ property $
    --         \a -> isNothing $ strictUnion Wait a
    --
    --     it "never combines Kill" $ property $
    --         \a -> isNothing $ strictUnion Kill a
    --
    --     it "never combines Send" $ property $
    --         \a b t -> isNothing $ strictUnion (Send t a) b
    --
    --     it "never combines Recv" $ property $
    --         \a b t -> isNothing $ strictUnion (Recv t a) b
    --
    --     it "never combines valid SessionTypes with themselves" $ property $
    --         \a -> isValid a --> isNothing (strictUnion a a)
    --
    --     it "never combines clashing names in Choose" $ property $
    --         \a b s -> isNothing $ strictUnion (Choose $ Map.singleton s a)
    --             (Choose $ Map.singleton s b)
    --
    --     it "never combines clashing names in Offer" $ property $
    --         \a b s -> isNothing $ strictUnion (Offer $ Map.singleton s a)
    --             (Offer $ Map.singleton s b)
    --
    --     it "never combines Choose and Offer" $ property $
    --         \a s t -> isNothing $ strictUnion (Choose $ Map.singleton s a)
    --             (Offer $ Map.singleton t a)
    --
    --     it "combines non-clashing names in Choose" $ property $
    --         \a b s t -> (s /= t) --> isJust (strictUnion
    --             (Choose $ Map.singleton s a) (Choose $ Map.singleton t b))
    --
    --     it "combines non-clashing names in Offer" $ property $
    --         \a b s t -> (s /= t) --> isJust (strictUnion
    --             (Offer $ Map.singleton s a) (Offer $ Map.singleton t b))
    --
    --     it "creates supertypes in Choose" $ property $
    --         \a b s t -> (s /= t) --> ((((Choose $ Map.singleton s a) <:) <$>
    --             strictUnion (Choose $ Map.singleton s a)
    --             (Choose $ Map.singleton t b)) == Just True)
    --
    --     it "creates subtypes in Offer" $ property $
    --         \a b s t -> (s /= t) --> (((<: (Offer $ Map.singleton s a)) <$>
    --             strictUnion (Offer $ Map.singleton s a)
    --             (Offer $ Map.singleton t b)) == Just True)
    --
    --     it "is symmetric" $ property $
    --         \a b -> strictUnion a b == strictUnion b a
    --
    --     it "is associative" $ property $
    --         \a b c -> (isJust (strictUnion a b) && isJust (strictUnion b c))
    --             --> ((strictUnion a <$> strictUnion b c) ==
    --                 (($ c) <$> (strictUnion <$> strictUnion a b)))
    --
    -- describe "SessionType.smartUnion" $ do
    --     it "combines two protocols if possible" $ do
    --         smartUnion (Choose $ Map.singleton "a" Kill)
    --             (Choose $ Map.fromList [("a",Kill),("b",Wait)]) `shouldBe`
    --             Just (Choose $ Map.fromList [("a",Kill),("b",Wait)])
    --
    --     it "works for everything strictUnion works for" $ property $
    --         \a b -> isJust (strictUnion a b) -->
    --             (smartUnion a b == strictUnion a b)
    --
    --     it "never combines Wait" $ property $
    --         \a -> isNothing $ smartUnion Wait a
    --
    --     it "never combines Kill" $ property $
    --         \a -> isNothing $ smartUnion Kill a
    --
    --     it "never combines Send" $ property $
    --         \a b t -> isNothing $ smartUnion (Send t a) b
    --
    --     it "never combines Recv" $ property $
    --         \a b t -> isNothing $ smartUnion (Recv t a) b
    --
    --     it "is symmetric" $ property $
    --         \a b -> smartUnion a b == smartUnion b a
    --
    --     it "is associative" $ property $
    --         \a b c -> (isJust (smartUnion a b) && isJust (smartUnion b c))
    --             --> ((smartUnion a <$> smartUnion b c) ==
    --                 (($ c) <$> (smartUnion <$> smartUnion a b)))
    --
    --
    -- describe "SessionType.union" $ do
    --     it "combines two SessionTypes if possible" $ do
    --         union (Choose $ Map.singleton "a" Kill)
    --             (Choose $ Map.singleton "b" Wait) `shouldBe`
    --             Just (Choose $ Map.fromList [("a",Kill),("b",Wait)])
    --         union (Offer $ Map.singleton "a" Kill)
    --             (Offer $ Map.fromList [("a",Kill),("b",Wait)]) `shouldBe`
    --             Just (Offer $ Map.fromList [("a",Kill),("b",Wait)])
    --
    --         union (Choose $ Map.singleton "a" Kill)
    --             (Choose $ Map.singleton "a" Wait) `shouldBe` Nothing
    --
    --     it "works for everything smartUnion works for" $ property $
    --         \a b -> isJust (smartUnion a b) --> (union a b == smartUnion a b)
    --
    --     it "combines Wait with Wait only" $ property $
    --         \a -> isJust (union a Wait) == (a == Wait)
    --
    --     it "combines Kill with Kill only" $ property $
    --         \a -> isJust (union a Kill) == (a == Kill)
    --
    --     it "combines the contents of a Send" $ property $
    --         \a b t -> (Send t <$> union a b) == union (Send t a) (Send t b)
    --
    --     it "combines the contents of a Recv" $ property $
    --         \a b t -> (Recv t <$> union a b) == union (Recv t a) (Recv t b)
    --
    --     it "combines the contents of a Choose" $ property $
    --         \a b s -> (Choose . (Map.singleton s) <$> union a b) == union
    --             (Choose $ Map.singleton s a) (Choose $ Map.singleton s b)
    --
    --     it "combines the contents of an Offer" $ property $
    --         \a b s -> (Offer . (Map.singleton s) <$> union a b) == union
    --             (Offer $ Map.singleton s a) (Offer $ Map.singleton s b)
    --
    --     it "combines anything with itself" $ property $
    --         \a -> union a a == Just a
    --
    --     it "is symmetric" $ property $
    --         \a b -> union a b == union b a
    --
    --     it "is associative" $ property $
    --         \a b c -> (isJust (union a b) && isJust (union b c)) -->
    --             ((union a <$> union b c) == (($ c) <$> (union <$> union a b)))
