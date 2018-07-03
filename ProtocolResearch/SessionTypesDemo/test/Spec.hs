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

-- Takes a max reference and a max size to make arbitrary valid branches
sizedRefMap :: Integer -> Int -> QC.Gen Branches
sizedRefMap maxref size = do
    branches <- QC.choose (1,size)
    list <- QC.vectorOf branches $ do
        key <- liftA2 (:) arbitrary arbitrary
        value <- sizedRefSession maxref $ (size-1) `quot` branches
        return (key, value)
    return $ Map.fromList list

-- Takes a max reference and a max size to make an arbitrary valid session type
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
    [ do
        firstSize <- QC.choose (1,size-1)
        first <- sizedRefSession maxref firstSize
        second <- sizedRefSession maxref (size-firstSize)
        return $ join first second
    , choose <$> sizedRefMap maxref (size-1)
    , offer <$> sizedRefMap maxref (size-1)
    , mu <$> sizedRefSession (maxref+1) (size-1)
    ]

-- Takes a max size to make an arbitrary valid session type
sizedSession :: Int -> QC.Gen SessionType
sizedSession = sizedRefSession 0

validSession :: QC.Gen SessionType
validSession = QC.sized sizedSession

smallInvalid :: QC.Gen SessionType
smallInvalid = QC.elements
    [ ref 0
    , choose Map.empty
    , offer Map.empty
    , choose $ Map.singleton "" end
    , offer $ Map.singleton "" end
    ]

-- Makes an invalid session type
invalidSession :: QC.Gen SessionType
invalidSession = do
    before <- validSession
    invalid <- smallInvalid
    after <- validSession
    return $ before ++ invalid ++ after

-- 50/50 valid and invalid session types
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

        it "won't fail a valid SessionType in a fixpoint" $
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

        it "preserves subtypes when loops are added" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                (a <: b) === (mu (a `join` ref 1) <: mu (b `join` ref 1))

    describe "SessionType.dual" $ do
        it "constructs the dual of a SessionType" $ do
            dual end `shouldBe` end
            dual (send STRING) `shouldBe` recv STRING
            dual (recv BOOL) `shouldBe` send BOOL
            dual (choose $ Map.singleton "post" (send INT)) `shouldBe`
                (offer $ Map.singleton "post" (recv INT))
            dual (offer $ Map.singleton "get" (recv BOOL)) `shouldBe`
                (choose $ Map.singleton "get" (send BOOL))

        it "is its own inverse" $
            forAll validSession $ \a ->
                dual (dual a) === a

        it "is bijective" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                (dual a == dual b) === (a == b)

        it "constructs duals when concatenated" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                dual (a `join` b) === dual a `join` dual b

        it "constructs the dual of a SessionType in Choose" $
            forAll validSession $ \a ->
                \c s -> dual (choose $ Map.singleton (c:s) a) ===
                    offer (Map.singleton (c:s) $ dual a)

        it "constructs the dual of a SessionType in Option" $
            forAll validSession $ \a ->
                \c s -> dual (offer $ Map.singleton (c:s) a) ===
                    choose (Map.singleton (c:s) $ dual a)

        it "constructs the dual of a SessionType in a fixpoint" $
            forAll validSession $ \a ->
                dual (mu a) === mu (dual a)

        it "inverts subtypes" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                a <: b === dual b <: dual a

    describe "SessionType.isCompatible" $ do
        it "checks if two session types can communicate" $ do
            end `shouldSatisfy` (<=> end)
            send BOOL `shouldSatisfy` (<=> recv BOOL)
            recv INT `shouldSatisfy` (<=> send INT)

            (choose $ Map.singleton "a" end) `shouldSatisfy`
                (<=> offer (Map.fromList [("a",end),("b",end)]))

            (offer $ Map.singleton "a" end) `shouldNotSatisfy`
                (<=> choose (Map.fromList [("a",end),("b",end)]))

        it "is symmetric" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                a <=> b === b <=> a

        it "is always satisfied by duals" $
            forAll validSession $ \a ->
                a <=> dual a .&&. dual a <=> a

        it "determines compatibility after concatenation" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
            forAll validSession $ \c ->
                ((a <=> b) === (a `join` c <=> b `join` c)) .&&.
                ((a <=> b) === (c `join` a <=> c `join` b))

        it "determines compatibility inside a Choose/Offer" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                \c s -> a <=> b ===
                    offer (Map.singleton (c:s) a) <=>
                    choose (Map.singleton (c:s) b)

        it "determines compatibility inside a Fixpoint" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
                a <=> b === mu a <=> mu b

        it "matches End to End only" $
            forAll validSession $ \a ->
                a <=> end === (a == end)

        it "matches subtypes when supertypes match" $
            forAll validSession $ \a ->
            forAll validSession $ \b ->
            forAll validSession $ \c ->
                (a <: b && b <=> c) --> a <=> c

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
