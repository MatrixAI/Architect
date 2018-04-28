import SessionType
import qualified Test.Hspec as Hspec
import Test.Hspec (hspec, it, describe, shouldSatisfy, shouldNotSatisfy,
    shouldBe)
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, arbitrary, property)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing)

instance Arbitrary Type where
    arbitrary = QC.elements [INT, STRING, BOOL]

sizedMap :: Int -> QC.Gen (Map String SessionType)
sizedMap n = do
    branches <- QC.choose (1,n)
    list <- QC.vectorOf branches $ do
        key <- arbitrary
        value <- sizedSession $ (n-1) `quot` branches
        return (key, value)
    return $ Map.fromList list

sizedSession :: Int -> QC.Gen SessionType
sizedSession 0 = QC.elements [Wait, Kill]
sizedSession n = QC.oneof
    [ return Wait
    , return Kill
    , Send <$> arbitrary <*> sizedSession (n-1)
    , Recv <$> arbitrary <*> sizedSession (n-1)
    , Choose <$> sizedMap n
    , Offer <$> sizedMap n
    ]

instance Arbitrary SessionType where
    arbitrary = QC.sized sizedSession

infix 4 -->
(-->) :: Bool -> Bool -> Bool
(-->) = (<=)

main :: IO ()
main = hspec $ do
    describe "SessionType.isValid" $ do
        it "checks if a session type is valid" $ do
            Wait `shouldSatisfy` isValid
            Kill `shouldSatisfy` isValid
            Send INT Wait `shouldSatisfy` isValid
            Recv STRING Kill `shouldSatisfy` isValid
            (Choose $ Map.singleton "a" Wait) `shouldSatisfy` isValid
            (Offer $ Map.singleton "get" Kill) `shouldSatisfy` isValid

            Choose Map.empty `shouldNotSatisfy` isValid
            (Offer $ Map.fromList [("", Kill)]) `shouldNotSatisfy` isValid

        it "checks a SessionType in Send is valid" $ property $
            \a t -> isValid (Send t a) == isValid a

        it "checks a SessionType in Recv is valid" $ property $
            \a t -> isValid (Recv t a) == isValid a

        it "checks a SessionType in Choose is valid" $ property $
            \a c s -> isValid (Choose $ Map.singleton (c:s) a) == isValid a

        it "checks a SessionType in Offer is valid" $ property $
            \a c s -> isValid (Offer $ Map.singleton (c:s) a) == isValid a

    describe "SessionType.isSubType" $ do
        it "checks if one SessionType is a subtype of another" $ do
            Wait `shouldSatisfy` (<: Wait)
            Kill `shouldSatisfy` (<: Kill)
            Send STRING Wait `shouldSatisfy` (<: Send STRING Wait)
            Recv INT Kill `shouldSatisfy` (<: Recv INT Kill)
            (Choose $ Map.fromList [("a",Kill),("b",Wait)]) `shouldSatisfy`
                (<: Choose (Map.singleton "b" Wait))
            (Offer $ Map.singleton "post" Kill) `shouldSatisfy`
                (<: Offer (Map.fromList [("get",Wait),("post",Kill)]))

            (Choose $ Map.singleton "a" Kill) `shouldNotSatisfy`
                (<: Choose (Map.fromList [("a",Kill),("b",Wait)]))
            (Offer $ Map.fromList [("get",Wait),("post",Kill)])
                `shouldNotSatisfy` (<: Offer (Map.singleton "get" Wait))

        it "is reflexive" $ property $
            \a -> a <: a

        it "is not symmetric" $ property $
            \a b -> (a == b) == (a <: b && b <: a)

        it "is transitive" $ property $
            \a b c -> (a <: b && b <: c) --> a <: c

        it "matches Wait to Wait only" $ property $
            \a -> (a <: Wait || Wait <: a) == (a == Wait)

        it "matches Kill to Kill only" $ property $
            \a -> (a <: Kill || Kill <: a) == (a == Kill)

        it "determines subtype inside a Send" $ property $
            \a b t -> (a <: b) == (Send t a <: Send t b)

        it "determines subtype inside a Recv" $ property $
            \a b t -> (a <: b) == (Recv t a <: Recv t b)

        it "determines subtype inside a Choose" $ property $
            \a b s -> (a <: b) ==
                (Choose $ Map.singleton s a) <: (Choose $ Map.singleton s b)

        it "determines subtype inside an Offer" $ property $
            \a b s -> (a <: b) ==
                (Offer $ Map.singleton s a) <: (Offer $ Map.singleton s b)

    describe "SessionType.dual" $ do
        it "constructs the dual of a SessionType" $ do
            dual Wait `shouldBe` Kill
            dual Kill `shouldBe` Wait
            dual (Send STRING Kill) `shouldBe` Recv STRING Wait
            dual (Recv BOOL Wait) `shouldBe` Send BOOL Kill
            dual (Choose $ Map.fromList [("post", Kill)]) `shouldBe`
                (Offer $ Map.fromList [("post", Wait)])
            dual (Offer $ Map.fromList [("get", Wait)]) `shouldBe`
                (Choose $ Map.fromList [("get", Kill)])

        it "is its own inverse" $ property $
            \a -> dual (dual a) == a

        it "is bijective" $ property $
            \a b -> (dual a == dual b) == (a == b)

        it "constructs the dual of a SessionType in Send" $ property $
            \a t -> dual (Send t a) == Recv t (dual a)

        it "constructs the dual of a SessionType in Recv" $ property $
            \a t -> dual (Recv t a) == Send t (dual a)

        it "constructs the dual of a SessionType in Choose" $ property $
            \a s -> dual (Choose $ Map.singleton s a) ==
                Offer (Map.singleton s $ dual a)

        it "constructs the dual of a SessionType in Option" $ property $
            \a s -> dual (Offer $ Map.singleton s a) ==
                Choose (Map.singleton s $ dual a)

    describe "SessionType.isCompatible" $ do
        it "checks if two session types can communicate" $ do
            Wait `shouldSatisfy` (<=> Kill)
            Kill `shouldSatisfy` (<=> Wait)
            Send BOOL Wait `shouldSatisfy` (<=> Recv BOOL Kill)
            Recv INT Wait `shouldSatisfy` (<=> Send INT Kill)
            (Choose $ Map.singleton "a" Wait) `shouldSatisfy`
                (<=> Offer (Map.fromList [("a",Kill),("b",Wait)]))

            (Offer $ Map.singleton "a" Wait) `shouldNotSatisfy`
                (<=> Choose (Map.fromList [("a",Kill),("b",Wait)]))

        it "is symmetric" $ property $
            \a b -> a <=> b == b <=> a

        it "is always satisfied by duals" $ property $
            \a -> a <=> dual a && dual a <=> a

        it "determines compatibility inside a Send/Recv" $ property $
            \a b t -> a <=> b == Send t a <=> Recv t b

        it "determines compatibility inside a Choose/Offer" $ property $
            \a b s -> a <=> b ==
                Offer (Map.singleton s a) <=> Choose (Map.singleton s b)

        it "matches Wait to Kill only" $ property $
            \a -> a <=> Wait == (a == Kill)

        it "matches Kill to Wait only" $ property $
            \a -> a <=> Kill == (a == Wait)

        it "matches subtypes when supertypes match" $ property $
            \a b c -> a <: c --> (a <=> b == b <=> c)

    describe "SessionType.strictUnion" $ do
        it "combines two protocols if possible and unambiguous" $ do
            strictUnion (Choose $ Map.singleton "a" Wait)
                (Choose $ Map.singleton "b" Kill) `shouldBe`
                (Just $ Choose (Map.fromList [("a",Wait),("b",Kill)]))
            strictUnion (Offer $ Map.singleton "get" Kill)
                (Offer $ Map.singleton "post" Wait) `shouldBe`
                (Just $ Offer (Map.fromList [("get",Kill),("post",Wait)]))

            strictUnion (Choose $ Map.singleton "a" Wait)
                (Choose $ Map.singleton "a" Wait) `shouldBe` Nothing
            strictUnion (Offer $ Map.singleton "get" Kill)
                (Offer $ Map.singleton "get" Wait) `shouldBe` Nothing

        it "never combines Wait" $ property $
            \a -> isNothing $ strictUnion Wait a

        it "never combines Kill" $ property $
            \a -> isNothing $ strictUnion Kill a

        it "never combines Send" $ property $
            \a b t -> isNothing $ strictUnion (Send t a) b

        it "never combines Recv" $ property $
            \a b t -> isNothing $ strictUnion (Recv t a) b

        it "never combines valid SessionTypes with themselves" $ property $
            \a -> isValid a --> isNothing (strictUnion a a)

        it "never combines clashing names in Choose" $ property $
            \a b s -> isNothing $ strictUnion (Choose $ Map.singleton s a)
                (Choose $ Map.singleton s b)

        it "never combines clashing names in Offer" $ property $
            \a b s -> isNothing $ strictUnion (Offer $ Map.singleton s a)
                (Offer $ Map.singleton s b)

        it "never combines Choose and Offer" $ property $
            \a s t -> isNothing $ strictUnion (Choose $ Map.singleton s a)
                (Offer $ Map.singleton t a)

        it "combines non-clashing names in Choose" $ property $
            \a b s t -> (s /= t) --> isJust (strictUnion
                (Choose $ Map.singleton s a) (Choose $ Map.singleton t b))

        it "combines non-clashing names in Offer" $ property $
            \a b s t -> (s /= t) --> isJust (strictUnion
                (Offer $ Map.singleton s a) (Offer $ Map.singleton t b))

        it "creates subtypes in Choose" $ property $
            \a b s t -> (s /= t) --> (((<: Choose (Map.singleton s a)) <$>
                strictUnion (Choose $ Map.singleton s a)
                (Choose $ Map.singleton t b)) == Just True)

        it "creates supertypes in Offer" $ property $
            \a b s t -> (s /= t) --> (((Offer (Map.singleton s a) <:) <$>
                strictUnion (Offer $ Map.singleton s a)
                (Offer $ Map.singleton t b)) == Just True)

        it "is symmetric" $ property $
            \a b -> strictUnion a b == strictUnion b a

        it "is associative" $ property $
            \a b c -> (isJust (strictUnion a b) && isJust (strictUnion b c))
                --> ((strictUnion a <$> strictUnion b c) ==
                    (($ c) <$> (strictUnion <$> strictUnion a b)))


    describe "SessionType.union" $ do
        it "combines two SessionTypes if possible" $ do
            union (Choose $ Map.singleton "a" Kill)
                (Choose $ Map.singleton "b" Wait) `shouldBe`
                Just (Choose $ Map.fromList [("a",Kill),("b",Wait)])
            union (Offer $ Map.singleton "a" Kill)
                (Offer $ Map.fromList [("a",Kill),("b",Wait)]) `shouldBe`
                Just (Offer $ Map.fromList [("a",Kill),("b",Wait)])

            union (Choose $ Map.singleton "a" Kill)
                (Choose $ Map.singleton "a" Wait) `shouldBe` Nothing

        it "works for everything strictUnion works for" $ property $
            \a b -> isJust (strictUnion a b) --> (union a b == strictUnion a b)

        it "combines Wait with Wait only" $ property $
            \a -> isJust (union a Wait) == (a == Wait)

        it "combines Kill with Kill only" $ property $
            \a -> isJust (union a Kill) == (a == Kill)

        it "combines the contents of a Send" $ property $
            \a b t -> (Send t <$> union a b) == union (Send t a) (Send t b)

        it "combines the contents of a Recv" $ property $
            \a b t -> (Recv t <$> union a b) == union (Recv t a) (Recv t b)

        it "combines the contents of a Choose" $ property $
            \a b s -> (Choose . (Map.singleton s) <$> union a b) == union
                (Choose $ Map.singleton s a) (Choose $ Map.singleton s b)

        it "combines the contents of an Offer" $ property $
            \a b s -> (Offer . (Map.singleton s) <$> union a b) == union
                (Offer $ Map.singleton s a) (Offer $ Map.singleton s b)

        it "combines anything with itself" $ property $
            \a -> union a a == Just a

        it "is symmetric" $ property $
            \a b -> union a b == union b a

        it "is associative" $ property $
            \a b c -> (isJust (union a b) && isJust (union b c)) -->
                ((union a <$> union b c) == (($ c) <$> (union <$> union a b)))
