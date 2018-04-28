import SessionType
import qualified Test.Hspec as Hspec
import Test.Hspec (hspec, it, describe, shouldSatisfy, shouldNotSatisfy,
    shouldBe)
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, arbitrary, property)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

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
