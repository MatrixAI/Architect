import SessionType
import Test.Hspec (hspec, it, describe, shouldSatisfy, shouldBe)
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
    , Send <$> arbitrary <*> (sizedSession $ n-1)
    , Recv <$> arbitrary <*> (sizedSession $ n-1)
    , Choose <$> sizedMap n
    , Offer <$> sizedMap n
    ]

instance Arbitrary SessionType where
    arbitrary = QC.sized sizedSession

main :: IO ()
main = hspec $ do
    describe "SessionType.isValid" $ do
        it "checks if a session type is valid" $ do
            Wait `shouldSatisfy` isValid
            Kill `shouldSatisfy` isValid
            (Send INT Wait) `shouldSatisfy` isValid
            (Recv STRING Kill) `shouldSatisfy` isValid
            (Choose $ Map.singleton "a" Wait) `shouldSatisfy` isValid
            (Offer $ Map.singleton "get" Kill) `shouldSatisfy` isValid

            (Choose Map.empty) `shouldSatisfy` (not . isValid)
            (Offer $ Map.fromList [("", Kill)]) `shouldSatisfy` (not . isValid)

        it "checks a SessionType in Send is valid" $
            property $ \a t -> isValid (Send t a) == isValid a

        it "checks a SessionType in Recv is valid" $
            property $ \a t -> isValid (Recv t a) == isValid a

        it "checks a SessionType in Choose is valid" $
            property $ \a c s -> isValid (Choose $ Map.singleton (c:s) a) ==
                isValid a

        it "checks a SessionType in Offer is valid" $
            property $ \a c s -> isValid (Offer $ Map.singleton (c:s) a) ==
                isValid a

    describe "SessionType.isSubType" $ do
        it "checks if one SessionType is a subtype of another" $ do
            isSubType (Choose $ Map.fromList [("a",Kill),("b",Wait)])
                (Choose $ Map.singleton "b" Wait) `shouldBe` True
            isSubType (Offer $ Map.singleton "post" Kill)
                (Offer $ Map.fromList [("get",Wait),("post",Kill)]) `shouldBe`
                True

            isSubType (Choose $ Map.singleton "a" Kill)
                (Choose $ Map.fromList [("a",Kill),("b",Wait)]) `shouldBe`
                False
            isSubType (Offer $ Map.fromList [("get",Wait),("post",Kill)])
                (Offer $ Map.singleton "get" Wait) `shouldBe` False

        it "is reflexive" $
            property $ \a -> a `isSubType` a

        it "is not symmetric" $
            property $ \a b -> (a == b) == (a `isSubType` b && b `isSubType` a)

        it "is transitive" $
            property $ \a b c ->
                (a `isSubType` b && b `isSubType` c) <= a `isSubType` c



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

        it "is its own inverse" $
            property $ \a -> dual (dual a) == a

        it "is bijective" $
            property $ \a b -> (dual a == dual b) == (a == b)

        it "constructs the dual of a SessionType in Send" $
            property $ \a t -> dual (Send t a) == Recv t (dual a)

        it "constructs the dual of a SessionType in Recv" $
            property $ \a t -> dual (Recv t a) == Send t (dual a)

        it "constructs the dual of a SessionType in Choose" $
            property $ \a s -> dual (Choose $ Map.singleton s a) ==
                Offer (Map.singleton s $ dual a)

        it "constructs the dual of a SessionType in Option" $
            property $ \a s -> dual (Offer $ Map.singleton s a) ==
                Choose (Map.singleton s $ dual a)
