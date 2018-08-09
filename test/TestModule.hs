
module TestModule where

data FirstType = PlainConstructor | ConstuctorWithType String

data OtherCase = OtherCase Int | OtherCase2 String

data SecondType = SecondConstructor | AnotherSecondConstructor

newtype TestNewType = TestNewType Int

testfunction :: String
testfunction = "TEST"

data BlockchainInspectorContext = BlockchainInspectorContext { bicNodeDBs :: String
                                                             , secondAcc :: Int}
