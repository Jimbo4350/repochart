
import           System.IO  (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified Test.Props

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [ Test.Props.tests ]
  pure ()
