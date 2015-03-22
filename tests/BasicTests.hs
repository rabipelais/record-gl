{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Word
import           Foreign.Ptr                    (nullPtr, plusPtr)
import           Graphics.RecordGL
import           Graphics.Rendering.OpenGL      (DataType (..), GLfloat,
                                                 VertexArrayDescriptor (..))
import           Linear                         (V1 (..), V3 (..))
import           Record
import           Record.Types
import           Test.Framework                 (defaultMain)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit                     (Test (..), (~=?))

type Vertex = [r| {vpos :: V3 GLfloat, tagByte :: V1 Word8} |]

testVad :: Test
testVad = TestLabel "Sample VAD Creation" $
          vad ~=? fieldToVAD (undefined::FieldName "vpos") (undefined::Vertex)
  where vad = VertexArrayDescriptor 3 Float 13 (nullPtr `plusPtr` 1)

main :: IO ()
main = defaultMain . hUnitTestToTests $ testVad
