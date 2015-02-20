import Window (initGL, UI(..))

loop :: IO UI -> IO ()
loop = undefined

main :: IO ()
main = usage >> initGL "2D Platformer" 640 480 >>= loop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, ESC to exit."
