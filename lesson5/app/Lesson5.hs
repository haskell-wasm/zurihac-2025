{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

module Lesson5
  ( main,
  )
where

import Codec.Picture
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign
import GHC.Wasm.Prim

newtype JSCanvasRenderingContext2D = JSCanvasRenderingContext2D JSVal

newtype JSCallback = JSCallback JSVal

-- Important: export it as sync callback, so previous frame's render
-- callback runs to completion before this frame's render callback is
-- entered.
foreign import javascript "wrapper sync"
  jsExportDrawFrame :: (Double -> IO ()) -> IO JSCallback

-- We only need one same context, no need to wrap in IO
foreign import javascript unsafe
  "document.getElementsByTagName('canvas')[0].getContext('2d')"
  jsCanvasRenderingContext2D :: JSCanvasRenderingContext2D

-- Given the pixel buffer, draw it to the <canvas> element
foreign import javascript unsafe
  "$1.putImageData(new ImageData(new Uint8ClampedArray(__exports.memory.buffer, $2, $3 * $4 * 4), $3, $4), 0, 0)"
  jsDrawFrame :: JSCanvasRenderingContext2D -> Ptr Pixel8 -> Int -> Int -> IO ()

-- Given a callback that takes a monotonic timestamp, repeatedly call
-- it at screen refresh rate.
foreign import javascript unsafe
  "const cb = (t) => { $1(t); requestAnimationFrame(cb); }; requestAnimationFrame(cb);"
  jsStartRenderLoop :: JSCallback -> IO ()

foreign export javascript "hsMain sync"
  main :: IO ()

main :: IO ()
main = do
  -- canvas size hard coded to 1080p for simplicity
  cb <- jsExportDrawFrame $ drawFrame jsCanvasRenderingContext2D 1920 1080
  jsStartRenderLoop cb

-- TODO: for simplicity (elegance?), we construct a fresh `Image`
-- for each frame which allocates a big fresh buffer. This means we'll
-- GC excessively. To avoid excessive GC, at least reuse a
-- `MutableImage` across frames.
drawFrame :: JSCanvasRenderingContext2D -> Int -> Int -> Double -> IO ()
drawFrame ctx w h t = V.unsafeWith imageData $ \ptr -> jsDrawFrame ctx ptr w h
  where
    Image {..} = mkImage w h t

mkImage :: Int -> Int -> Double -> Image PixelRGBA8
mkImage w h t =
  generateImage pixel w h
  where
    ----------------------------------------------------------------
    -- Per-pixel colour decision -----------------------------------
    ----------------------------------------------------------------
    pixel x y
      | sqDist p1 < r2 = c1
      | sqDist p2 < r2 = c2
      | sqDist p3 < r2 = c3
      | otherwise      = bg
      where
        -- squared Euclidean distance to a point (px,py)
        sqDist (# px, py #) = dx*dx + dy*dy
          where
            dx = fromIntegral x - px
            dy = fromIntegral y - py

    ----------------------------------------------------------------
    -- Scene geometry ----------------------------------------------
    ----------------------------------------------------------------
    (# cx, cy #) = (# fromIntegral w / 2, fromIntegral h / 2 #)
    s        = 0.40 * fromIntegral (min w h)
    r        = 0.04 * fromIntegral (min w h)
    r2       = r*r                     -- radius squared

    p1 = (# cx + s*0.90 * cos (k1*t)
         , cy + s*0.90 * sin (k1*t) #)

    p2 = (# cx + s*0.60 * cos (k2*t + phi)
         , cy + s*0.60 * sin (k2*t + phi) #)

    p3 = (# cx + s*0.75 * cos (k3*t - phi)
         , cy + s*0.75 * sin (k3*t - phi) #)

    k1 = 0.0006          -- angular speeds (rad / ms)
    k2 = 0.0008
    k3 = 0.0007
    phi = 2*pi/3         -- 120 ° phase offset

    ----------------------------------------------------------------
    -- Projector-friendly palette ----------------------------------
    ----------------------------------------------------------------
    bg = PixelRGBA8 250 250 250 255   -- very light grey
    c1 = PixelRGBA8 200  80  80 255   -- soft red
    c2 = PixelRGBA8  80 200  80 255   -- soft green
    c3 = PixelRGBA8  80  80 200 255   -- soft blue
