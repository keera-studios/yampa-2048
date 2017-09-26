module Graphics.Gloss.Interface.FRP.YampaDriver (driveYampa, InputEvent) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Graphics.Gloss (Display, Color, Picture, blank)
import Graphics.Gloss.Interface.IO.Game (playIO)
import FRP.Yampa (Event(..), SF, reactimate, DTime)

import Control.Concurrent

import Types (InputEvent)

-- | Play the game in a window, updating when the value of the provided 
driveYampa :: Display -- ^ The display method
          -> Color   -- ^ The background color
          -> Int     -- ^ The refresh rate, in Hertz
          -> SF (Event InputEvent) Picture
          -> IO ()
driveYampa display color frequency mainSF = do
    -- NOTE: we have to use an IORef instead of a MVar because Gloss is rendering the picture more often than after
    -- each input. We would get deadlocks because ending up calling takeMVar twice in rendering-function of Gloss
    -- because of the way Gloss works - it does not guarantee a specific ordering of calls
    renderOutputRef <- newIORef blank
    userInputVar <- newEmptyMVar

    _ <- forkIO (yampaThread userInputVar renderOutputRef mainSF)

    playIO display
           color
           frequency
           0 -- the time is the world
           (const $ readIORef renderOutputRef) -- An action to convert the world to a picture
           (\e t -> putMVar userInputVar (delta, Just (Event e)) >> return (t + delta)) -- A function to handle input events
           (\d t -> let delta' = realToFrac d - t
                   in if delta' > 0
                        then return 0.0 -- putMVar userInputVar (delta, Just NoEvent) >> 
                        else return (-delta')) -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced
  where
    delta = 0.01 / fromIntegral frequency

yampaThread :: MVar (DTime, Maybe (Event InputEvent))
              -> IORef Picture
              -> SF (Event InputEvent) Picture 
              -> IO ()
yampaThread userInputVar renderOutputRef sf = do
    reactimate
      (return NoEvent)
      inputSense
      outputRender
      sf

  where
    inputSense :: Bool -> IO (DTime, Maybe (Event InputEvent))
    inputSense _ = takeMVar userInputVar

    outputRender :: Bool -> Picture -> IO Bool
    outputRender updated pic = when updated (writeIORef renderOutputRef pic) >> return False