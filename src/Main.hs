{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex.Dom
import qualified Data.Text as T
import Data.Functor
import Text.Read
import Control.Monad
import qualified ParseMath as PM
import Data.Maybe
import Data.Map.Strict(Map, fromList)

main :: IO ()
main = mainWidget $ mdo
        
      styleSheet "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"

      let eClick0 = buttonAttr "0" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick1 = buttonAttr "1" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick2 = buttonAttr "2" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick3 = buttonAttr "3" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick4 = buttonAttr "4" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick5 = buttonAttr "5" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick6 = buttonAttr "6" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick7 = buttonAttr "7" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick8 = buttonAttr "8" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          eClick9 = buttonAttr "9" (fromList[("type", "button"), ("class", "btn btn-outline-dark")])
          ePlus = buttonAttr "+" (fromList[("type", "button"), ("class", "btn btn-info")])
          eMinus = buttonAttr "-" (fromList[("type", "button"), ("class", "btn btn-info")])
          eProd = buttonAttr "*" (fromList[("type", "button"), ("class", "btn btn-info")])
          eDiv = buttonAttr "/" (fromList[("type", "button"), ("class", "btn btn-info")])
          eSend = buttonAttr "=" (fromList[("type", "button"), ("class", "btn btn-outline-danger")])
          eC = buttonAttr "C" (fromList[("type", "button"), ("class", "btn btn-outline-danger"), 
            ("style", "width: 34.988636px; font-size: .9rem; height: 36.988636px")]) 


      mainDiv $ mdo
        -- Dynamic String
        
        el "div" $ mdo
          eClick7' <- eClick7
          eClick8' <- eClick8
          eClick9' <- eClick9
          

          divClass "btg-group" $ mdo
            eClick4' <- eClick4 
            eClick5' <- eClick5
            eClick6' <- eClick6

            

            divClass "btg-group" $ mdo
              eClick1' <- eClick1
              eClick2' <- eClick2
              eClick3' <- eClick3
              
              divClass "btg-group" $ mdo
                eC' <- eC
                eClick0' <- eClick0
                eSend' <- eSend
                

                a <- btnVertical $ mdo
                  ePlus' <- ePlus
                  eMinus' <- eMinus
                  eProd' <- eProd
                  eDiv' <- eDiv

                  let eClick = leftmost [eClick0' $> "0", eClick1' $> "1", eClick2' $> "2", eClick3' $> "3", eClick4' $> "4",
                         eClick5' $> "5", eClick6' $> "6", eClick7' $> "7", eClick8' $> "8", eClick9' $> "9",
                         ePlus' $> "+", eMinus' $> "-", eProd' $> "*", eDiv' $> "/"]
                      -- Integral a => Event Maybe a 
                      eResult = PM.parse <$> (current dynDisplay) <@ eSend'
                      -- Event Maybe ()
                      eClear = gate (current dynClear) eClick
                      -- Dynamic Bool: True if any click must clear the result display
                      eC'' = gate (fmap (\x -> not x) $ current dynClear) eC'

                  dynClear <- holdDyn False $ leftmost [eResult $> True, eClear $> False]
                  dynDisplay <- foldDyn ($) mempty $ leftmost [showResult <$> eResult, mergeWith (.) [
                          eClick0' $> (++ "0"), eClick1' $> (++ "1"), eClick2' $> (++ "2"), eClick3' $> (++ "3"),
                          eClick4' $> (++ "4"), eClick5' $> (++ "5"), eClick6' $> (++ "6"), eClick7' $> (++ "7"), 
                          eClick8' $> (++ "8"), eClick9' $> (++ "9"), ePlus' $> (++ "+"), eMinus' $> (++ "-"),
                          eProd' $> (++ "*"), eDiv' $> (++ "/"), eC'' $> (\x -> take (length x-1) x), eClear $> (const (mempty :: String))]]
                  return dynDisplay
                el "h3" $ dynText $ (\x -> T.pack x) <$> a  
                  
                blank

              blank
            blank
          blank

      blank
      

    where styleSheet link = elAttr "link" (fromList [("rel", "stylesheet"), ("type", "text/css"), ("href", link)]) $ return ()
          btnVertical a = elAttr "div" (fromList[("class", "btn-group-vertical"), ("style", "position: absolute; top: 280px")]) $ a
          mainDiv a = elAttr "div" (fromList[("class", "container"), ("style", "margin: auto; width: 140px; padding: 280px 0")]) $ a
          
-- | Return an empty string if the result is Nothing, else the Int
showResult :: (Integral a, Show a) => Maybe a -> String -> String
showResult Nothing = const mempty
showResult a       = (const . show . fromJust) a

buttonAttr :: MonadWidget t m => T.Text -> Map T.Text T.Text -> m (Event t ())
buttonAttr t attrs = do
    (btn, _) <- elAttr' "button" attrs $ text t
    pure $ domEvent Click btn