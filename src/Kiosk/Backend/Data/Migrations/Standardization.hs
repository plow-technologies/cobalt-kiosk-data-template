

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}  
{- |
Module      :  Kiosk.Backend.Data.Migrations.Standardization
Description :  Many to one standardizer for creating nice code.
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

There are a lot of places in loose data migrations where you have text that looks like 
      
"Company A" or "CompanyA" or "A Company", but you want all of them to mean, "Company A"

standardizers take a list of stuff like this and return a given result. 
            
* inputs are tokenized (white space deleted)
* then turned into parsers and applied sequentially.

* you build a library of these parsers to apply to an input


"Company A" , "Company    A" are the same set of matching rules




-}
module Kiosk.Backend.Data.Migrations.Standardization ( createStandardizer
                                                     , makeConstantStanderdizer
                                                     , makeStandardForm
                                                     , makeRegexForm
                                                     , RegexForm
                                                     , StandardForm
                                                     , Standardizer
                                                     , emptyStandardizer
                                                     , standardize
                                                     , union
                                                     , makeStandardizer) where


import Data.Monoid (Monoid)                          
import Data.Text   (Text)

import Data.String (IsString)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Attoparsec.Text (Parser)

import Control.Applicative  ((*>)
                            ,pure
                            )   
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import Text.Parser.Token   (textSymbol)             
import Data.Hashable (Hashable)
-- | Standardization transformer properties
-- 'StandardizationTransformer' compose

{-|
        
str1::String
str2::String
nt1 :: StandardizationTransformer
nt2 :: StandardizationTransformer
    
where
>>> standardize nt1 str1
    Just "Something"

>>> standardize nt1 str2
    Nothing

>>> standardize nt2 str2
Just "Something Else"

>>> standardize nt2 str1
Nothing
        
>>> standardize (nt1 <|> nt2) str1
Just "Something"

>>> standardize (nt1 <|> nt2) str2
Just "Something Else"
|-}

newtype RegexForm = RegexForm {
                                _getRegexForm :: Text} 
  deriving (Show,Ord,Eq,IsString, Monoid,Read,Hashable)


newtype StandardForm = StandardForm {
                                       _getStandardForm :: Text } 
  deriving (Show,Ord,Eq,IsString, Monoid,Read)

newtype Standardizer  = Standardizer {_getStandardizer ::(HashMap RegexForm StandardForm)}
  deriving (Show,Eq,Monoid)

-- | make a standardizer such that if ta == ts, from (RegexForm ta) and (StandardForm ts)
--   then then makeConstantStandardizer will return ts when given
makeConstantStanderdizer :: StandardForm -> Standardizer
makeConstantStanderdizer sf@(StandardForm s) =  Standardizer . HM.insert (RegexForm s) sf $ HM.empty      

-- |Standardizers allways are created iwth the Constant Standardizer
createStandardizer :: RegexForm ->
                      StandardForm ->
                      Standardizer                      
createStandardizer altForm standardForm = Standardizer . HM.insert altForm standardForm . 
                                          _getStandardizer $ standardFormConstantMap
  where
     standardFormConstantMap :: Standardizer
     standardFormConstantMap = makeConstantStanderdizer standardForm                                                  


-- | I am a big jerk... so no Pattern matching on the external here... once they are made
-- That is it!
makeStandardForm :: Text -> StandardForm
makeStandardForm = StandardForm

makeRegexForm :: Text -> RegexForm      
makeRegexForm = RegexForm

-- | If left and right standardizers match, a is used
union :: Standardizer ->
         Standardizer -> Standardizer
union (Standardizer a) (Standardizer b) = Standardizer . HM.union a $ b

-- | Standardize a Text against a Standardizer
-- find the first match in a Text and replace with the StandardForm
-- only the first match is found   

emptyStandardizer :: Standardizer
emptyStandardizer = Standardizer HM.empty                     


makeStandardizer :: RegexForm ->
                              StandardForm 
                             -> Parser Text                                  
makeStandardizer (RegexForm af) (StandardForm sf) = textSymbol af *> 
                                                            pure sf    



standardize :: Standardizer -> Text -> Text
standardize (Standardizer s) txt = maybe txt id $ HM.foldrWithKey generateAndRunParser Nothing s  
  where                                                     
    generateAndRunParser _ _ jt@(Just _) = jt
    generateAndRunParser af sf Nothing = symbolParser af sf 
    symbolParser :: RegexForm -> StandardForm -> Maybe Text
    symbolParser (RegexForm af) (StandardForm sf) = if (txt =~ af)
                                                    then Just sf
                                                    else Nothing

