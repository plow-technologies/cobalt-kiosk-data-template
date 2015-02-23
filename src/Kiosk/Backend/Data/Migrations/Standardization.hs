


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
module Kiosk.Backend.Data.Migrations.Standardization () where


import Data.Monoid 
import Data.Foldable
import Control.Applicative       
import Data.Attoparsec.Text  
import Data.Text   
import Text.Regex.TDFA ((=~))
import Text.Parser.Token
       


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



data StandardizationTransformer o i = 
-- standardize :: StandardizationTransformer o i ->
--              i -> Maybe o

-- standardize = undefined
-- -- | where standardize picks a standard for you
-- -- suggest standard returns a container with whatever standard you want.   

-- suggestStandards :: StandardizationTransformer o i ->
--                                o -> Maybe b
-- suggestStandards = undefined
