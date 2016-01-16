{-
Copyright 2014 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE PatternSynonyms, ViewPatterns, RankNTypes, LambdaCase, TupleSections #-}

import Language.C.Clang
import Data.Foldable
import Data.Function
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Tree
import Data.Tree
import qualified Data.Set as S

main = do
  idx <- createIndex
  tu <- parseTranslationUnit idx "test.c"
          [ "-I/usr/lib/llvm-3.7/lib/clang/3.7.0/include"
          , "-I/usr/include/c++/4.9"
          , "-I/usr/include/x86_64-linux-gnu/c++/4.9"
          , "-std=c++14"
          , "-stdlib=libstdc++"
          ]
  let
    root = translationUnitCursor tu
    rootF f = root ^.. cosmosOf cursorChildrenF . f
    decs t = rootF
        ( filtered (\c -> cursorKind c == t)
        . folding (\c -> ( cursorSpelling c, ,,,)
                   <$> (typeSpelling <$> cursorType c) 
                   <*> (spellingLocation . rangeStart <$> cursorExtent c)
                   <*> (spellingLocation . rangeEnd   <$> cursorExtent c)
                   <*> (cursorSpelling <$> cursorReferenced c)
                  )
        )
    descS k j = S.fromList $ map  (\(f, t,s,e,r) -> BS.unpack f ++ j ++ BS.unpack t ++ "@" ++ show (fileName . file $ s) ++ "\n > "++ show r ) (decs k)
    printS k j = for_ (descS k j)  putStrLn
  print (root  ^.. cosmosOf cursorChildrenF)


  printS FunctionDecl " :>: "
  printS TypedefDecl  " :: "
  printS VarDecl " :=: "
  printS ClassDecl " :~: "
  printS CXXMethod " :+: "

  print ( root)

