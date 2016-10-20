module AccTransformScript where

import           Prelude hiding (repeat)

import           HERMIT.API
import           HERMIT.API.Types

fullBetaReduce :: Rewrite LCore
fullBetaReduce = betaReduce >>> letSubst

script :: Shell ()
script = do
  apply flattenModule
  eval "set-pp-type Omit"

  setPath $ rhsOf "main"
  apply . oneTD $ unfoldRuleUnsafe "Acc-start"

  -- Inline function body
  scope $ do
    setPath $ applicationOf "inline"
    sendCrumb appFun
    sendCrumb appArg
    apply inline
  apply . oneTD $ unfoldRuleUnsafe "inline-elim"

  mapM_ (apply . repeat . oneTD . unfoldRuleUnsafe)
        [ "==*-intro"
        , "+-intro"
        , "*-intro"
        , "quot-intro"
        , "even-intro"
        ]


  -- apply $ repeat (extractR $ focus (applicationOf "abs")
  --                                  (promote (caseFloatArgLemma "abs-lemma" <+ letFloat)))

  -- Get rid of dictionary lets
  apply . repeat $ oneTD fullBetaReduce
  apply . repeat $ oneTD letSubst

  apply . try . repeat $ oneTD letFloatApp

  -- Bring 'abs' into let body
  apply $ oneTD letFloatArg

  -- Recursion transformation
  apply $ oneTD fixIntro

  scope $ do
    setPath $ applicationOf "fix"
    kernelEffect up
    kernelEffect up
    kernelEffect up
    apply letSubst

  apply . oneTD $ unfoldRuleUnsafe "fix-abs-rep-intro" 
  apply . repeat $ oneTD fullBetaReduce

