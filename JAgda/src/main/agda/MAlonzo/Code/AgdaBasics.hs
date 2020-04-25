{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, RankNTypes,
             PatternSynonyms #-}
module MAlonzo.Code.AgdaBasics where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text

name2 = "AgdaBasics.Bool"
d2 = ()
data T2 = C4 | C6
name8 = "AgdaBasics.not"
d8 :: T2 -> T2
d8 v0
  = case coe v0 of
      C4 -> coe C6
      C6 -> coe C4
      _ -> MAlonzo.RTE.mazUnreachableError
name10 = "AgdaBasics._or_"
d10 :: T2 -> T2 -> T2
d10 v0 v1
  = case coe v0 of
      C4 -> coe v0
      C6 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name14 = "AgdaBasics._and_"
d14 :: T2 -> T2 -> T2
d14 v0 v1
  = case coe v0 of
      C4 -> coe v1
      C6 -> coe v0
      _ -> MAlonzo.RTE.mazUnreachableError
name20 = "AgdaBasics.if_then_else_"
d20 :: () -> T2 -> AgdaAny -> AgdaAny -> AgdaAny
d20 v0 v1 v2 v3 = du20 v1 v2 v3
du20 :: T2 -> AgdaAny -> AgdaAny -> AgdaAny
du20 v0 v1 v2
  = case coe v0 of
      C4 -> coe v1
      C6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name30 = "AgdaBasics.Nat"
d30 = ()
data T30 = C32 | C34 T30
name36 = "AgdaBasics._+_"
d36 :: T30 -> T30 -> T30
d36 v0 v1
  = case coe v0 of
      C32 -> coe v1
      C34 v2 -> coe C34 (coe d36 (coe v2) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name44 = "AgdaBasics._*_"
d44 :: T30 -> T30 -> T30
d44 v0 v1
  = case coe v0 of
      C32 -> coe v0
      C34 v2 -> coe d36 (coe v1) (coe d44 (coe v2) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name52 = "AgdaBasics.List"
d52 a0 = ()
data T52 = C56 | C58 AgdaAny T52
name60 = "AgdaBasics.w0'"
d60 :: T52
d60 = coe C58 (coe C4) (coe C56)
name62 = "AgdaBasics.A:Set"
d62 :: T2
d62 = coe C4
name64 = "AgdaBasics.w1'"
d64 :: ()
d64 = erased
name66 = "AgdaBasics.w2'"
d66 :: T52
d66 = coe C58 (coe C32) (coe C56)
name68 = "AgdaBasics.w3'"
d68 :: T52
d68
  = coe
      C58 (coe C32)
      (coe
         C58 (coe C34 (coe C34 (coe C32)))
         (coe C58 (coe C34 (coe C32)) (coe C56)))
name74 = "AgdaBasics.identity"
d74 a0 a1 = ()
data T74 = C80
name82 = "AgdaBasics.w4'"
d82 :: T74
d82 = erased
name84 = "AgdaBasics.w5'"
d84 :: T74
d84 = erased
name90 = "AgdaBasics.identity₂"
d90 a0 a1 = ()
data T90 = C96
name98 = "AgdaBasics.w8'"
d98 :: T90
d98 = erased
name100 = "AgdaBasics.w9'"
d100 :: ()
d100 = erased
name108 = "AgdaBasics._≡_"
d108 a0 a1 a2 = ()
data T108 = C114
name116 = "AgdaBasics.w6'"
d116 :: T108
d116 = erased
name138 = "AgdaBasics._◦_"
d138 ::
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d138 v0 v1 v2 v3 v4 v5 = du138 v3 v4 v5
du138 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du138 v0 v1 v2 = coe v0 v2 (coe v1 v2)
name146 = "AgdaBasics.suc2'"
d146 :: T30 -> T30
d146 = coe du138 (coe (\ v0 -> coe C34)) (coe C34)
name148 = "AgdaBasics.or3'"
d148 :: T2 -> T2
d148 = coe du138 (coe (\ v0 -> d8)) (coe d8)
name154 = "AgdaBasics.map"
d154 :: () -> () -> (AgdaAny -> AgdaAny) -> T52 -> T52
d154 v0 v1 v2 v3 = du154 v2 v3
du154 :: (AgdaAny -> AgdaAny) -> T52 -> T52
du154 v0 v1
  = case coe v1 of
      C56 -> coe v1
      C58 v2 v3 -> coe C58 (coe v0 v2) (coe du154 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
name166 = "AgdaBasics._++_"
d166 :: () -> T52 -> T52 -> T52
d166 v0 v1 v2 = du166 v1 v2
du166 :: T52 -> T52 -> T52
du166 v0 v1
  = case coe v0 of
      C56 -> coe v1
      C58 v2 v3 -> coe C58 (coe v2) (coe du166 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name178 = "AgdaBasics.Vec"
d178 a0 a1 = ()
data T178 = C182 | C186 AgdaAny T178
name188 = "AgdaBasics.w10'"
d188 :: T178
d188 = coe C186 (coe C32) (coe C182)
name190 = "AgdaBasics.w11'"
d190 :: T178
d190 = coe C186 (coe C34 (coe C32)) (coe C186 (coe C32) (coe C182))
name196 = "AgdaBasics.head"
d196 :: () -> T30 -> T178 -> AgdaAny
d196 v0 v1 v2 = du196 v2
du196 :: T178 -> AgdaAny
du196 v0
  = case coe v0 of
      C186 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name202 = "AgdaBasics.w12'"
d202 :: T30
d202 = coe du196 (coe d190)
name204 = "AgdaBasics.w13'"
d204 :: T30
d204 = coe du196 (coe d188)
name212 = "AgdaBasics.vmap"
d212 :: () -> () -> T30 -> (AgdaAny -> AgdaAny) -> T178 -> T178
d212 v0 v1 v2 v3 v4 = du212 v2 v3 v4
du212 :: T30 -> (AgdaAny -> AgdaAny) -> T178 -> T178
du212 v0 v1 v2
  = case coe v2 of
      C182 -> coe v2
      C186 v4 v5
        -> case coe v0 of
             C34 v6
               -> coe C186 (coe v1 v4) (coe du212 (coe v6) (coe v1) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name228 = "AgdaBasics.Bild_≡_"
d228 a0 a1 a2 a3 = ()
newtype T228 = C238 AgdaAny
name240 = "AgdaBasics.bildwert"
d240 :: T228
d240 = coe C238 (coe C32)
name250 = "AgdaBasics.inv"
d250 ::
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> T228 -> AgdaAny
d250 v0 v1 v2 v3 v4 = du250 v4
du250 :: T228 -> AgdaAny
du250 v0
  = case coe v0 of
      C238 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name258 = "AgdaBasics.isuc"
d258 :: T30 -> T228 -> T30
d258 v0 v1 = coe du250 v1
name260 = "AgdaBasics.fbild'"
d260 :: T228 -> T30
d260 = coe d258 (coe C34 (coe C34 (coe C32)))
name262 = "AgdaBasics.w14'"
d262 :: T228
d262 = coe C238 (coe C34 (coe C32))
name264 = "AgdaBasics.test1"
d264 :: T108
d264 = erased
name266 = "AgdaBasics.Bin"
d266 = ()
data T266 = C268 | C270 T266 | C272 T266
name274 = "AgdaBasics.w15'"
d274 :: T266
d274 = coe C272 (coe C270 (coe C268))
name276 = "AgdaBasics.invert2"
d276 :: T266 -> T266 -> T266
d276 v0 v1
  = case coe v0 of
      C268 -> coe v1
      C270 v2 -> coe d276 (coe v2) (coe C270 (coe v1))
      C272 v2 -> coe d276 (coe v2) (coe C272 (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name288 = "AgdaBasics.invert"
d288 :: T266 -> T266
d288 v0 = coe d276 (coe v0) (coe C268)
name292 = "AgdaBasics.inc2"
d292 :: T266 -> T266 -> T2 -> T266
d292 v0 v1 v2
  = case coe v0 of
      C268
        -> case coe v2 of
             C4 -> coe C272 (coe v1)
             C6 -> coe v1
             _ -> MAlonzo.RTE.mazUnreachableError
      C270 v3
        -> case coe v2 of
             C4 -> coe d292 (coe v3) (coe C272 (coe v1)) (coe C6)
             C6 -> coe d292 (coe v3) (coe C270 (coe v1)) (coe v2)
             _ -> MAlonzo.RTE.mazUnreachableError
      C272 v3
        -> case coe v2 of
             C4 -> coe d292 (coe v3) (coe C270 (coe v1)) (coe v2)
             C6 -> coe d292 (coe v3) (coe C272 (coe v1)) (coe v2)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name314 = "AgdaBasics.inc"
d314 :: T266 -> T266
d314 v0
  = let v1 = d288 (coe d292 (coe v0) (coe C268) (coe C4)) in
    case coe v0 of
      C268 -> coe C272 (coe v0)
      _ -> coe v1
name318 = "AgdaBasics.eqBin"
d318 :: T266 -> T266 -> T2
d318 v0 v1
  = let v2 = coe C6 in
    case coe v0 of
      C268
        -> case coe v1 of
             C268 -> coe C4
             _ -> coe v2
      C270 v3
        -> case coe v1 of
             C270 v4 -> coe d318 (coe v3) (coe v4)
             _ -> coe v2
      C272 v3
        -> case coe v1 of
             C272 v4 -> coe d318 (coe v3) (coe v4)
             _ -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name328 = "AgdaBasics.IstFalsch"
d328 = ()
data T328
name330 = "AgdaBasics.IstWahr"
d330 = ()
data T330 = C332
name334 = "AgdaBasics.ist_wahr?"
d334 :: T2 -> ()
d334 = erased
name336 = "AgdaBasics.testinc"
d336 :: T266 -> T266 -> ()
d336 = erased
name342 = "AgdaBasics.tinc0'"
d342 :: T330
d342 = erased
name344 = "AgdaBasics.tinc1'"
d344 :: T330
d344 = erased
name346 = "AgdaBasics.tinc2'"
d346 :: T330
d346 = erased
name348 = "AgdaBasics.tinc4'"
d348 :: T330
d348 = erased
name350 = "AgdaBasics.tinc'"
d350 :: T330
d350 = erased
name352 = "AgdaBasics.bin2nat3"
d352 :: T266 -> T30 -> T30 -> T30
d352 v0 v1 v2
  = case coe v0 of
      C268 -> coe v2
      C270 v3 -> coe d352 (coe v3) (coe d36 (coe v1) (coe v1)) (coe v2)
      C272 v3
        -> coe
             d352 (coe v3) (coe d36 (coe v1) (coe v1))
             (coe d36 (coe v1) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
name368 = "AgdaBasics.bin2nat"
d368 :: T266 -> T30
d368 v0 = coe d352 (coe v0) (coe C34 (coe C32)) (coe C32)
name372 = "AgdaBasics.eqNat"
d372 :: T30 -> T30 -> T2
d372 v0 v1
  = let v2 = coe C6 in
    case coe v0 of
      C32
        -> case coe v1 of
             C32 -> coe C4
             _ -> coe v2
      C34 v3
        -> case coe v1 of
             C34 v4 -> coe d372 (coe v3) (coe v4)
             _ -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name378 = "AgdaBasics.testBin2Nat"
d378 :: T30 -> T266 -> ()
d378 = erased
name384 = "AgdaBasics.tbin2nat1"
d384 :: T330
d384 = erased
name386 = "AgdaBasics.tbin2nat2"
d386 :: T330
d386 = erased
name388 = "AgdaBasics.tbin2nat3"
d388 :: T330
d388 = erased
name390 = "AgdaBasics.tbin2nat4"
d390 :: T330
d390 = erased
name392 = "AgdaBasics.tbin2nat5"
d392 :: T330
d392 = erased
name394 = "AgdaBasics.nat2bin2"
d394 :: T30 -> T266 -> T266
d394 v0 v1
  = case coe v0 of
      C32 -> coe v1
      C34 v2 -> coe d394 (coe v2) (coe d314 (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name402 = "AgdaBasics.nat2bin"
d402 :: T30 -> T266
d402 v0 = coe d394 (coe v0) (coe C268)
name406 = "AgdaBasics.testNat2Bin"
d406 :: T266 -> T30 -> ()
d406 = erased
name412 = "AgdaBasics.tnat2bin1"
d412 :: T330
d412 = erased
name414 = "AgdaBasics.tnat2bin2"
d414 :: T330
d414 = erased
name416 = "AgdaBasics.tnat2bin3"
d416 :: T330
d416 = erased
name418 = "AgdaBasics.test+1"
d418 :: T108
d418 = erased
name420 = "AgdaBasics.test*1"
d420 :: T108
d420 = erased
name422 = "AgdaBasics._^_"
d422 :: T30 -> T30 -> T30
d422 v0 v1
  = case coe v1 of
      C32 -> coe C34 (coe v1)
      C34 v2
        -> let v3 = d44 (coe v0) (coe d422 (coe v0) (coe v2)) in
           case coe v0 of
             C32 -> coe v0
             _ -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name428 = "AgdaBasics.test^1"
d428 :: T108
d428 = erased
name430 = "AgdaBasics._∸_"
d430 :: T30 -> T30 -> T30
d430 v0 v1
  = case coe v0 of
      C32 -> coe v0
      C34 v2
        -> case coe v1 of
             C32 -> coe v0
             C34 v3 -> coe d430 (coe v2) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name438 = "AgdaBasics.test∸1"
d438 :: T108
d438 = erased
name440 = "AgdaBasics._<_"
d440 :: T30 -> T30 -> T2
d440 v0 v1 = coe d372 (coe d430 (coe v0) (coe v1)) (coe C32)
name446 = "AgdaBasics.test<1"
d446 :: T330
d446 = erased
