To reproduce

```
cabal build lib:repro2
```

Produces:

```
Build profile: -w ghc-9.4.4 -O1
In order, the following will be built (use -v for more details):
 - repro2-0.1.0.0 (lib) (file src/Repro.hs changed)
Preprocessing library for repro2-0.1.0.0..
Building library for repro2-0.1.0.0..
[1 of 1] Compiling Repro            ( src/Repro.hs, /home/matt/graphql-engine/repro2/dist-newstyle/build/x86_64-linux/ghc-9.4.4/repro2-0.1.0.0/opt/build/Repro.o, /home/matt/graphql-engine/repro2/dist-newstyle/build/x86_64-linux/ghc-9.4.4/repro2-0.1.0.0/opt/build/Repro.dyn_o ) [Source file changed]

src/Repro.hs:32:20: warning: [-Wunused-matches]
    Defined but not used: ‘be’
   |
32 |   toSQL (WhereFrag be) =
   |                    ^^

src/Repro.hs:47:20: warning: [-Wunused-matches]
    Defined but not used: ‘ce’
   |
47 |   toSQL (Extractor ce ) = undefined
   |                    ^^

src/Repro.hs:51:10: warning: [-Wmissing-methods]
    • No explicit implementation for
        ‘toSQL’
    • In the instance declaration for ‘ToSQL FromItem’
   |
51 | instance ToSQL FromItem
   |          ^^^^^^^^^^^^^^

src/Repro.hs:94:7: warning: [-Wunused-matches]
    Defined but not used: ‘kat’
   |
94 | (<+>) kat (x : xs) =
   |       ^^^
*** Core Lint errors : in result of Simplifier ***
src/Repro.hs:75:9: warning:
    Rule "SC:$j0": unbound [sg_s1Rr]
    In the RHS of $ctoSQL_s1Oh :: SQLUpdate -> TextBuilder
    In the body of lambda with binder a_a13J :: SQLUpdate
    In a case alternative: (TextBuilder ww_a1OX :: Action,
                                        ww1_a1OY :: Int#,
                                        ww2_a1OZ :: Int#)
    In a case alternative: (TextBuilder ww_X2 :: Action,
                                        ww1_X3 :: Int#,
                                        ww2_X4 :: Int#)
    In a case alternative: (SQLUpdate ds_d1Dk :: SetExp,
                                      ds_d1Dl :: Maybe FromExp,
                                      ds_d1Dm :: Maybe WhereFrag,
                                      ds_d1Dn :: Maybe RetExp)
    In the body of lambda with binder sc_s1Rd :: Int#
    In the body of lambda with binder sc_s1Rc :: Int#
    In the body of lambda with binder sg_s1Rb :: (forall {s}.
                                                  MArray s -> Int -> State# s -> (# State# s, () #))
                                                 ~R# Action
    In the body of letrec with binders $s$j_s1RF :: Int#
                                                    -> Int#
                                                    -> ((forall {s}.
                                                         MArray s
                                                         -> Int -> State# s -> (# State# s, () #))
                                                        ~R# Action)
                                                    -> TextBuilder
    In the body of lambda with binder ww_X8 :: Action
    In the body of lambda with binder ww1_X9 :: Int#
    In the body of lambda with binder ww2_Xa :: Int#
    In the body of letrec with binders $s$j_s1Rx :: Int#
                                                    -> Int#
                                                    -> ((forall {s}.
                                                         MArray s
                                                         -> Int -> State# s -> (# State# s, () #))
                                                        ~R# Action)
                                                    -> TextBuilder
    In a rule attached to $j_s1PG :: Action
                                     -> Int# -> Int# -> TextBuilder
    Substitution: [TCvSubst
                     In scope: InScope {sg_s1Rb sg_s1Rr}
                     Type env: []
                     Co env: [s1Rb :-> sg_s1Rb, s1Rr :-> sg_s1Rr]]
*** Offending Program ***
$ctoSQL_a1C6 :: forall a. ToSQL a => Maybe a -> Builder
[LclId,
 Arity=2,
 Str=<MCM(L)><1L>,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [60 30] 40 0}]
$ctoSQL_a1C6
  = \ (@a_a1C3)
      ($dToSQL_a1C4 [Dmd=MCM(L)] :: ToSQL a_a1C3)
      (ds_d1DU [Dmd=1L, OS=OneShot] :: Maybe a_a1C3) ->
      case ds_d1DU of {
        Nothing ->
          $fMonoidBuilder2
          `cast` (Sym (N:Builder[0]) :: TextBuilder ~R# Builder);
        Just a_a1ay ->
          ($dToSQL_a1C4
           `cast` (N:ToSQL[0] <a_a1C3>_N
                   :: ToSQL a_a1C3 ~R# (a_a1C3 -> Builder)))
            a_a1ay
      }

$fToSQLMaybe [InlPrag=INLINE (sat-args=0)]
  :: forall a. ToSQL a => ToSQL (Maybe a)
[LclIdX[DFunId(nt)],
 Arity=2,
 Str=<MCM(L)><1L>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_a1C6
               `cast` (forall (a :: <*>_N).
                       <ToSQL a>_R %<'Many>_N ->_R Sym (N:ToSQL[0] <Maybe a>_N)
                       :: (forall {a}. ToSQL a => Maybe a -> Builder)
                          ~R# (forall {a}. ToSQL a => ToSQL (Maybe a)))}]
$fToSQLMaybe
  = $ctoSQL_a1C6
    `cast` (forall (a :: <*>_N).
            <ToSQL a>_R %<'Many>_N ->_R Sym (N:ToSQL[0] <Maybe a>_N)
            :: (forall {a}. ToSQL a => Maybe a -> Builder)
               ~R# (forall {a}. ToSQL a => ToSQL (Maybe a)))

lvl_s1Qi :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 30 0}]
lvl_s1Qi = "WHERE"#

lvl_s1Ok :: TextBuilder
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 60 10}]
lvl_s1Ok
  = case $wgo1 (unpackCString# lvl_s1Qi) of
    { (# ww_a1OP, ww1_a1OQ, ww2_a1OR #) ->
    TextBuilder ww_a1OP ww1_a1OQ ww2_a1OR
    }

$ctoSQL_s1Ic :: WhereFrag -> TextBuilder
[LclId,
 Arity=1,
 Str=<A>,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
$ctoSQL_s1Ic = \ _ [Occ=Dead, Dmd=A] -> lvl_s1Ok

$fToSQLWhereFrag [InlPrag=INLINE (sat-args=0)] :: ToSQL WhereFrag
[LclIdX[DFunId(nt)],
 Arity=1,
 Str=<A>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_s1Ic
               `cast` (<WhereFrag>_R %<'Many>_N ->_R Sym (N:Builder[0])
                       ; Sym (N:ToSQL[0] <WhereFrag>_N)
                       :: (WhereFrag -> TextBuilder) ~R# ToSQL WhereFrag)}]
$fToSQLWhereFrag
  = $ctoSQL_s1Ic
    `cast` (<WhereFrag>_R %<'Many>_N ->_R Sym (N:Builder[0])
            ; Sym (N:ToSQL[0] <WhereFrag>_N)
            :: (WhereFrag -> TextBuilder) ~R# ToSQL WhereFrag)

lvl_s1Ol :: [Char]
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 63 0}]
lvl_s1Ol
  = case quotRemInt# -9223372036854775808# 10# of
    { (# q_a1Im, r_a1In #) ->
    $fShow(,)_itos'
      (negateInt# q_a1Im)
      ($fShow(,)_itos' (negateInt# r_a1In) ([] @Char))
    }

lvl_s1Om :: [Char]
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
lvl_s1Om = : @Char $fShow(,)9 lvl_s1Ol

$ctoSQL_a1BE :: SQLExp -> Builder
[LclId,
 Arity=1,
 Str=<1!P(L)>,
 Cpr=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [20] 242 30}]
$ctoSQL_a1BE
  = \ (ds_d1DO [Dmd=1!P(L)] :: SQLExp) ->
      case ds_d1DO of { SEPrep bx_d1E0 ->
      case <# bx_d1E0 0# of {
        __DEFAULT ->
          case $wgo1 ($fShow(,)_itos' bx_d1E0 ([] @Char)) of
          { (# ww_a1OP, ww1_a1OQ, ww2_a1OR #) ->
          (TextBuilder ww_a1OP ww1_a1OQ ww2_a1OR)
          `cast` (Sym (N:Builder[0]) :: TextBuilder ~R# Builder)
          };
        1# ->
          case bx_d1E0 of wild_a1Ik {
            __DEFAULT ->
              case $wgo1
                     (: @Char
                        $fShow(,)9
                        ($fShow(,)_itos' (negateInt# wild_a1Ik) ([] @Char)))
              of
              { (# ww_a1OP, ww1_a1OQ, ww2_a1OR #) ->
              (TextBuilder ww_a1OP ww1_a1OQ ww2_a1OR)
              `cast` (Sym (N:Builder[0]) :: TextBuilder ~R# Builder)
              };
            -9223372036854775808# ->
              case $wgo1 lvl_s1Om of { (# ww_a1OP, ww1_a1OQ, ww2_a1OR #) ->
              (TextBuilder ww_a1OP ww1_a1OQ ww2_a1OR)
              `cast` (Sym (N:Builder[0]) :: TextBuilder ~R# Builder)
              }
          }
      }
      }

$fToSQLSQLExp [InlPrag=INLINE (sat-args=0)] :: ToSQL SQLExp
[LclIdX[DFunId(nt)],
 Arity=1,
 Str=<1!P(L)>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_a1BE
               `cast` (Sym (N:ToSQL[0] <SQLExp>_N)
                       :: (SQLExp -> Builder) ~R# ToSQL SQLExp)}]
$fToSQLSQLExp
  = $ctoSQL_a1BE
    `cast` (Sym (N:ToSQL[0] <SQLExp>_N)
            :: (SQLExp -> Builder) ~R# ToSQL SQLExp)

lvl_s1Qj :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 40 0}]
lvl_s1Qj = "undefined"#

lvl_s1Qk :: [Char]
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=True,
         WorkFree=False, Expandable=True, Guidance=IF_ARGS [] 20 0}]
lvl_s1Qk = unpackCString# lvl_s1Qj

lvl_s1Ql :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 70 0}]
lvl_s1Ql = "repro2-0.1.0.0-inplace"#

lvl_s1Qm :: [Char]
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=True,
         WorkFree=False, Expandable=True, Guidance=IF_ARGS [] 20 0}]
lvl_s1Qm = unpackCString# lvl_s1Ql

lvl_s1Qn :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 30 0}]
lvl_s1Qn = "Repro"#

lvl_s1Qo :: [Char]
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=True,
         WorkFree=False, Expandable=True, Guidance=IF_ARGS [] 20 0}]
lvl_s1Qo = unpackCString# lvl_s1Qn

lvl_s1Qp :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 40 0}]
lvl_s1Qp = "src/Repro.hs"#

lvl_s1Qq :: [Char]
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=True,
         WorkFree=False, Expandable=True, Guidance=IF_ARGS [] 20 0}]
lvl_s1Qq = unpackCString# lvl_s1Qp

lvl_s1Qr :: Int
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
lvl_s1Qr = I# 47#

lvl_s1Qs :: Int
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
lvl_s1Qs = I# 27#

lvl_s1Qu :: Int
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
lvl_s1Qu = I# 36#

lvl_s1Qv :: SrcLoc
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
lvl_s1Qv
  = SrcLoc
      lvl_s1Qm lvl_s1Qo lvl_s1Qq lvl_s1Qr lvl_s1Qs lvl_s1Qr lvl_s1Qu

lvl_s1Qw :: CallStack
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
lvl_s1Qw = PushCallStack lvl_s1Qk lvl_s1Qv EmptyCallStack

lvl_s1Ov :: Builder
[LclId,
 Str=b,
 Cpr=b,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=NEVER}]
lvl_s1Ov
  = undefined
      @LiftedRep
      @Builder
      (lvl_s1Qw
       `cast` (Sym (N:IP[0] <"callStack">_N <CallStack>_N)
               :: CallStack ~R# (?callStack::CallStack)))

$ctoSQL_a1oA :: Extractor -> Builder
[LclId,
 Arity=1,
 Str=<L>b,
 Cpr=b,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
$ctoSQL_a1oA
  = \ (ds_d1DM [Dmd=1!B] :: Extractor) ->
      case ds_d1DM of { Extractor bx_d1DZ [Dmd=B] -> lvl_s1Ov }

$fToSQLExtractor [InlPrag=INLINE (sat-args=0)] :: ToSQL Extractor
[LclIdX[DFunId(nt)],
 Arity=1,
 Str=<L>b,
 Cpr=b,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_a1oA
               `cast` (Sym (N:ToSQL[0] <Extractor>_N)
                       :: (Extractor -> Builder) ~R# ToSQL Extractor)}]
$fToSQLExtractor
  = $ctoSQL_a1oA
    `cast` (Sym (N:ToSQL[0] <Extractor>_N)
            :: (Extractor -> Builder) ~R# ToSQL Extractor)

lvl_s1Qx :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 80 0}]
lvl_s1Qx = "src/Repro.hs:51:10-23|toSQL"#

$ctoSQL_a1ov :: FromItem -> Builder
[LclId,
 Str=b,
 Cpr=b,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=NEVER}]
$ctoSQL_a1ov
  = noMethodBindingError @LiftedRep @(FromItem -> Builder) lvl_s1Qx

$fToSQLFromItem [InlPrag=INLINE (sat-args=0)] :: ToSQL FromItem
[LclIdX[DFunId(nt)],
 Str=b,
 Cpr=b,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=False, ConLike=False,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_a1ov
               `cast` (Sym (N:ToSQL[0] <FromItem>_N)
                       :: (FromItem -> Builder) ~R# ToSQL FromItem)}]
$fToSQLFromItem
  = $ctoSQL_a1ov
    `cast` (Sym (N:ToSQL[0] <FromItem>_N)
            :: (FromItem -> Builder) ~R# ToSQL FromItem)

$fToSQLSetExpItem [InlPrag=INLINE (sat-args=0)] :: ToSQL SetExpItem
[LclIdX[DFunId(nt)],
 Arity=1,
 Str=<1!P(L)>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_a1BE
               `cast` (Sym (N:SetExpItem[0]) %<'Many>_N ->_R <Builder>_R
                       ; Sym (N:ToSQL[0] <SetExpItem>_N)
                       :: (SQLExp -> Builder) ~R# ToSQL SetExpItem)}]
$fToSQLSetExpItem
  = $ctoSQL_a1BE
    `cast` (Sym (N:SetExpItem[0]) %<'Many>_N ->_R <Builder>_R
            ; Sym (N:ToSQL[0] <SetExpItem>_N)
            :: (SQLExp -> Builder) ~R# ToSQL SetExpItem)

getWFBoolExp_s1II :: WhereFrag -> WhereFrag
[LclId,
 Arity=1,
 Str=<1!A>,
 Cpr=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
getWFBoolExp_s1II = \ (ds_d1DG [Dmd=1!A] :: WhereFrag) -> ds_d1DG

getWFBoolExp :: WhereFrag -> ()
[LclIdX[[RecSel]],
 Arity=1,
 Str=<1!A>,
 Cpr=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)}]
getWFBoolExp
  = getWFBoolExp_s1II
    `cast` (<WhereFrag>_R %<'Many>_N ->_R N:WhereFrag[0]
            :: (WhereFrag -> WhereFrag) ~R# (WhereFrag -> ()))

upRet :: SQLUpdate -> Maybe RetExp
[LclIdX[[RecSel]],
 Arity=1,
 Str=<1!P(A,A,A,1L)>,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
upRet
  = \ (ds_d1DA [Dmd=1!P(A,A,A,1L)] :: SQLUpdate) ->
      case ds_d1DA of
      { SQLUpdate ds_d1DC [Dmd=A] ds_d1DD [Dmd=A] ds_d1DE [Dmd=A]
                  ds_d1DF [Dmd=1L] ->
      ds_d1DF
      }

upWhere :: SQLUpdate -> Maybe WhereFrag
[LclIdX[[RecSel]],
 Arity=1,
 Str=<1!P(A,A,1L,A)>,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
upWhere
  = \ (ds_d1Du [Dmd=1!P(A,A,1L,A)] :: SQLUpdate) ->
      case ds_d1Du of
      { SQLUpdate ds_d1Dw [Dmd=A] ds_d1Dx [Dmd=A] ds_d1Dy [Dmd=1L]
                  ds_d1Dz [Dmd=A] ->
      ds_d1Dy
      }

upFrom :: SQLUpdate -> Maybe FromExp
[LclIdX[[RecSel]],
 Arity=1,
 Str=<1!P(A,1L,A,A)>,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
upFrom
  = \ (ds_d1Do [Dmd=1!P(A,1L,A,A)] :: SQLUpdate) ->
      case ds_d1Do of
      { SQLUpdate ds_d1Dq [Dmd=A] ds_d1Dr [Dmd=1L] ds_d1Ds [Dmd=A]
                  ds_d1Dt [Dmd=A] ->
      ds_d1Dr
      }

upSet :: SQLUpdate -> SetExp
[LclIdX[[RecSel]],
 Arity=1,
 Str=<1!P(1L,A,A,A)>,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
upSet
  = \ (ds_d1Di [Dmd=1!P(1L,A,A,A)] :: SQLUpdate) ->
      case ds_d1Di of
      { SQLUpdate ds_d1Dk [Dmd=1L] ds_d1Dl [Dmd=A] ds_d1Dm [Dmd=A]
                  ds_d1Dn [Dmd=A] ->
      ds_d1Dk
      }

upTable :: SQLUpdate -> ()
[LclIdX[[RecSel]],
 Arity=1,
 Str=<1!A>,
 Cpr=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
upTable
  = \ (ds_d1Dc [Dmd=1!A] :: SQLUpdate) ->
      case ds_d1Dc of
      { SQLUpdate ds_d1De [Dmd=A] ds_d1Df [Dmd=A] ds_d1Dg [Dmd=A]
                  ds_d1Dh [Dmd=A] ->
      ()
      }

(<+>) [InlPrag=INLINE (sat-args=2)]
  :: forall a. ToSQL a => Text -> [a] -> Builder
[LclIdX,
 Arity=3,
 Str=<L><A><1L>,
 Cpr=1,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=2,unsat_ok=False,boring_ok=False)
         Tmpl= \ (@a_a1mD)
                 ($dToSQL_a1mE :: ToSQL a_a1mD)
                 _ [Occ=Dead]
                 (ds_d1D2 [Occ=Once1!] :: [a_a1mD]) ->
                 case ds_d1D2 of {
                   [] ->
                     $fMonoidBuilder2
                     `cast` (Sym (N:Builder[0]) :: TextBuilder ~R# Builder);
                   : x_a1bk [Occ=Once1] xs_a1bl [Occ=Once1] ->
                     ($fIsStringTextBuilder_$c<>
                        ((($dToSQL_a1mE
                           `cast` (N:ToSQL[0] <a_a1mD>_N
                                   :: ToSQL a_a1mD ~R# (a_a1mD -> Builder)))
                            x_a1bk)
                         `cast` (N:Builder[0] :: Builder ~R# TextBuilder))
                        ($fMonoidBuilder1
                           ((build
                               @Builder
                               (\ (@a_d1D7)
                                  (c_d1D8 [Occ=OnceL1!, OS=OneShot] :: Builder -> a_d1D7 -> a_d1D7)
                                  (n_d1D9 [Occ=Once1, OS=OneShot] :: a_d1D7) ->
                                  foldr
                                    @a_a1mD
                                    @a_d1D7
                                    (\ (ds_d1Db [Occ=Once1] :: a_a1mD)
                                       (ds_d1Da [Occ=Once1, OS=OneShot] :: a_d1D7) ->
                                       c_d1D8
                                         (($dToSQL_a1mE
                                           `cast` (N:ToSQL[0] <a_a1mD>_N
                                                   :: ToSQL a_a1mD ~R# (a_a1mD -> Builder)))
                                            ds_d1Db)
                                         ds_d1Da)
                                    n_d1D9
                                    xs_a1bl))
                            `cast` (([N:Builder[0]])_R :: [Builder] ~R# [TextBuilder]))))
                     `cast` (Sym (N:Builder[0]) :: TextBuilder ~R# Builder)
                 }}]
(<+>)
  = \ (@a_a1mD)
      ($dToSQL_a1mE :: ToSQL a_a1mD)
      _ [Occ=Dead, Dmd=A]
      (ds_d1D2 [Dmd=1L] :: [a_a1mD]) ->
      case ds_d1D2 of {
        [] ->
          $fMonoidBuilder2
          `cast` (Sym (N:Builder[0]) :: TextBuilder ~R# Builder);
        : x_a1bk xs_a1bl [Dmd=1L] ->
          case (($dToSQL_a1mE
                 `cast` (N:ToSQL[0] <a_a1mD>_N
                         :: ToSQL a_a1mD ~R# (a_a1mD -> Builder)))
                  x_a1bk)
               `cast` (N:Builder[0] :: Builder ~R# TextBuilder)
          of
          { TextBuilder ww_a1OX [Dmd=LCL(C1(C1(!P(L,A))))] ww1_a1OY
                        ww2_a1OZ ->
          letrec {
            go1_a1Qb [Occ=LoopBreaker] :: [a_a1mD] -> [Builder]
            [LclId,
             Arity=1,
             Str=<1L>,
             Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                     WorkFree=True, Expandable=True, Guidance=IF_ARGS [30] 70 20}]
            go1_a1Qb
              = \ (ds_a1Qc [Dmd=1L] :: [a_a1mD]) ->
                  case ds_a1Qc of {
                    [] -> [] @Builder;
                    : y_a1Qf ys_a1Qg [Dmd=ML] ->
                      : @Builder
                        (($dToSQL_a1mE
                          `cast` (N:ToSQL[0] <a_a1mD>_N
                                  :: ToSQL a_a1mD ~R# (a_a1mD -> Builder)))
                           y_a1Qf)
                        (go1_a1Qb ys_a1Qg)
                  }; } in
          case $wgo2
                 ((go1_a1Qb xs_a1bl)
                  `cast` (([N:Builder[0]])_R :: [Builder] ~R# [TextBuilder]))
          of
          { (# ww_a1Pk [Dmd=LCL(C1(C1(L)))], ww1_a1Pl, ww2_a1Pm #) ->
          (TextBuilder
             ((\ (@s_a1P2)
                 (array_a1P3 :: MArray s_a1P2)
                 (offset_a1P4 :: Int)
                 (s1_a1P5 :: State# s_a1P2) ->
                 case (((ww_a1OX
                         `cast` (N:Action[0]
                                 :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                          @s_a1P2 array_a1P3 offset_a1P4)
                       `cast` (N:ST[0] <s_a1P2>_N <()>_R
                               :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                        s1_a1P5
                 of
                 { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                 (((ww_a1Pk
                    `cast` (N:Action[0]
                            :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                     @s_a1P2
                     array_a1P3
                     (case offset_a1P4 of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OY) }))
                  `cast` (N:ST[0] <s_a1P2>_N <()>_R
                          :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                   ipv_a1Pb
                 })
              `cast` (forall (s :: <*>_N).
                      <MArray s>_R
                      %<'Many>_N ->_R <Int>_R
                      %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                      ; Sym (N:Action[0])
                      :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
             (+# ww1_a1OY ww1_a1Pl)
             (+# ww2_a1OZ ww2_a1Pm))
          `cast` (Sym (N:Builder[0]) :: TextBuilder ~R# Builder)
          }
          }
      }

lvl_s1Qy :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 40 0}]
lvl_s1Qy = "RETURNING"#

lvl_s1Oz :: TextBuilder
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 60 10}]
lvl_s1Oz
  = case $wgo1 (unpackCString# lvl_s1Qy) of
    { (# ww_a1OP, ww1_a1OQ, ww2_a1OR #) ->
    TextBuilder ww_a1OP ww1_a1OQ ww2_a1OR
    }

$ctoSQL_s1Ob :: RetExp -> TextBuilder
[LclId,
 Arity=1,
 Str=<1L>,
 Cpr=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [40] 130 10}]
$ctoSQL_s1Ob
  = \ (ds_d1DI [Dmd=1L] :: RetExp) ->
      case lvl_s1Oz of
      { TextBuilder ww_a1OX [Dmd=LCL(C1(C1(!P(L,A))))] ww1_a1OY
                    ww2_a1OZ ->
      case ds_d1DI `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
        [] ->
          TextBuilder
            ((\ (@s_a1P2)
                (array_a1P3 :: MArray s_a1P2)
                (offset_a1P4 :: Int)
                (s1_a1P5 :: State# s_a1P2) ->
                case (((ww_a1OX
                        `cast` (N:Action[0]
                                :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                         @s_a1P2 array_a1P3 offset_a1P4)
                      `cast` (N:ST[0] <s_a1P2>_N <()>_R
                              :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                       s1_a1P5
                of
                { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                (# ipv_a1Pb, () #)
                })
             `cast` (forall (s :: <*>_N).
                     <MArray s>_R
                     %<'Many>_N ->_R <Int>_R
                     %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                     ; Sym (N:Action[0])
                     :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
            ww1_a1OY
            ww2_a1OZ;
        : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
          case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
          case lvl_s1Ov of wild_00 { }
          }
      }
      }

$fToSQLRetExp [InlPrag=INLINE (sat-args=0)] :: ToSQL RetExp
[LclIdX[DFunId(nt)],
 Arity=1,
 Str=<1L>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_s1Ob
               `cast` (<RetExp>_R %<'Many>_N ->_R Sym (N:Builder[0])
                       ; Sym (N:ToSQL[0] <RetExp>_N)
                       :: (RetExp -> TextBuilder) ~R# ToSQL RetExp)}]
$fToSQLRetExp
  = $ctoSQL_s1Ob
    `cast` (<RetExp>_R %<'Many>_N ->_R Sym (N:Builder[0])
            ; Sym (N:ToSQL[0] <RetExp>_N)
            :: (RetExp -> TextBuilder) ~R# ToSQL RetExp)

lvl_s1Qz :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 20 0}]
lvl_s1Qz = "SET"#

lvl_s1OC :: TextBuilder
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 60 10}]
lvl_s1OC
  = case $wgo1 (unpackCString# lvl_s1Qz) of
    { (# ww_a1OP, ww1_a1OQ, ww2_a1OR #) ->
    TextBuilder ww_a1OP ww1_a1OQ ww2_a1OR
    }

Rec {
go1_s1QB [Occ=LoopBreaker] :: [SetExpItem] -> [Builder]
[LclId,
 Arity=1,
 Str=<1L>,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [30] 70 20}]
go1_s1QB
  = \ (ds_a1Qc [Dmd=1L] :: [SetExpItem]) ->
      case ds_a1Qc of {
        [] -> [] @Builder;
        : y_a1Qf [Dmd=M!P(L)] ys_a1Qg [Dmd=ML] ->
          : @Builder
            ($ctoSQL_a1BE
               (y_a1Qf `cast` (N:SetExpItem[0] :: SetExpItem ~R# SQLExp)))
            (go1_s1QB ys_a1Qg)
      }
end Rec }

$ctoSQL_s1Oc :: SetExp -> TextBuilder
[LclId,
 Arity=1,
 Str=<1L>,
 Cpr=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=NEVER}]
$ctoSQL_s1Oc
  = \ (ds_d1DJ [Dmd=1L] :: SetExp) ->
      case lvl_s1OC of
      { TextBuilder ww_a1OX [Dmd=LCL(C1(C1(!P(L,A))))] ww1_a1OY
                    ww2_a1OZ ->
      case ds_d1DJ `cast` (N:SetExp[0] :: SetExp ~R# [SetExpItem]) of {
        [] ->
          TextBuilder
            ((\ (@s_a1P2)
                (array_a1P3 :: MArray s_a1P2)
                (offset_a1P4 :: Int)
                (s1_a1P5 :: State# s_a1P2) ->
                case (((ww_a1OX
                        `cast` (N:Action[0]
                                :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                         @s_a1P2 array_a1P3 offset_a1P4)
                      `cast` (N:ST[0] <s_a1P2>_N <()>_R
                              :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                       s1_a1P5
                of
                { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                (# ipv_a1Pb, () #)
                })
             `cast` (forall (s :: <*>_N).
                     <MArray s>_R
                     %<'Many>_N ->_R <Int>_R
                     %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                     ; Sym (N:Action[0])
                     :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
            ww1_a1OY
            ww2_a1OZ;
        : x_a1bk [Dmd=1!P(L)] xs_a1bl [Dmd=1L] ->
          case x_a1bk `cast` (N:SetExpItem[0] :: SetExpItem ~R# SQLExp) of
          { SEPrep bx_d1E0 ->
          case <# bx_d1E0 0# of {
            __DEFAULT ->
              case $wgo1 ($fShow(,)_itos' bx_d1E0 ([] @Char)) of
              { (# ww_a1OP [Dmd=1CL(C1(C1(!P(L,A))))], ww1_a1OQ, ww2_a1OR #) ->
              case ww_a1OP
                   `cast` (N:Action[0]
                           :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
              of nt_s1PN [Dmd=LCL(CS(CS(!P(L,A))))]
              { __DEFAULT ->
              case $wgo2
                     ((go1_s1QB xs_a1bl)
                      `cast` (([N:Builder[0]])_R :: [Builder] ~R# [TextBuilder]))
              of
              { (# ww_a1Pk [Dmd=LCL(C1(C1(L)))], ww1_a1Pl, ww2_a1Pm #) ->
              TextBuilder
                ((\ (@s_X8)
                    (array_X9 :: MArray s_X8)
                    (offset_Xa :: Int)
                    (s1_Xb :: State# s_X8) ->
                    case (((ww_a1OX
                            `cast` (N:Action[0]
                                    :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                             @s_X8 array_X9 offset_Xa)
                          `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                           s1_Xb
                    of
                    { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                    let {
                      offset_Xd :: Int
                      [LclId,
                       Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                               WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 21 10}]
                      offset_Xd
                        = case offset_Xa of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OY) } } in
                    case ((nt_s1PN @s_X8 array_X9 offset_Xd)
                          `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                           ipv_a1Pb
                    of
                    { (# ipv_Xf, ipv1_Xg [Dmd=A] #) ->
                    (((ww_a1Pk
                       `cast` (N:Action[0]
                               :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                        @s_X8
                        array_X9
                        (case offset_Xd of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OQ) }))
                     `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                      ipv_Xf
                    }
                    })
                 `cast` (forall (s :: <*>_N).
                         <MArray s>_R
                         %<'Many>_N ->_R <Int>_R
                         %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                         ; Sym (N:Action[0])
                         :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                (+# ww1_a1OY (+# ww1_a1OQ ww1_a1Pl))
                (+# ww2_a1OZ (+# ww2_a1OR ww2_a1Pm))
              }
              }
              };
            1# ->
              case bx_d1E0 of wild_a1Ik {
                __DEFAULT ->
                  case $wgo1
                         (: @Char
                            $fShow(,)9
                            ($fShow(,)_itos' (negateInt# wild_a1Ik) ([] @Char)))
                  of
                  { (# ww_a1OP [Dmd=1CL(C1(C1(!P(L,A))))], ww1_a1OQ, ww2_a1OR #) ->
                  case ww_a1OP
                       `cast` (N:Action[0]
                               :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
                  of nt_s1PN [Dmd=LCL(CS(CS(!P(L,A))))]
                  { __DEFAULT ->
                  case $wgo2
                         ((go1_s1QB xs_a1bl)
                          `cast` (([N:Builder[0]])_R :: [Builder] ~R# [TextBuilder]))
                  of
                  { (# ww_a1Pk [Dmd=LCL(C1(C1(L)))], ww1_a1Pl, ww2_a1Pm #) ->
                  TextBuilder
                    ((\ (@s_X8)
                        (array_X9 :: MArray s_X8)
                        (offset_Xa :: Int)
                        (s1_Xb :: State# s_X8) ->
                        case (((ww_a1OX
                                `cast` (N:Action[0]
                                        :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                                 @s_X8 array_X9 offset_Xa)
                              `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                               s1_Xb
                        of
                        { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                        let {
                          offset_Xd :: Int
                          [LclId,
                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                   WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 21 10}]
                          offset_Xd
                            = case offset_Xa of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OY) } } in
                        case ((nt_s1PN @s_X8 array_X9 offset_Xd)
                              `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                               ipv_a1Pb
                        of
                        { (# ipv_Xf, ipv1_Xg [Dmd=A] #) ->
                        (((ww_a1Pk
                           `cast` (N:Action[0]
                                   :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                            @s_X8
                            array_X9
                            (case offset_Xd of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OQ) }))
                         `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                          ipv_Xf
                        }
                        })
                     `cast` (forall (s :: <*>_N).
                             <MArray s>_R
                             %<'Many>_N ->_R <Int>_R
                             %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                             ; Sym (N:Action[0])
                             :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                    (+# ww1_a1OY (+# ww1_a1OQ ww1_a1Pl))
                    (+# ww2_a1OZ (+# ww2_a1OR ww2_a1Pm))
                  }
                  }
                  };
                -9223372036854775808# ->
                  case $wgo1 lvl_s1Om of
                  { (# ww_a1OP [Dmd=1CL(C1(C1(!P(L,A))))], ww1_a1OQ, ww2_a1OR #) ->
                  case ww_a1OP
                       `cast` (N:Action[0]
                               :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
                  of nt_s1PN [Dmd=LCL(CS(CS(!P(L,A))))]
                  { __DEFAULT ->
                  case $wgo2
                         ((go1_s1QB xs_a1bl)
                          `cast` (([N:Builder[0]])_R :: [Builder] ~R# [TextBuilder]))
                  of
                  { (# ww_a1Pk [Dmd=LCL(C1(C1(L)))], ww1_a1Pl, ww2_a1Pm #) ->
                  TextBuilder
                    ((\ (@s_X8)
                        (array_X9 :: MArray s_X8)
                        (offset_Xa :: Int)
                        (s1_Xb :: State# s_X8) ->
                        case (((ww_a1OX
                                `cast` (N:Action[0]
                                        :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                                 @s_X8 array_X9 offset_Xa)
                              `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                               s1_Xb
                        of
                        { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                        let {
                          offset_Xd :: Int
                          [LclId,
                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                   WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 21 10}]
                          offset_Xd
                            = case offset_Xa of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OY) } } in
                        case ((nt_s1PN @s_X8 array_X9 offset_Xd)
                              `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                               ipv_a1Pb
                        of
                        { (# ipv_Xf, ipv1_Xg [Dmd=A] #) ->
                        (((ww_a1Pk
                           `cast` (N:Action[0]
                                   :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                            @s_X8
                            array_X9
                            (case offset_Xd of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OQ) }))
                         `cast` (N:ST[0] <s_X8>_N <()>_R :: ST s_X8 () ~R# STRep s_X8 ()))
                          ipv_Xf
                        }
                        })
                     `cast` (forall (s :: <*>_N).
                             <MArray s>_R
                             %<'Many>_N ->_R <Int>_R
                             %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                             ; Sym (N:Action[0])
                             :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                    (+# ww1_a1OY (+# ww1_a1OQ ww1_a1Pl))
                    (+# ww2_a1OZ (+# ww2_a1OR ww2_a1Pm))
                  }
                  }
                  }
              }
          }
          }
      }
      }

$fToSQLSetExp [InlPrag=INLINE (sat-args=0)] :: ToSQL SetExp
[LclIdX[DFunId(nt)],
 Arity=1,
 Str=<1L>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_s1Oc
               `cast` (<SetExp>_R %<'Many>_N ->_R Sym (N:Builder[0])
                       ; Sym (N:ToSQL[0] <SetExp>_N)
                       :: (SetExp -> TextBuilder) ~R# ToSQL SetExp)}]
$fToSQLSetExp
  = $ctoSQL_s1Oc
    `cast` (<SetExp>_R %<'Many>_N ->_R Sym (N:Builder[0])
            ; Sym (N:ToSQL[0] <SetExp>_N)
            :: (SetExp -> TextBuilder) ~R# ToSQL SetExp)

lvl_s1QG :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 20 0}]
lvl_s1QG = "FROM"#

lvl_s1OF :: TextBuilder
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 60 10}]
lvl_s1OF
  = case $wgo1 (unpackCString# lvl_s1QG) of
    { (# ww_a1OP, ww1_a1OQ, ww2_a1OR #) ->
    TextBuilder ww_a1OP ww1_a1OQ ww2_a1OR
    }

$ctoSQL_s1Od :: FromExp -> TextBuilder
[LclId,
 Arity=1,
 Str=<1L>,
 Cpr=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [30] 120 10}]
$ctoSQL_s1Od
  = \ (ds_d1DL [Dmd=1L] :: FromExp) ->
      case lvl_s1OF of
      { TextBuilder ww_a1OX [Dmd=LCL(C1(C1(!P(L,A))))] ww1_a1OY
                    ww2_a1OZ ->
      case ds_d1DL `cast` (N:FromExp[0] :: FromExp ~R# [FromItem]) of {
        [] ->
          TextBuilder
            ((\ (@s_a1P2)
                (array_a1P3 :: MArray s_a1P2)
                (offset_a1P4 :: Int)
                (s1_a1P5 :: State# s_a1P2) ->
                case (((ww_a1OX
                        `cast` (N:Action[0]
                                :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                         @s_a1P2 array_a1P3 offset_a1P4)
                      `cast` (N:ST[0] <s_a1P2>_N <()>_R
                              :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                       s1_a1P5
                of
                { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                (# ipv_a1Pb, () #)
                })
             `cast` (forall (s :: <*>_N).
                     <MArray s>_R
                     %<'Many>_N ->_R <Int>_R
                     %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                     ; Sym (N:Action[0])
                     :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
            ww1_a1OY
            ww2_a1OZ;
        : x_a1bk [Dmd=B] xs_a1bl [Dmd=B] ->
          case $ctoSQL_a1ov of wild_00 { }
      }
      }

$fToSQLFromExp [InlPrag=INLINE (sat-args=0)] :: ToSQL FromExp
[LclIdX[DFunId(nt)],
 Arity=1,
 Str=<1L>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_s1Od
               `cast` (<FromExp>_R %<'Many>_N ->_R Sym (N:Builder[0])
                       ; Sym (N:ToSQL[0] <FromExp>_N)
                       :: (FromExp -> TextBuilder) ~R# ToSQL FromExp)}]
$fToSQLFromExp
  = $ctoSQL_s1Od
    `cast` (<FromExp>_R %<'Many>_N ->_R Sym (N:Builder[0])
            ; Sym (N:ToSQL[0] <FromExp>_N)
            :: (FromExp -> TextBuilder) ~R# ToSQL FromExp)

lvl_s1QH :: Addr#
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 30 0}]
lvl_s1QH = "UPDATE"#

lvl_s1OJ :: TextBuilder
[LclId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 60 10}]
lvl_s1OJ
  = case $wgo1 (unpackCString# lvl_s1QH) of
    { (# ww_a1OP, ww1_a1OQ, ww2_a1OR #) ->
    TextBuilder ww_a1OP ww1_a1OQ ww2_a1OR
    }

$ctoSQL_s1Oh :: SQLUpdate -> TextBuilder
[LclId,
 Arity=1,
 Str=<1!P(1L,1L,1L,1L)>,
 Cpr=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=NEVER}]
$ctoSQL_s1Oh
  = \ (a_a13J [Dmd=1!P(1L,1L,1L,1L)] :: SQLUpdate) ->
      case lvl_s1OJ of
      { TextBuilder ww_a1OX [Dmd=LCL(C1(C1(!P(L,A))))] ww1_a1OY
                    ww2_a1OZ ->
      case lvl_s1OC of
      { TextBuilder ww_X2 [Dmd=LCL(C1(C1(!P(L,A))))] ww1_X3 ww2_X4 ->
      case a_a13J of
      { SQLUpdate ds_d1Dk [Dmd=1L] ds_d1Dl [Dmd=1L] ds_d1Dm [Dmd=1L]
                  ds_d1Dn [Dmd=1L] ->
      join {
        $s$j_s1Rq
          :: Int#
             -> Int#
             -> ((forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                 ~R# Action)
             -> TextBuilder
        [LclId[JoinId(3)(Just [])],
         Arity=3,
         Str=<L><L><L>,
         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                 WorkFree=True, Expandable=True, Guidance=NEVER}]
        $s$j_s1Rq (sc_s1Rd [OS=OneShot] :: Int#)
                  (sc_s1Rc [OS=OneShot] :: Int#)
                  (sg_s1Rb
                     :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                        ~R# Action)
          = join {
              $s$j_s1RF
                :: Int#
                   -> Int#
                   -> ((forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                       ~R# Action)
                   -> TextBuilder
              [LclId[JoinId(3)(Just [])],
               Arity=3,
               Str=<L><L><L>,
               Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                       WorkFree=True, Expandable=True, Guidance=NEVER}]
              $s$j_s1RF (sc_s1RA [OS=OneShot] :: Int#)
                        (sc_s1Rz [OS=OneShot] :: Int#)
                        (sg_s1Ry
                           :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                              ~R# Action)
                = join {
                    $j_s1PG :: Action -> Int# -> Int# -> TextBuilder
                    [LclId[JoinId(3)(Nothing)],
                     Arity=3,
                     Str=<1CL(C1(C1(P(L,A))))><L><L> {d1Dn->1L},
                     Cpr=1,
                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                             WorkFree=True, Expandable=True, Guidance=IF_ARGS [20 0 0] 654 20}]
                    $j_s1PG (ww_Xc [Dmd=1CL(C1(C1(P(L,A)))), OS=OneShot] :: Action)
                            (ww1_Xd [OS=OneShot] :: Int#)
                            (ww2_Xe [OS=OneShot] :: Int#)
                      = case ww_Xc
                             `cast` (N:Action[0]
                                     :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
                        of nt_s1PQ [Dmd=LCL(CS(CS(!P(L,A))))]
                        { __DEFAULT ->
                        let {
                          ww1_X7 :: Int#
                          [LclId,
                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                   WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                          ww1_X7 = +# ww1_X3 sc_s1Rc } in
                        case ds_d1Dn of {
                          Nothing ->
                            TextBuilder
                              ((\ (@s_a1P2)
                                  (array_a1P3 :: MArray s_a1P2)
                                  (offset_a1P4 :: Int)
                                  (s1_a1P5 :: State# s_a1P2) ->
                                  case (((ww_a1OX
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_a1P4)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         s1_a1P5
                                  of
                                  { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                  let {
                                    offset_s1Pz :: Int
                                    [LclId,
                                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                             ConLike=False, WorkFree=False, Expandable=False,
                                             Guidance=IF_ARGS [] 21 10}]
                                    offset_s1Pz
                                      = case offset_a1P4 of { I# x_a1Pf ->
                                        I# (+# x_a1Pf ww1_a1OY)
                                        } } in
                                  case (((ww_X2
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_s1Pz)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_a1Pb
                                  of
                                  { (# ipv_Xl, ipv1_Xm [Dmd=A] #) ->
                                  case ((nt_s1PQ
                                           @s_a1P2
                                           array_a1P3
                                           (case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# (+# x_a1Pf ww1_X7) sc_s1Rz)
                                            }))
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_Xl
                                  of
                                  { (# ipv_Xu, ipv1_Xv [Dmd=A] #) ->
                                  (# ipv_Xu, () #)
                                  }
                                  }
                                  })
                               `cast` (forall (s :: <*>_N).
                                       <MArray s>_R
                                       %<'Many>_N ->_R <Int>_R
                                       %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                       ; Sym (N:Action[0])
                                       :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                              (+# ww1_a1OY (+# ww1_X7 (+# sc_s1Rz ww1_Xd)))
                              (+# ww2_a1OZ (+# (+# ww2_X4 sc_s1Rd) (+# sc_s1RA ww2_Xe)));
                          Just a_a1ay [Dmd=1L] ->
                            case lvl_s1Oz of
                            { TextBuilder ww_Xi [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xj ww2_Xk ->
                            case a_a1ay `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                              [] ->
                                TextBuilder
                                  ((\ (@s_Xm)
                                      (array_Xn :: MArray s_Xm)
                                      (offset_Xo :: Int)
                                      (s1_Xp :: State# s_Xm) ->
                                      case (((ww_a1OX
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_Xo)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             s1_Xp
                                      of
                                      { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                      let {
                                        offset_s1Pz :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Pz
                                          = case offset_Xo of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_a1OY)
                                            } } in
                                      case (((ww_X2
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_s1Pz)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_a1Pb
                                      of
                                      { (# ipv_Xr, ipv1_Xs [Dmd=A] #) ->
                                      let {
                                        offset_s1Pv :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 22 10}]
                                        offset_s1Pv
                                          = case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# (+# x_a1Pf ww1_X7) sc_s1Rz)
                                            } } in
                                      case ((nt_s1PQ @s_Xm array_Xn offset_s1Pv)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xr
                                      of
                                      { (# ipv_XA, ipv1_XB [Dmd=A] #) ->
                                      case (((ww_Xi
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm
                                               array_Xn
                                               (case offset_s1Pv of { I# x_a1Pf ->
                                                I# (+# x_a1Pf ww1_Xd)
                                                }))
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_XA
                                      of
                                      { (# ipv_XD, ipv1_XE [Dmd=A] #) ->
                                      (# ipv_XD, () #)
                                      }
                                      }
                                      }
                                      })
                                   `cast` (forall (s :: <*>_N).
                                           <MArray s>_R
                                           %<'Many>_N ->_R <Int>_R
                                           %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                           ; Sym (N:Action[0])
                                           :: (forall {s}. MArray s -> Int -> STRep s ())
                                              ~R# Action))
                                  (+# ww1_a1OY (+# ww1_X7 (+# sc_s1Rz (+# ww1_Xd ww1_Xj))))
                                  (+#
                                     ww2_a1OZ
                                     (+# (+# ww2_X4 sc_s1Rd) (+# sc_s1RA (+# ww2_Xe ww2_Xk))));
                              : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                                case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                                case lvl_s1Ov of wild_00 { }
                                }
                            }
                            }
                        }
                        } } in
                  case ds_d1Dm of {
                    Nothing ->
                      let {
                        ww1_X7 :: Int#
                        [LclId,
                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                 WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                        ww1_X7 = +# ww1_X3 sc_s1Rc } in
                      case ds_d1Dn of {
                        Nothing ->
                          TextBuilder
                            ((\ (@s_a1P2)
                                (array_a1P3 :: MArray s_a1P2)
                                (offset_a1P4 :: Int)
                                (s1_a1P5 :: State# s_a1P2) ->
                                case (((ww_a1OX
                                        `cast` (N:Action[0]
                                                :: Action
                                                   ~R# (forall s. MArray s -> Int -> ST s ())))
                                         @s_a1P2 array_a1P3 offset_a1P4)
                                      `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                              :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                       s1_a1P5
                                of
                                { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                let {
                                  offset_s1Pz :: Int
                                  [LclId,
                                   Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                           WorkFree=False, Expandable=False,
                                           Guidance=IF_ARGS [] 21 10}]
                                  offset_s1Pz
                                    = case offset_a1P4 of { I# x_a1Pf ->
                                      I# (+# x_a1Pf ww1_a1OY)
                                      } } in
                                case (((ww_X2
                                        `cast` (N:Action[0]
                                                :: Action
                                                   ~R# (forall s. MArray s -> Int -> ST s ())))
                                         @s_a1P2 array_a1P3 offset_s1Pz)
                                      `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                              :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                       ipv_a1Pb
                                of
                                { (# ipv_Xl, ipv1_Xm [Dmd=A] #) ->
                                (# ipv_Xl, () #)
                                }
                                })
                             `cast` (forall (s :: <*>_N).
                                     <MArray s>_R
                                     %<'Many>_N ->_R <Int>_R
                                     %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                     ; Sym (N:Action[0])
                                     :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                            (+# ww1_a1OY (+# ww1_X7 sc_s1Rz))
                            (+# ww2_a1OZ (+# (+# ww2_X4 sc_s1Rd) sc_s1RA));
                        Just a_a1ay [Dmd=1L] ->
                          case lvl_s1Oz of
                          { TextBuilder ww_Xi [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xj ww2_Xk ->
                          case a_a1ay `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                            [] ->
                              TextBuilder
                                ((\ (@s_Xm)
                                    (array_Xn :: MArray s_Xm)
                                    (offset_Xo :: Int)
                                    (s1_Xp :: State# s_Xm) ->
                                    case (((ww_a1OX
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xm array_Xn offset_Xo)
                                          `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                  :: ST s_Xm () ~R# STRep s_Xm ()))
                                           s1_Xp
                                    of
                                    { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                    let {
                                      offset_s1Pz :: Int
                                      [LclId,
                                       Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                               ConLike=False, WorkFree=False, Expandable=False,
                                               Guidance=IF_ARGS [] 21 10}]
                                      offset_s1Pz
                                        = case offset_Xo of { I# x_a1Pf ->
                                          I# (+# x_a1Pf ww1_a1OY)
                                          } } in
                                    case (((ww_X2
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xm array_Xn offset_s1Pz)
                                          `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                  :: ST s_Xm () ~R# STRep s_Xm ()))
                                           ipv_a1Pb
                                    of
                                    { (# ipv_Xr, ipv1_Xs [Dmd=A] #) ->
                                    let {
                                      offset_s1Pv :: Int
                                      [LclId,
                                       Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                               ConLike=False, WorkFree=False, Expandable=False,
                                               Guidance=IF_ARGS [] 22 10}]
                                      offset_s1Pv
                                        = case offset_s1Pz of { I# x_a1Pf ->
                                          I# (+# (+# x_a1Pf ww1_X7) sc_s1Rz)
                                          } } in
                                    case (((ww_Xi
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xm array_Xn offset_s1Pv)
                                          `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                  :: ST s_Xm () ~R# STRep s_Xm ()))
                                           ipv_Xr
                                    of
                                    { (# ipv_XD, ipv1_XE [Dmd=A] #) ->
                                    (# ipv_XD, () #)
                                    }
                                    }
                                    })
                                 `cast` (forall (s :: <*>_N).
                                         <MArray s>_R
                                         %<'Many>_N ->_R <Int>_R
                                         %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                         ; Sym (N:Action[0])
                                         :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                                (+# ww1_a1OY (+# ww1_X7 (+# sc_s1Rz ww1_Xj)))
                                (+# ww2_a1OZ (+# (+# ww2_X4 sc_s1Rd) (+# sc_s1RA ww2_Xk)));
                            : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                              case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                              case lvl_s1Ov of wild_00 { }
                              }
                          }
                          }
                      };
                    Just a_a1ay [Dmd=A] ->
                      case lvl_s1Ok of
                      { TextBuilder ww_Xe [Dmd=1CL(C1(C1(P(L,A))))] ww1_Xf ww2_Xg ->
                      let {
                        ww1_X7 :: Int#
                        [LclId,
                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                 WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                        ww1_X7 = +# ww1_X3 sc_s1Rc } in
                      case ds_d1Dn of {
                        Nothing ->
                          TextBuilder
                            ((\ (@s_a1P2)
                                (array_a1P3 :: MArray s_a1P2)
                                (offset_a1P4 :: Int)
                                (s1_a1P5 :: State# s_a1P2) ->
                                case (((ww_a1OX
                                        `cast` (N:Action[0]
                                                :: Action
                                                   ~R# (forall s. MArray s -> Int -> ST s ())))
                                         @s_a1P2 array_a1P3 offset_a1P4)
                                      `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                              :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                       s1_a1P5
                                of
                                { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                let {
                                  offset_s1Pz :: Int
                                  [LclId,
                                   Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                           WorkFree=False, Expandable=False,
                                           Guidance=IF_ARGS [] 21 10}]
                                  offset_s1Pz
                                    = case offset_a1P4 of { I# x_a1Pf ->
                                      I# (+# x_a1Pf ww1_a1OY)
                                      } } in
                                case (((ww_X2
                                        `cast` (N:Action[0]
                                                :: Action
                                                   ~R# (forall s. MArray s -> Int -> ST s ())))
                                         @s_a1P2 array_a1P3 offset_s1Pz)
                                      `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                              :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                       ipv_a1Pb
                                of
                                { (# ipv_Xl, ipv1_Xm [Dmd=A] #) ->
                                case (((ww_Xe
                                        `cast` (N:Action[0]
                                                :: Action
                                                   ~R# (forall s. MArray s -> Int -> ST s ())))
                                         @s_a1P2
                                         array_a1P3
                                         (case offset_s1Pz of { I# x_a1Pf ->
                                          I# (+# (+# x_a1Pf ww1_X7) sc_s1Rz)
                                          }))
                                      `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                              :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                       ipv_Xl
                                of
                                { (# ipv_Xu, ipv1_Xv [Dmd=A] #) ->
                                (# ipv_Xu, () #)
                                }
                                }
                                })
                             `cast` (forall (s :: <*>_N).
                                     <MArray s>_R
                                     %<'Many>_N ->_R <Int>_R
                                     %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                     ; Sym (N:Action[0])
                                     :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                            (+# ww1_a1OY (+# ww1_X7 (+# sc_s1Rz ww1_Xf)))
                            (+# ww2_a1OZ (+# (+# ww2_X4 sc_s1Rd) (+# sc_s1RA ww2_Xg)));
                        Just a_Xi [Dmd=1L] ->
                          case lvl_s1Oz of
                          { TextBuilder ww_Xk [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xl ww2_Xm ->
                          case a_Xi `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                            [] ->
                              TextBuilder
                                ((\ (@s_Xo)
                                    (array_Xp :: MArray s_Xo)
                                    (offset_Xq :: Int)
                                    (s1_Xr :: State# s_Xo) ->
                                    case (((ww_a1OX
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xo array_Xp offset_Xq)
                                          `cast` (N:ST[0] <s_Xo>_N <()>_R
                                                  :: ST s_Xo () ~R# STRep s_Xo ()))
                                           s1_Xr
                                    of
                                    { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                    let {
                                      offset_s1Pz :: Int
                                      [LclId,
                                       Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                               ConLike=False, WorkFree=False, Expandable=False,
                                               Guidance=IF_ARGS [] 21 10}]
                                      offset_s1Pz
                                        = case offset_Xq of { I# x_a1Pf ->
                                          I# (+# x_a1Pf ww1_a1OY)
                                          } } in
                                    case (((ww_X2
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xo array_Xp offset_s1Pz)
                                          `cast` (N:ST[0] <s_Xo>_N <()>_R
                                                  :: ST s_Xo () ~R# STRep s_Xo ()))
                                           ipv_a1Pb
                                    of
                                    { (# ipv_Xt, ipv1_Xu [Dmd=A] #) ->
                                    let {
                                      offset_s1Pv :: Int
                                      [LclId,
                                       Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                               ConLike=False, WorkFree=False, Expandable=False,
                                               Guidance=IF_ARGS [] 22 10}]
                                      offset_s1Pv
                                        = case offset_s1Pz of { I# x_a1Pf ->
                                          I# (+# (+# x_a1Pf ww1_X7) sc_s1Rz)
                                          } } in
                                    case (((ww_Xe
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xo array_Xp offset_s1Pv)
                                          `cast` (N:ST[0] <s_Xo>_N <()>_R
                                                  :: ST s_Xo () ~R# STRep s_Xo ()))
                                           ipv_Xt
                                    of
                                    { (# ipv_XA, ipv1_XB [Dmd=A] #) ->
                                    case (((ww_Xk
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xo
                                             array_Xp
                                             (case offset_s1Pv of { I# x_a1Pf ->
                                              I# (+# x_a1Pf ww1_Xf)
                                              }))
                                          `cast` (N:ST[0] <s_Xo>_N <()>_R
                                                  :: ST s_Xo () ~R# STRep s_Xo ()))
                                           ipv_XA
                                    of
                                    { (# ipv_XD, ipv1_XE [Dmd=A] #) ->
                                    (# ipv_XD, () #)
                                    }
                                    }
                                    }
                                    })
                                 `cast` (forall (s :: <*>_N).
                                         <MArray s>_R
                                         %<'Many>_N ->_R <Int>_R
                                         %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                         ; Sym (N:Action[0])
                                         :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                                (+# ww1_a1OY (+# ww1_X7 (+# sc_s1Rz (+# ww1_Xf ww1_Xl))))
                                (+#
                                   ww2_a1OZ
                                   (+# (+# ww2_X4 sc_s1Rd) (+# sc_s1RA (+# ww2_Xg ww2_Xm))));
                            : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                              case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                              case lvl_s1Ov of wild_00 { }
                              }
                          }
                          }
                      }
                      }
                  } } in
            join {
              $j_s1PH :: Action -> Int# -> Int# -> TextBuilder
              [LclId[JoinId(3)(Nothing)],
               Arity=3,
               Str=<1CL(C1(C1(P(L,A))))><L><L> {d1Dm->1L d1Dn->1L},
               Cpr=1,
               Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                       WorkFree=True, Expandable=True, Guidance=NEVER},
               RULES: "SC:$j0"
                          forall (sc_s1RA :: Int#)
                                 (sc_s1Rz :: Int#)
                                 (sg_s1Ry
                                    :: (forall {s}.
                                        MArray s -> Int -> State# s -> (# State# s, () #))
                                       ~R# Action).
                            $j_s1PH ($fMonoidBuilder3
                                     `cast` (sg_s1Rb
                                             :: (forall {s}.
                                                 MArray s -> Int -> State# s -> (# State# s, () #))
                                                ~R# Action))
                                    sc_s1Rz
                                    sc_s1RA
                            = jump $s$j_s1RF
                                sc_s1RA
                                sc_s1Rz
                                @~(sg_s1Ry
                                   :: (forall {s}.
                                       MArray s -> Int -> State# s -> (# State# s, () #))
                                      ~R# Action)]
              $j_s1PH (ww_X8 [Dmd=1CL(C1(C1(P(L,A)))), OS=OneShot] :: Action)
                      (ww1_X9 [OS=OneShot] :: Int#)
                      (ww2_Xa [OS=OneShot] :: Int#)
                = case ww_X8
                       `cast` (N:Action[0]
                               :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
                  of nt_s1PP [Dmd=LCL(CS(CS(!P(L,A))))]
                  { __DEFAULT ->
                  join {
                    $s$j_s1Rx
                      :: Int#
                         -> Int#
                         -> ((forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                             ~R# Action)
                         -> TextBuilder
                    [LclId[JoinId(3)(Just [])],
                     Arity=3,
                     Str=<L><L><L>,
                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                             WorkFree=True, Expandable=True, Guidance=IF_ARGS [0 0 0] 643 20}]
                    $s$j_s1Rx (sc_s1Rt [OS=OneShot] :: Int#)
                              (sc_s1Rs [OS=OneShot] :: Int#)
                              (sg_s1Rr
                                 :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                                    ~R# Action)
                      = let {
                          ww1_X7 :: Int#
                          [LclId,
                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                   WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                          ww1_X7 = +# ww1_X3 sc_s1Rc } in
                        case ds_d1Dn of {
                          Nothing ->
                            TextBuilder
                              ((\ (@s_a1P2)
                                  (array_a1P3 :: MArray s_a1P2)
                                  (offset_a1P4 :: Int)
                                  (s1_a1P5 :: State# s_a1P2) ->
                                  case (((ww_a1OX
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_a1P4)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         s1_a1P5
                                  of
                                  { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                  let {
                                    offset_s1Pz :: Int
                                    [LclId,
                                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                             ConLike=False, WorkFree=False, Expandable=False,
                                             Guidance=IF_ARGS [] 21 10}]
                                    offset_s1Pz
                                      = case offset_a1P4 of { I# x_a1Pf ->
                                        I# (+# x_a1Pf ww1_a1OY)
                                        } } in
                                  case (((ww_X2
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_s1Pz)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_a1Pb
                                  of
                                  { (# ipv_Xl, ipv1_Xm [Dmd=A] #) ->
                                  case ((nt_s1PP
                                           @s_a1P2
                                           array_a1P3
                                           (case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X7)
                                            }))
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_Xl
                                  of
                                  { (# ipv_Xr, ipv1_Xs [Dmd=A] #) ->
                                  (# ipv_Xr, () #)
                                  }
                                  }
                                  })
                               `cast` (forall (s :: <*>_N).
                                       <MArray s>_R
                                       %<'Many>_N ->_R <Int>_R
                                       %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                       ; Sym (N:Action[0])
                                       :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                              (+# ww1_a1OY (+# ww1_X7 (+# ww1_X9 sc_s1Rs)))
                              (+# ww2_a1OZ (+# (+# ww2_X4 sc_s1Rd) (+# ww2_Xa sc_s1Rt)));
                          Just a_a1ay [Dmd=1L] ->
                            case lvl_s1Oz of
                            { TextBuilder ww_Xi [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xj ww2_Xk ->
                            case a_a1ay `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                              [] ->
                                TextBuilder
                                  ((\ (@s_Xm)
                                      (array_Xn :: MArray s_Xm)
                                      (offset_Xo :: Int)
                                      (s1_Xp :: State# s_Xm) ->
                                      case (((ww_a1OX
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_Xo)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             s1_Xp
                                      of
                                      { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                      let {
                                        offset_s1Pz :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Pz
                                          = case offset_Xo of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_a1OY)
                                            } } in
                                      case (((ww_X2
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_s1Pz)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_a1Pb
                                      of
                                      { (# ipv_Xr, ipv1_Xs [Dmd=A] #) ->
                                      let {
                                        offset_s1Px :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Px
                                          = case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X7)
                                            } } in
                                      case ((nt_s1PP @s_Xm array_Xn offset_s1Px)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xr
                                      of
                                      { (# ipv_Xx, ipv1_Xy [Dmd=A] #) ->
                                      case (((ww_Xi
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm
                                               array_Xn
                                               (case offset_s1Px of { I# x_a1Pf ->
                                                I# (+# (+# x_a1Pf ww1_X9) sc_s1Rs)
                                                }))
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xx
                                      of
                                      { (# ipv_XD, ipv1_XE [Dmd=A] #) ->
                                      (# ipv_XD, () #)
                                      }
                                      }
                                      }
                                      })
                                   `cast` (forall (s :: <*>_N).
                                           <MArray s>_R
                                           %<'Many>_N ->_R <Int>_R
                                           %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                           ; Sym (N:Action[0])
                                           :: (forall {s}. MArray s -> Int -> STRep s ())
                                              ~R# Action))
                                  (+# ww1_a1OY (+# ww1_X7 (+# ww1_X9 (+# sc_s1Rs ww1_Xj))))
                                  (+#
                                     ww2_a1OZ
                                     (+# (+# ww2_X4 sc_s1Rd) (+# ww2_Xa (+# sc_s1Rt ww2_Xk))));
                              : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                                case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                                case lvl_s1Ov of wild_00 { }
                                }
                            }
                            }
                        } } in
                  join {
                    $j_s1PG :: Action -> Int# -> Int# -> TextBuilder
                    [LclId[JoinId(3)(Nothing)],
                     Arity=3,
                     Str=<1CL(C1(C1(P(L,A))))><L><L> {d1Dn->1L},
                     Cpr=1,
                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                             WorkFree=True, Expandable=True, Guidance=NEVER},
                     RULES: "SC:$j0"
                                forall (sc_s1Rt :: Int#)
                                       (sc_s1Rs :: Int#)
                                       (sg_s1Rr
                                          :: (forall {s}.
                                              MArray s -> Int -> State# s -> (# State# s, () #))
                                             ~R# Action).
                                  $j_s1PG ($fMonoidBuilder3
                                           `cast` (sg_s1Rb
                                                   :: (forall {s}.
                                                       MArray s
                                                       -> Int -> State# s -> (# State# s, () #))
                                                      ~R# Action))
                                          sc_s1Rs
                                          sc_s1Rt
                                  = jump $s$j_s1Rx
                                      sc_s1Rt
                                      sc_s1Rs
                                      @~(sg_s1Rr
                                         :: (forall {s}.
                                             MArray s -> Int -> State# s -> (# State# s, () #))
                                            ~R# Action)]
                    $j_s1PG (ww_Xc [Dmd=1CL(C1(C1(P(L,A)))), OS=OneShot] :: Action)
                            (ww1_Xd [OS=OneShot] :: Int#)
                            (ww2_Xe [OS=OneShot] :: Int#)
                      = case ww_Xc
                             `cast` (N:Action[0]
                                     :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
                        of nt_s1PQ [Dmd=LCL(CS(CS(!P(L,A))))]
                        { __DEFAULT ->
                        let {
                          ww1_X7 :: Int#
                          [LclId,
                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                   WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                          ww1_X7 = +# ww1_X3 sc_s1Rc } in
                        case ds_d1Dn of {
                          Nothing ->
                            TextBuilder
                              ((\ (@s_a1P2)
                                  (array_a1P3 :: MArray s_a1P2)
                                  (offset_a1P4 :: Int)
                                  (s1_a1P5 :: State# s_a1P2) ->
                                  case (((ww_a1OX
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_a1P4)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         s1_a1P5
                                  of
                                  { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                  let {
                                    offset_s1Pz :: Int
                                    [LclId,
                                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                             ConLike=False, WorkFree=False, Expandable=False,
                                             Guidance=IF_ARGS [] 21 10}]
                                    offset_s1Pz
                                      = case offset_a1P4 of { I# x_a1Pf ->
                                        I# (+# x_a1Pf ww1_a1OY)
                                        } } in
                                  case (((ww_X2
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_s1Pz)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_a1Pb
                                  of
                                  { (# ipv_Xl, ipv1_Xm [Dmd=A] #) ->
                                  let {
                                    offset_s1Px :: Int
                                    [LclId,
                                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                             ConLike=False, WorkFree=False, Expandable=False,
                                             Guidance=IF_ARGS [] 21 10}]
                                    offset_s1Px
                                      = case offset_s1Pz of { I# x_a1Pf ->
                                        I# (+# x_a1Pf ww1_X7)
                                        } } in
                                  case ((nt_s1PP @s_a1P2 array_a1P3 offset_s1Px)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_Xl
                                  of
                                  { (# ipv_Xr, ipv1_Xs [Dmd=A] #) ->
                                  case ((nt_s1PQ
                                           @s_a1P2
                                           array_a1P3
                                           (case offset_s1Px of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X9)
                                            }))
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_Xr
                                  of
                                  { (# ipv_Xu, ipv1_Xv [Dmd=A] #) ->
                                  (# ipv_Xu, () #)
                                  }
                                  }
                                  }
                                  })
                               `cast` (forall (s :: <*>_N).
                                       <MArray s>_R
                                       %<'Many>_N ->_R <Int>_R
                                       %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                       ; Sym (N:Action[0])
                                       :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                              (+# ww1_a1OY (+# ww1_X7 (+# ww1_X9 ww1_Xd)))
                              (+# ww2_a1OZ (+# (+# ww2_X4 sc_s1Rd) (+# ww2_Xa ww2_Xe)));
                          Just a_a1ay [Dmd=1L] ->
                            case lvl_s1Oz of
                            { TextBuilder ww_Xi [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xj ww2_Xk ->
                            case a_a1ay `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                              [] ->
                                TextBuilder
                                  ((\ (@s_Xm)
                                      (array_Xn :: MArray s_Xm)
                                      (offset_Xo :: Int)
                                      (s1_Xp :: State# s_Xm) ->
                                      case (((ww_a1OX
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_Xo)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             s1_Xp
                                      of
                                      { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                      let {
                                        offset_s1Pz :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Pz
                                          = case offset_Xo of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_a1OY)
                                            } } in
                                      case (((ww_X2
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_s1Pz)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_a1Pb
                                      of
                                      { (# ipv_Xr, ipv1_Xs [Dmd=A] #) ->
                                      let {
                                        offset_s1Px :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Px
                                          = case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X7)
                                            } } in
                                      case ((nt_s1PP @s_Xm array_Xn offset_s1Px)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xr
                                      of
                                      { (# ipv_Xx, ipv1_Xy [Dmd=A] #) ->
                                      let {
                                        offset_s1Pv :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Pv
                                          = case offset_s1Px of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X9)
                                            } } in
                                      case ((nt_s1PQ @s_Xm array_Xn offset_s1Pv)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xx
                                      of
                                      { (# ipv_XA, ipv1_XB [Dmd=A] #) ->
                                      case (((ww_Xi
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm
                                               array_Xn
                                               (case offset_s1Pv of { I# x_a1Pf ->
                                                I# (+# x_a1Pf ww1_Xd)
                                                }))
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_XA
                                      of
                                      { (# ipv_XD, ipv1_XE [Dmd=A] #) ->
                                      (# ipv_XD, () #)
                                      }
                                      }
                                      }
                                      }
                                      })
                                   `cast` (forall (s :: <*>_N).
                                           <MArray s>_R
                                           %<'Many>_N ->_R <Int>_R
                                           %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                           ; Sym (N:Action[0])
                                           :: (forall {s}. MArray s -> Int -> STRep s ())
                                              ~R# Action))
                                  (+# ww1_a1OY (+# ww1_X7 (+# ww1_X9 (+# ww1_Xd ww1_Xj))))
                                  (+#
                                     ww2_a1OZ
                                     (+# (+# ww2_X4 sc_s1Rd) (+# ww2_Xa (+# ww2_Xe ww2_Xk))));
                              : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                                case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                                case lvl_s1Ov of wild_00 { }
                                }
                            }
                            }
                        }
                        } } in
                  case ds_d1Dm of {
                    Nothing ->
                      jump $j_s1PG
                        ($fMonoidBuilder3
                         `cast` (sg_s1Rb
                                 :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                                    ~R# Action))
                        0#
                        0#;
                    Just a_a1ay [Dmd=A] ->
                      case lvl_s1Ok of
                      { TextBuilder ww_Xe [Dmd=1CL(C1(C1(P(L,A))))] ww1_Xf ww2_Xg ->
                      jump $j_s1PG ww_Xe ww1_Xf ww2_Xg
                      }
                  }
                  } } in
            case ds_d1Dl of {
              Nothing ->
                jump $j_s1PH
                  ($fMonoidBuilder3
                   `cast` (sg_s1Rb
                           :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                              ~R# Action))
                  0#
                  0#;
              Just a_a1ay [Dmd=1L] ->
                case lvl_s1OF of
                { TextBuilder ww_Xa [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xb ww2_Xc ->
                case a_a1ay `cast` (N:FromExp[0] :: FromExp ~R# [FromItem]) of {
                  [] ->
                    jump $j_s1PH
                      ((\ (@s_a1P2)
                          (array_a1P3 :: MArray s_a1P2)
                          (offset_a1P4 [OS=OneShot] :: Int)
                          (s1_a1P5 [OS=OneShot] :: State# s_a1P2) ->
                          case (((ww_Xa
                                  `cast` (N:Action[0]
                                          :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                                   @s_a1P2 array_a1P3 offset_a1P4)
                                `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                        :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                 s1_a1P5
                          of
                          { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                          (# ipv_a1Pb, () #)
                          })
                       `cast` (forall (s :: <*>_N).
                               <MArray s>_R
                               %<'Many>_N ->_R <Int>_R
                               %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                               ; Sym (N:Action[0])
                               :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                      ww1_Xb
                      ww2_Xc;
                  : x_a1bk [Dmd=B] xs_a1bl [Dmd=B] ->
                    case $ctoSQL_a1ov of wild_00 { }
                }
                }
            } } in
      join {
        $j_s1PJ :: Action -> Int# -> Int# -> TextBuilder
        [LclId[JoinId(3)(Nothing)],
         Arity=3,
         Str=<1CL(C1(C1(P(L,A))))><L><L> {d1Dl->1L d1Dm->1L d1Dn->1L},
         Cpr=1,
         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                 WorkFree=True, Expandable=True, Guidance=NEVER},
         RULES: "SC:$j0"
                    forall (sc_s1Rd :: Int#)
                           (sc_s1Rc :: Int#)
                           (sg_s1Rb
                              :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                                 ~R# Action).
                      $j_s1PJ ($fMonoidBuilder3
                               `cast` (sg_s1Rb
                                       :: (forall {s}.
                                           MArray s -> Int -> State# s -> (# State# s, () #))
                                          ~R# Action))
                              sc_s1Rc
                              sc_s1Rd
                      = jump $s$j_s1Rq
                          sc_s1Rd
                          sc_s1Rc
                          @~(sg_s1Rb
                             :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                                ~R# Action)]
        $j_s1PJ (ww3_a1P6 [Dmd=1CL(C1(C1(P(L,A)))), OS=OneShot] :: Action)
                (ww4_a1P7 [OS=OneShot] :: Int#)
                (ww5_a1P8 [OS=OneShot] :: Int#)
          = case ww3_a1P6
                 `cast` (N:Action[0]
                         :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
            of nt_s1PO [Dmd=LCL(CS(CS(!P(L,A))))]
            { __DEFAULT ->
            join {
              $s$j_s1R3
                :: Int#
                   -> Int#
                   -> ((forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                       ~R# Action)
                   -> TextBuilder
              [LclId[JoinId(3)(Just [])],
               Arity=3,
               Str=<L><L><L>,
               Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                       WorkFree=True, Expandable=True, Guidance=NEVER}]
              $s$j_s1R3 (sc_s1QY [OS=OneShot] :: Int#)
                        (sc_s1QX [OS=OneShot] :: Int#)
                        (sg_s1QW
                           :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                              ~R# Action)
                = join {
                    $s$j_s1Ra
                      :: Int#
                         -> Int#
                         -> ((forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                             ~R# Action)
                         -> TextBuilder
                    [LclId[JoinId(3)(Just [])],
                     Arity=3,
                     Str=<L><L><L>,
                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                             WorkFree=True, Expandable=True, Guidance=IF_ARGS [0 0 0] 634 20}]
                    $s$j_s1Ra (sc_s1R6 [OS=OneShot] :: Int#)
                              (sc_s1R5 [OS=OneShot] :: Int#)
                              (sg_s1R4
                                 :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                                    ~R# Action)
                      = let {
                          ww1_X7 :: Int#
                          [LclId,
                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                   WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                          ww1_X7 = +# ww1_X3 ww4_a1P7 } in
                        case ds_d1Dn of {
                          Nothing ->
                            TextBuilder
                              ((\ (@s_a1P2)
                                  (array_a1P3 :: MArray s_a1P2)
                                  (offset_a1P4 :: Int)
                                  (s1_a1P5 :: State# s_a1P2) ->
                                  case (((ww_a1OX
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_a1P4)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         s1_a1P5
                                  of
                                  { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                  let {
                                    offset_s1Pz :: Int
                                    [LclId,
                                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                             ConLike=False, WorkFree=False, Expandable=False,
                                             Guidance=IF_ARGS [] 21 10}]
                                    offset_s1Pz
                                      = case offset_a1P4 of { I# x_a1Pf ->
                                        I# (+# x_a1Pf ww1_a1OY)
                                        } } in
                                  case (((ww_X2
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_s1Pz)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_a1Pb
                                  of
                                  { (# ipv_Xl, ipv1_Xm [Dmd=A] #) ->
                                  case ((nt_s1PO
                                           @s_a1P2
                                           array_a1P3
                                           (case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X3)
                                            }))
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_Xl
                                  of
                                  { (# ipv_Xo, ipv1_Xp [Dmd=A] #) ->
                                  (# ipv_Xo, () #)
                                  }
                                  }
                                  })
                               `cast` (forall (s :: <*>_N).
                                       <MArray s>_R
                                       %<'Many>_N ->_R <Int>_R
                                       %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                       ; Sym (N:Action[0])
                                       :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                              (+# ww1_a1OY (+# ww1_X7 (+# sc_s1QX sc_s1R5)))
                              (+# ww2_a1OZ (+# (+# ww2_X4 ww5_a1P8) (+# sc_s1QY sc_s1R6)));
                          Just a_a1ay [Dmd=1L] ->
                            case lvl_s1Oz of
                            { TextBuilder ww_Xi [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xj ww2_Xk ->
                            case a_a1ay `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                              [] ->
                                TextBuilder
                                  ((\ (@s_Xm)
                                      (array_Xn :: MArray s_Xm)
                                      (offset_Xo :: Int)
                                      (s1_Xp :: State# s_Xm) ->
                                      case (((ww_a1OX
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_Xo)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             s1_Xp
                                      of
                                      { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                      let {
                                        offset_s1Pz :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Pz
                                          = case offset_Xo of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_a1OY)
                                            } } in
                                      case (((ww_X2
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_s1Pz)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_a1Pb
                                      of
                                      { (# ipv_Xr, ipv1_Xs [Dmd=A] #) ->
                                      case ((nt_s1PO
                                               @s_Xm
                                               array_Xn
                                               (case offset_s1Pz of { I# x_a1Pf ->
                                                I# (+# x_a1Pf ww1_X3)
                                                }))
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xr
                                      of
                                      { (# ipv_Xu, ipv1_Xv [Dmd=A] #) ->
                                      case (((ww_Xi
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm
                                               array_Xn
                                               (case offset_s1Pz of { I# x_a1Pf ->
                                                I# (+# (+# (+# x_a1Pf ww1_X7) sc_s1QX) sc_s1R5)
                                                }))
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xu
                                      of
                                      { (# ipv_XD, ipv1_XE [Dmd=A] #) ->
                                      (# ipv_XD, () #)
                                      }
                                      }
                                      }
                                      })
                                   `cast` (forall (s :: <*>_N).
                                           <MArray s>_R
                                           %<'Many>_N ->_R <Int>_R
                                           %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                           ; Sym (N:Action[0])
                                           :: (forall {s}. MArray s -> Int -> STRep s ())
                                              ~R# Action))
                                  (+# ww1_a1OY (+# ww1_X7 (+# sc_s1QX (+# sc_s1R5 ww1_Xj))))
                                  (+#
                                     ww2_a1OZ
                                     (+# (+# ww2_X4 ww5_a1P8) (+# sc_s1QY (+# sc_s1R6 ww2_Xk))));
                              : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                                case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                                case lvl_s1Ov of wild_00 { }
                                }
                            }
                            }
                        } } in
                  join {
                    $j_s1PG :: Action -> Int# -> Int# -> TextBuilder
                    [LclId[JoinId(3)(Nothing)],
                     Arity=3,
                     Str=<1CL(C1(C1(P(L,A))))><L><L> {d1Dn->1L},
                     Cpr=1,
                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                             WorkFree=True, Expandable=True, Guidance=NEVER},
                     RULES: "SC:$j0"
                                forall (sc_s1R6 :: Int#)
                                       (sc_s1R5 :: Int#)
                                       (sg_s1R4
                                          :: (forall {s}.
                                              MArray s -> Int -> State# s -> (# State# s, () #))
                                             ~R# Action).
                                  $j_s1PG ($fMonoidBuilder3
                                           `cast` (sg_s1QW
                                                   :: (forall {s}.
                                                       MArray s
                                                       -> Int -> State# s -> (# State# s, () #))
                                                      ~R# Action))
                                          sc_s1R5
                                          sc_s1R6
                                  = jump $s$j_s1Ra
                                      sc_s1R6
                                      sc_s1R5
                                      @~(sg_s1R4
                                         :: (forall {s}.
                                             MArray s -> Int -> State# s -> (# State# s, () #))
                                            ~R# Action)]
                    $j_s1PG (ww_Xc [Dmd=1CL(C1(C1(P(L,A)))), OS=OneShot] :: Action)
                            (ww1_Xd [OS=OneShot] :: Int#)
                            (ww2_Xe [OS=OneShot] :: Int#)
                      = case ww_Xc
                             `cast` (N:Action[0]
                                     :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
                        of nt_s1PQ [Dmd=LCL(CS(CS(!P(L,A))))]
                        { __DEFAULT ->
                        let {
                          ww1_X7 :: Int#
                          [LclId,
                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                   WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                          ww1_X7 = +# ww1_X3 ww4_a1P7 } in
                        case ds_d1Dn of {
                          Nothing ->
                            TextBuilder
                              ((\ (@s_a1P2)
                                  (array_a1P3 :: MArray s_a1P2)
                                  (offset_a1P4 :: Int)
                                  (s1_a1P5 :: State# s_a1P2) ->
                                  case (((ww_a1OX
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_a1P4)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         s1_a1P5
                                  of
                                  { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                  let {
                                    offset_s1Pz :: Int
                                    [LclId,
                                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                             ConLike=False, WorkFree=False, Expandable=False,
                                             Guidance=IF_ARGS [] 21 10}]
                                    offset_s1Pz
                                      = case offset_a1P4 of { I# x_a1Pf ->
                                        I# (+# x_a1Pf ww1_a1OY)
                                        } } in
                                  case (((ww_X2
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_a1P2 array_a1P3 offset_s1Pz)
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_a1Pb
                                  of
                                  { (# ipv_Xl, ipv1_Xm [Dmd=A] #) ->
                                  case ((nt_s1PO
                                           @s_a1P2
                                           array_a1P3
                                           (case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X3)
                                            }))
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_Xl
                                  of
                                  { (# ipv_Xo, ipv1_Xp [Dmd=A] #) ->
                                  case ((nt_s1PQ
                                           @s_a1P2
                                           array_a1P3
                                           (case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# (+# x_a1Pf ww1_X7) sc_s1QX)
                                            }))
                                        `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                                :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                         ipv_Xo
                                  of
                                  { (# ipv_Xu, ipv1_Xv [Dmd=A] #) ->
                                  (# ipv_Xu, () #)
                                  }
                                  }
                                  }
                                  })
                               `cast` (forall (s :: <*>_N).
                                       <MArray s>_R
                                       %<'Many>_N ->_R <Int>_R
                                       %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                       ; Sym (N:Action[0])
                                       :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                              (+# ww1_a1OY (+# ww1_X7 (+# sc_s1QX ww1_Xd)))
                              (+# ww2_a1OZ (+# (+# ww2_X4 ww5_a1P8) (+# sc_s1QY ww2_Xe)));
                          Just a_a1ay [Dmd=1L] ->
                            case lvl_s1Oz of
                            { TextBuilder ww_Xi [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xj ww2_Xk ->
                            case a_a1ay `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                              [] ->
                                TextBuilder
                                  ((\ (@s_Xm)
                                      (array_Xn :: MArray s_Xm)
                                      (offset_Xo :: Int)
                                      (s1_Xp :: State# s_Xm) ->
                                      case (((ww_a1OX
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_Xo)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             s1_Xp
                                      of
                                      { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                      let {
                                        offset_s1Pz :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Pz
                                          = case offset_Xo of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_a1OY)
                                            } } in
                                      case (((ww_X2
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm array_Xn offset_s1Pz)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_a1Pb
                                      of
                                      { (# ipv_Xr, ipv1_Xs [Dmd=A] #) ->
                                      case ((nt_s1PO
                                               @s_Xm
                                               array_Xn
                                               (case offset_s1Pz of { I# x_a1Pf ->
                                                I# (+# x_a1Pf ww1_X3)
                                                }))
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xr
                                      of
                                      { (# ipv_Xu, ipv1_Xv [Dmd=A] #) ->
                                      let {
                                        offset_s1Pv :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 22 10}]
                                        offset_s1Pv
                                          = case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# (+# x_a1Pf ww1_X7) sc_s1QX)
                                            } } in
                                      case ((nt_s1PQ @s_Xm array_Xn offset_s1Pv)
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_Xu
                                      of
                                      { (# ipv_XA, ipv1_XB [Dmd=A] #) ->
                                      case (((ww_Xi
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xm
                                               array_Xn
                                               (case offset_s1Pv of { I# x_a1Pf ->
                                                I# (+# x_a1Pf ww1_Xd)
                                                }))
                                            `cast` (N:ST[0] <s_Xm>_N <()>_R
                                                    :: ST s_Xm () ~R# STRep s_Xm ()))
                                             ipv_XA
                                      of
                                      { (# ipv_XD, ipv1_XE [Dmd=A] #) ->
                                      (# ipv_XD, () #)
                                      }
                                      }
                                      }
                                      }
                                      })
                                   `cast` (forall (s :: <*>_N).
                                           <MArray s>_R
                                           %<'Many>_N ->_R <Int>_R
                                           %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                           ; Sym (N:Action[0])
                                           :: (forall {s}. MArray s -> Int -> STRep s ())
                                              ~R# Action))
                                  (+# ww1_a1OY (+# ww1_X7 (+# sc_s1QX (+# ww1_Xd ww1_Xj))))
                                  (+#
                                     ww2_a1OZ
                                     (+# (+# ww2_X4 ww5_a1P8) (+# sc_s1QY (+# ww2_Xe ww2_Xk))));
                              : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                                case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                                case lvl_s1Ov of wild_00 { }
                                }
                            }
                            }
                        }
                        } } in
                  case ds_d1Dm of {
                    Nothing ->
                      jump $j_s1PG
                        ($fMonoidBuilder3
                         `cast` (sg_s1QW
                                 :: (forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                                    ~R# Action))
                        0#
                        0#;
                    Just a_a1ay [Dmd=A] ->
                      case lvl_s1Ok of
                      { TextBuilder ww_Xe [Dmd=1CL(C1(C1(P(L,A))))] ww1_Xf ww2_Xg ->
                      jump $j_s1PG ww_Xe ww1_Xf ww2_Xg
                      }
                  } } in
            case ds_d1Dl of {
              Nothing ->
                jump $s$j_s1R3
                  0#
                  0#
                  @~(forall (s :: <*>_N).
                     <MArray s>_R
                     %<'Many>_N ->_R <Int>_R
                     %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                     ; Sym (N:Action[0])
                     :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action);
              Just a_a1ay [Dmd=1L] ->
                case lvl_s1OF of
                { TextBuilder ww_Xa [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xb ww2_Xc ->
                case a_a1ay `cast` (N:FromExp[0] :: FromExp ~R# [FromItem]) of {
                  [] ->
                    let {
                      nt_s1RV :: forall {s}. MArray s -> Int -> STRep s ()
                      [LclId,
                       Arity=3,
                       Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                               WorkFree=True, Expandable=True, Guidance=IF_ARGS [0 0 0] 50 10}]
                      nt_s1RV
                        = \ (@s_a1P2)
                            (array_a1P3 :: MArray s_a1P2)
                            (offset_a1P4 [OS=OneShot] :: Int)
                            (s1_a1P5 [OS=OneShot] :: State# s_a1P2) ->
                            case (((ww_Xa
                                    `cast` (N:Action[0]
                                            :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                                     @s_a1P2 array_a1P3 offset_a1P4)
                                  `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                          :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                                   s1_a1P5
                            of
                            { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                            (# ipv_a1Pb, () #)
                            } } in
                    join {
                      $s$j_s1QV
                        :: Int#
                           -> Int#
                           -> ((forall {s}. MArray s -> Int -> State# s -> (# State# s, () #))
                               ~R# Action)
                           -> TextBuilder
                      [LclId[JoinId(3)(Just [])],
                       Arity=3,
                       Str=<L><L><L>,
                       Unf=Unf{Src=<vanilla>, TopLvl=False, Value=True, ConLike=True,
                               WorkFree=True, Expandable=True, Guidance=NEVER}]
                      $s$j_s1QV (sc_s1QR [OS=OneShot] :: Int#)
                                (sc_s1QQ [OS=OneShot] :: Int#)
                                (sg_s1QP
                                   :: (forall {s}.
                                       MArray s -> Int -> State# s -> (# State# s, () #))
                                      ~R# Action)
                        = let {
                            ww1_X7 :: Int#
                            [LclId,
                             Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                     WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                            ww1_X7 = +# ww1_X3 ww4_a1P7 } in
                          case ds_d1Dn of {
                            Nothing ->
                              TextBuilder
                                ((\ (@s_Xh)
                                    (array_Xi :: MArray s_Xh)
                                    (offset_Xj :: Int)
                                    (s1_Xk :: State# s_Xh) ->
                                    case (((ww_a1OX
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xh array_Xi offset_Xj)
                                          `cast` (N:ST[0] <s_Xh>_N <()>_R
                                                  :: ST s_Xh () ~R# STRep s_Xh ()))
                                           s1_Xk
                                    of
                                    { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                    let {
                                      offset_s1Pz :: Int
                                      [LclId,
                                       Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                               ConLike=False, WorkFree=False, Expandable=False,
                                               Guidance=IF_ARGS [] 21 10}]
                                      offset_s1Pz
                                        = case offset_Xj of { I# x_a1Pf ->
                                          I# (+# x_a1Pf ww1_a1OY)
                                          } } in
                                    case (((ww_X2
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xh array_Xi offset_s1Pz)
                                          `cast` (N:ST[0] <s_Xh>_N <()>_R
                                                  :: ST s_Xh () ~R# STRep s_Xh ()))
                                           ipv_a1Pb
                                    of
                                    { (# ipv_Xm, ipv1_Xn [Dmd=A] #) ->
                                    case ((nt_s1PO
                                             @s_Xh
                                             array_Xi
                                             (case offset_s1Pz of { I# x_a1Pf ->
                                              I# (+# x_a1Pf ww1_X3)
                                              }))
                                          `cast` (N:ST[0] <s_Xh>_N <()>_R
                                                  :: ST s_Xh () ~R# STRep s_Xh ()))
                                           ipv_Xm
                                    of
                                    { (# ipv_Xp, ipv1_Xq [Dmd=A] #) ->
                                    case (((ww_Xa
                                            `cast` (N:Action[0]
                                                    :: Action
                                                       ~R# (forall s. MArray s -> Int -> ST s ())))
                                             @s_Xh
                                             array_Xi
                                             (case offset_s1Pz of { I# x_a1Pf ->
                                              I# (+# x_a1Pf ww1_X7)
                                              }))
                                          `cast` (N:ST[0] <s_Xh>_N <()>_R
                                                  :: ST s_Xh () ~R# STRep s_Xh ()))
                                           ipv_Xp
                                    of
                                    { (# ipv_Xs, ipv1_Xt [Dmd=A] #) ->
                                    (# ipv_Xs, () #)
                                    }
                                    }
                                    }
                                    })
                                 `cast` (forall (s :: <*>_N).
                                         <MArray s>_R
                                         %<'Many>_N ->_R <Int>_R
                                         %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                         ; Sym (N:Action[0])
                                         :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                                (+# ww1_a1OY (+# ww1_X7 (+# ww1_Xb sc_s1QQ)))
                                (+# ww2_a1OZ (+# (+# ww2_X4 ww5_a1P8) (+# ww2_Xc sc_s1QR)));
                            Just a_Xh [Dmd=1L] ->
                              case lvl_s1Oz of
                              { TextBuilder ww_Xj [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xk ww2_Xl ->
                              case a_Xh `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                                [] ->
                                  TextBuilder
                                    ((\ (@s_Xn)
                                        (array_Xo :: MArray s_Xn)
                                        (offset_Xp :: Int)
                                        (s1_Xq :: State# s_Xn) ->
                                        case (((ww_a1OX
                                                `cast` (N:Action[0]
                                                        :: Action
                                                           ~R# (forall s.
                                                                MArray s -> Int -> ST s ())))
                                                 @s_Xn array_Xo offset_Xp)
                                              `cast` (N:ST[0] <s_Xn>_N <()>_R
                                                      :: ST s_Xn () ~R# STRep s_Xn ()))
                                               s1_Xq
                                        of
                                        { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                        let {
                                          offset_s1Pz :: Int
                                          [LclId,
                                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                   ConLike=False, WorkFree=False, Expandable=False,
                                                   Guidance=IF_ARGS [] 21 10}]
                                          offset_s1Pz
                                            = case offset_Xp of { I# x_a1Pf ->
                                              I# (+# x_a1Pf ww1_a1OY)
                                              } } in
                                        case (((ww_X2
                                                `cast` (N:Action[0]
                                                        :: Action
                                                           ~R# (forall s.
                                                                MArray s -> Int -> ST s ())))
                                                 @s_Xn array_Xo offset_s1Pz)
                                              `cast` (N:ST[0] <s_Xn>_N <()>_R
                                                      :: ST s_Xn () ~R# STRep s_Xn ()))
                                               ipv_a1Pb
                                        of
                                        { (# ipv_Xs, ipv1_Xt [Dmd=A] #) ->
                                        case ((nt_s1PO
                                                 @s_Xn
                                                 array_Xo
                                                 (case offset_s1Pz of { I# x_a1Pf ->
                                                  I# (+# x_a1Pf ww1_X3)
                                                  }))
                                              `cast` (N:ST[0] <s_Xn>_N <()>_R
                                                      :: ST s_Xn () ~R# STRep s_Xn ()))
                                               ipv_Xs
                                        of
                                        { (# ipv_Xv, ipv1_Xw [Dmd=A] #) ->
                                        let {
                                          offset_s1Px :: Int
                                          [LclId,
                                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                   ConLike=False, WorkFree=False, Expandable=False,
                                                   Guidance=IF_ARGS [] 21 10}]
                                          offset_s1Px
                                            = case offset_s1Pz of { I# x_a1Pf ->
                                              I# (+# x_a1Pf ww1_X7)
                                              } } in
                                        case (((ww_Xa
                                                `cast` (N:Action[0]
                                                        :: Action
                                                           ~R# (forall s.
                                                                MArray s -> Int -> ST s ())))
                                                 @s_Xn array_Xo offset_s1Px)
                                              `cast` (N:ST[0] <s_Xn>_N <()>_R
                                                      :: ST s_Xn () ~R# STRep s_Xn ()))
                                               ipv_Xv
                                        of
                                        { (# ipv_Xy, ipv1_Xz [Dmd=A] #) ->
                                        case (((ww_Xj
                                                `cast` (N:Action[0]
                                                        :: Action
                                                           ~R# (forall s.
                                                                MArray s -> Int -> ST s ())))
                                                 @s_Xn
                                                 array_Xo
                                                 (case offset_s1Px of { I# x_a1Pf ->
                                                  I# (+# (+# x_a1Pf ww1_Xb) sc_s1QQ)
                                                  }))
                                              `cast` (N:ST[0] <s_Xn>_N <()>_R
                                                      :: ST s_Xn () ~R# STRep s_Xn ()))
                                               ipv_Xy
                                        of
                                        { (# ipv_XD, ipv1_XE [Dmd=A] #) ->
                                        (# ipv_XD, () #)
                                        }
                                        }
                                        }
                                        }
                                        })
                                     `cast` (forall (s :: <*>_N).
                                             <MArray s>_R
                                             %<'Many>_N ->_R <Int>_R
                                             %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                             ; Sym (N:Action[0])
                                             :: (forall {s}. MArray s -> Int -> STRep s ())
                                                ~R# Action))
                                    (+# ww1_a1OY (+# ww1_X7 (+# ww1_Xb (+# sc_s1QQ ww1_Xk))))
                                    (+#
                                       ww2_a1OZ
                                       (+# (+# ww2_X4 ww5_a1P8) (+# ww2_Xc (+# sc_s1QR ww2_Xl))));
                                : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                                  case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                                  case lvl_s1Ov of wild_00 { }
                                  }
                              }
                              }
                          } } in
                    case ds_d1Dm of {
                      Nothing ->
                        jump $s$j_s1QV
                          0#
                          0#
                          @~(forall (s :: <*>_N).
                             <MArray s>_R
                             %<'Many>_N ->_R <Int>_R
                             %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                             ; Sym (N:Action[0])
                             :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action);
                      Just a_Xf [Dmd=A] ->
                        case lvl_s1Ok of
                        { TextBuilder ww_Xh [Dmd=1CL(C1(C1(P(L,A))))] ww1_Xi ww2_Xj ->
                        let {
                          ww1_X7 :: Int#
                          [LclId,
                           Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False,
                                   WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 1 0}]
                          ww1_X7 = +# ww1_X3 ww4_a1P7 } in
                        case ds_d1Dn of {
                          Nothing ->
                            TextBuilder
                              ((\ (@s_Xl)
                                  (array_Xm :: MArray s_Xl)
                                  (offset_Xn :: Int)
                                  (s1_Xo :: State# s_Xl) ->
                                  case (((ww_a1OX
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_Xl array_Xm offset_Xn)
                                        `cast` (N:ST[0] <s_Xl>_N <()>_R
                                                :: ST s_Xl () ~R# STRep s_Xl ()))
                                         s1_Xo
                                  of
                                  { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                  let {
                                    offset_s1Pz :: Int
                                    [LclId,
                                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                             ConLike=False, WorkFree=False, Expandable=False,
                                             Guidance=IF_ARGS [] 21 10}]
                                    offset_s1Pz
                                      = case offset_Xn of { I# x_a1Pf ->
                                        I# (+# x_a1Pf ww1_a1OY)
                                        } } in
                                  case (((ww_X2
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_Xl array_Xm offset_s1Pz)
                                        `cast` (N:ST[0] <s_Xl>_N <()>_R
                                                :: ST s_Xl () ~R# STRep s_Xl ()))
                                         ipv_a1Pb
                                  of
                                  { (# ipv_Xq, ipv1_Xr [Dmd=A] #) ->
                                  case ((nt_s1PO
                                           @s_Xl
                                           array_Xm
                                           (case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X3)
                                            }))
                                        `cast` (N:ST[0] <s_Xl>_N <()>_R
                                                :: ST s_Xl () ~R# STRep s_Xl ()))
                                         ipv_Xq
                                  of
                                  { (# ipv_Xt, ipv1_Xu [Dmd=A] #) ->
                                  let {
                                    offset_s1Px :: Int
                                    [LclId,
                                     Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                             ConLike=False, WorkFree=False, Expandable=False,
                                             Guidance=IF_ARGS [] 21 10}]
                                    offset_s1Px
                                      = case offset_s1Pz of { I# x_a1Pf ->
                                        I# (+# x_a1Pf ww1_X7)
                                        } } in
                                  case (((ww_Xa
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_Xl array_Xm offset_s1Px)
                                        `cast` (N:ST[0] <s_Xl>_N <()>_R
                                                :: ST s_Xl () ~R# STRep s_Xl ()))
                                         ipv_Xt
                                  of
                                  { (# ipv_Xw, ipv1_Xx [Dmd=A] #) ->
                                  case (((ww_Xh
                                          `cast` (N:Action[0]
                                                  :: Action
                                                     ~R# (forall s. MArray s -> Int -> ST s ())))
                                           @s_Xl
                                           array_Xm
                                           (case offset_s1Px of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_Xb)
                                            }))
                                        `cast` (N:ST[0] <s_Xl>_N <()>_R
                                                :: ST s_Xl () ~R# STRep s_Xl ()))
                                         ipv_Xw
                                  of
                                  { (# ipv_Xz, ipv1_XA [Dmd=A] #) ->
                                  (# ipv_Xz, () #)
                                  }
                                  }
                                  }
                                  }
                                  })
                               `cast` (forall (s :: <*>_N).
                                       <MArray s>_R
                                       %<'Many>_N ->_R <Int>_R
                                       %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                       ; Sym (N:Action[0])
                                       :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                              (+# ww1_a1OY (+# ww1_X7 (+# ww1_Xb ww1_Xi)))
                              (+# ww2_a1OZ (+# (+# ww2_X4 ww5_a1P8) (+# ww2_Xc ww2_Xj)));
                          Just a_Xl [Dmd=1L] ->
                            case lvl_s1Oz of
                            { TextBuilder ww_Xn [Dmd=LCL(C1(C1(!P(L,A))))] ww1_Xo ww2_Xp ->
                            case a_Xl `cast` (N:RetExp[0] :: RetExp ~R# [Extractor]) of {
                              [] ->
                                TextBuilder
                                  ((\ (@s_Xr)
                                      (array_Xs :: MArray s_Xr)
                                      (offset_Xt :: Int)
                                      (s1_Xu :: State# s_Xr) ->
                                      case (((ww_a1OX
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xr array_Xs offset_Xt)
                                            `cast` (N:ST[0] <s_Xr>_N <()>_R
                                                    :: ST s_Xr () ~R# STRep s_Xr ()))
                                             s1_Xu
                                      of
                                      { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                                      let {
                                        offset_s1Pz :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Pz
                                          = case offset_Xt of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_a1OY)
                                            } } in
                                      case (((ww_X2
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xr array_Xs offset_s1Pz)
                                            `cast` (N:ST[0] <s_Xr>_N <()>_R
                                                    :: ST s_Xr () ~R# STRep s_Xr ()))
                                             ipv_a1Pb
                                      of
                                      { (# ipv_Xw, ipv1_Xx [Dmd=A] #) ->
                                      case ((nt_s1PO
                                               @s_Xr
                                               array_Xs
                                               (case offset_s1Pz of { I# x_a1Pf ->
                                                I# (+# x_a1Pf ww1_X3)
                                                }))
                                            `cast` (N:ST[0] <s_Xr>_N <()>_R
                                                    :: ST s_Xr () ~R# STRep s_Xr ()))
                                             ipv_Xw
                                      of
                                      { (# ipv_Xz, ipv1_XA [Dmd=A] #) ->
                                      let {
                                        offset_s1Px :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Px
                                          = case offset_s1Pz of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_X7)
                                            } } in
                                      case (((ww_Xa
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xr array_Xs offset_s1Px)
                                            `cast` (N:ST[0] <s_Xr>_N <()>_R
                                                    :: ST s_Xr () ~R# STRep s_Xr ()))
                                             ipv_Xz
                                      of
                                      { (# ipv_XC, ipv1_XD [Dmd=A] #) ->
                                      let {
                                        offset_s1Pv :: Int
                                        [LclId,
                                         Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False,
                                                 ConLike=False, WorkFree=False, Expandable=False,
                                                 Guidance=IF_ARGS [] 21 10}]
                                        offset_s1Pv
                                          = case offset_s1Px of { I# x_a1Pf ->
                                            I# (+# x_a1Pf ww1_Xb)
                                            } } in
                                      case (((ww_Xh
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xr array_Xs offset_s1Pv)
                                            `cast` (N:ST[0] <s_Xr>_N <()>_R
                                                    :: ST s_Xr () ~R# STRep s_Xr ()))
                                             ipv_XC
                                      of
                                      { (# ipv_XF, ipv1_XG [Dmd=A] #) ->
                                      case (((ww_Xn
                                              `cast` (N:Action[0]
                                                      :: Action
                                                         ~R# (forall s.
                                                              MArray s -> Int -> ST s ())))
                                               @s_Xr
                                               array_Xs
                                               (case offset_s1Pv of { I# x_a1Pf ->
                                                I# (+# x_a1Pf ww1_Xi)
                                                }))
                                            `cast` (N:ST[0] <s_Xr>_N <()>_R
                                                    :: ST s_Xr () ~R# STRep s_Xr ()))
                                             ipv_XF
                                      of
                                      { (# ipv_XI, ipv1_XJ [Dmd=A] #) ->
                                      (# ipv_XI, () #)
                                      }
                                      }
                                      }
                                      }
                                      }
                                      })
                                   `cast` (forall (s :: <*>_N).
                                           <MArray s>_R
                                           %<'Many>_N ->_R <Int>_R
                                           %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                                           ; Sym (N:Action[0])
                                           :: (forall {s}. MArray s -> Int -> STRep s ())
                                              ~R# Action))
                                  (+# ww1_a1OY (+# ww1_X7 (+# ww1_Xb (+# ww1_Xi ww1_Xo))))
                                  (+#
                                     ww2_a1OZ
                                     (+# (+# ww2_X4 ww5_a1P8) (+# ww2_Xc (+# ww2_Xj ww2_Xp))));
                              : x_a1bk [Dmd=1!B] xs_a1bl [Dmd=B] ->
                                case x_a1bk of { Extractor bx_d1DZ [Dmd=B] ->
                                case lvl_s1Ov of wild_00 { }
                                }
                            }
                            }
                        }
                        }
                    };
                  : x_a1bk [Dmd=B] xs_a1bl [Dmd=B] ->
                    case $ctoSQL_a1ov of wild_00 { }
                }
                }
            }
            } } in
      case ds_d1Dk `cast` (N:SetExp[0] :: SetExp ~R# [SetExpItem]) of {
        [] ->
          jump $s$j_s1Rq
            0#
            0#
            @~(forall (s :: <*>_N).
               <MArray s>_R
               %<'Many>_N ->_R <Int>_R
               %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
               ; Sym (N:Action[0])
               :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action);
        : x_a1bk [Dmd=1!P(L)] xs_a1bl [Dmd=1L] ->
          case x_a1bk `cast` (N:SetExpItem[0] :: SetExpItem ~R# SQLExp) of
          { SEPrep bx_d1E0 ->
          case <# bx_d1E0 0# of {
            __DEFAULT ->
              case $wgo1 ($fShow(,)_itos' bx_d1E0 ([] @Char)) of
              { (# ww_a1OP [Dmd=1CL(C1(C1(!P(L,A))))], ww1_a1OQ, ww2_a1OR #) ->
              case ww_a1OP
                   `cast` (N:Action[0]
                           :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
              of nt_s1PS [Dmd=LCL(CS(CS(!P(L,A))))]
              { __DEFAULT ->
              case $wgo2
                     ((go1_s1QB xs_a1bl)
                      `cast` (([N:Builder[0]])_R :: [Builder] ~R# [TextBuilder]))
              of
              { (# ww_a1Pk [Dmd=LCL(C1(C1(P(L,A))))], ww1_a1Pl, ww2_a1Pm #) ->
              jump $j_s1PJ
                ((\ (@s_a1P2)
                    (array_a1P3 :: MArray s_a1P2)
                    (offset_a1P4 [OS=OneShot] :: Int)
                    (s1_a1P5 [OS=OneShot] :: State# s_a1P2) ->
                    case ((nt_s1PS @s_a1P2 array_a1P3 offset_a1P4)
                          `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                  :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                           s1_a1P5
                    of
                    { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                    (((ww_a1Pk
                       `cast` (N:Action[0]
                               :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                        @s_a1P2
                        array_a1P3
                        (case offset_a1P4 of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OQ) }))
                     `cast` (N:ST[0] <s_a1P2>_N <()>_R
                             :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                      ipv_a1Pb
                    })
                 `cast` (forall (s :: <*>_N).
                         <MArray s>_R
                         %<'Many>_N ->_R <Int>_R
                         %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                         ; Sym (N:Action[0])
                         :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                (+# ww1_a1OQ ww1_a1Pl)
                (+# ww2_a1OR ww2_a1Pm)
              }
              }
              };
            1# ->
              case bx_d1E0 of wild_a1Ik {
                __DEFAULT ->
                  case $wgo1
                         (: @Char
                            $fShow(,)9
                            ($fShow(,)_itos' (negateInt# wild_a1Ik) ([] @Char)))
                  of
                  { (# ww_a1OP [Dmd=1CL(C1(C1(!P(L,A))))], ww1_a1OQ, ww2_a1OR #) ->
                  case ww_a1OP
                       `cast` (N:Action[0]
                               :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
                  of nt_s1PS [Dmd=LCL(CS(CS(!P(L,A))))]
                  { __DEFAULT ->
                  case $wgo2
                         ((go1_s1QB xs_a1bl)
                          `cast` (([N:Builder[0]])_R :: [Builder] ~R# [TextBuilder]))
                  of
                  { (# ww_a1Pk [Dmd=LCL(C1(C1(P(L,A))))], ww1_a1Pl, ww2_a1Pm #) ->
                  jump $j_s1PJ
                    ((\ (@s_a1P2)
                        (array_a1P3 :: MArray s_a1P2)
                        (offset_a1P4 [OS=OneShot] :: Int)
                        (s1_a1P5 [OS=OneShot] :: State# s_a1P2) ->
                        case ((nt_s1PS @s_a1P2 array_a1P3 offset_a1P4)
                              `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                      :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                               s1_a1P5
                        of
                        { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                        (((ww_a1Pk
                           `cast` (N:Action[0]
                                   :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                            @s_a1P2
                            array_a1P3
                            (case offset_a1P4 of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OQ) }))
                         `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                 :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                          ipv_a1Pb
                        })
                     `cast` (forall (s :: <*>_N).
                             <MArray s>_R
                             %<'Many>_N ->_R <Int>_R
                             %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                             ; Sym (N:Action[0])
                             :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                    (+# ww1_a1OQ ww1_a1Pl)
                    (+# ww2_a1OR ww2_a1Pm)
                  }
                  }
                  };
                -9223372036854775808# ->
                  case $wgo1 lvl_s1Om of
                  { (# ww_a1OP [Dmd=1CL(C1(C1(!P(L,A))))], ww1_a1OQ, ww2_a1OR #) ->
                  case ww_a1OP
                       `cast` (N:Action[0]
                               :: Action ~R# (forall s. MArray s -> Int -> ST s ()))
                  of nt_s1PS [Dmd=LCL(CS(CS(!P(L,A))))]
                  { __DEFAULT ->
                  case $wgo2
                         ((go1_s1QB xs_a1bl)
                          `cast` (([N:Builder[0]])_R :: [Builder] ~R# [TextBuilder]))
                  of
                  { (# ww_a1Pk [Dmd=LCL(C1(C1(P(L,A))))], ww1_a1Pl, ww2_a1Pm #) ->
                  jump $j_s1PJ
                    ((\ (@s_a1P2)
                        (array_a1P3 :: MArray s_a1P2)
                        (offset_a1P4 [OS=OneShot] :: Int)
                        (s1_a1P5 [OS=OneShot] :: State# s_a1P2) ->
                        case ((nt_s1PS @s_a1P2 array_a1P3 offset_a1P4)
                              `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                      :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                               s1_a1P5
                        of
                        { (# ipv_a1Pb, ipv1_a1Pc [Dmd=A] #) ->
                        (((ww_a1Pk
                           `cast` (N:Action[0]
                                   :: Action ~R# (forall s. MArray s -> Int -> ST s ())))
                            @s_a1P2
                            array_a1P3
                            (case offset_a1P4 of { I# x_a1Pf -> I# (+# x_a1Pf ww1_a1OQ) }))
                         `cast` (N:ST[0] <s_a1P2>_N <()>_R
                                 :: ST s_a1P2 () ~R# STRep s_a1P2 ()))
                          ipv_a1Pb
                        })
                     `cast` (forall (s :: <*>_N).
                             <MArray s>_R
                             %<'Many>_N ->_R <Int>_R
                             %<'Many>_N ->_R Sym (N:ST[0] <s>_N <()>_R)
                             ; Sym (N:Action[0])
                             :: (forall {s}. MArray s -> Int -> STRep s ()) ~R# Action))
                    (+# ww1_a1OQ ww1_a1Pl)
                    (+# ww2_a1OR ww2_a1Pm)
                  }
                  }
                  }
              }
          }
          }
      }
      }
      }
      }

$fToSQLSQLUpdate [InlPrag=INLINE (sat-args=0)] :: ToSQL SQLUpdate
[LclIdX[DFunId(nt)],
 Arity=1,
 Str=<1!P(1L,1L,1L,1L)>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=False,boring_ok=True)
         Tmpl= $ctoSQL_s1Oh
               `cast` (<SQLUpdate>_R %<'Many>_N ->_R Sym (N:Builder[0])
                       ; Sym (N:ToSQL[0] <SQLUpdate>_N)
                       :: (SQLUpdate -> TextBuilder) ~R# ToSQL SQLUpdate)}]
$fToSQLSQLUpdate
  = $ctoSQL_s1Oh
    `cast` (<SQLUpdate>_R %<'Many>_N ->_R Sym (N:Builder[0])
            ; Sym (N:ToSQL[0] <SQLUpdate>_N)
            :: (SQLUpdate -> TextBuilder) ~R# ToSQL SQLUpdate)

*** End of Offense ***


<no location info>: error:
Compilation had errors



<no location info>: error: ExitFailure 1
```
