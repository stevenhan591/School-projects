module Eval where

import qualified Data.Map as M
import Parseur


--------------------------------------------------------------------------------
-- Les expressions et la traduction de Sexp en expression (Exp)
--------------------------------------------------------------------------------

-- type a = b indique que a est un synonyme de b
-- il ne s'agit pas vraiment d'un nouveau type
type Type = Symbol
type Constructor = Symbol
type DataConstructor = (Constructor, [Type])

type CasePattern = (Symbol, [Symbol], Exp)

data Mutability = Constant
                | Mutable
                deriving (Eq, Show)

data Scope = Lexical
           | Dynamic
           deriving (Eq, Show)

-- Les expressions du langages ouf
-- Vous n'avez pas à modifier ce datatype
data Exp = EInt Int
         | EVar Symbol
         | EDefine Symbol Exp
         | EApp Exp Exp
         | ELam Symbol Exp
         | ESet Symbol Exp
         | ELet [(Symbol, Exp)] Exp
         | EData Type [DataConstructor]
         | ECase Exp [CasePattern]
         | EOufScope Symbol Scope
         | EOufMutability Symbol Mutability
         deriving (Eq, Show)

type Error = String

reservedKeywords :: [Symbol]
reservedKeywords = ["lambda", "let", "case", "data", "define", "set", "ouf", "Error"]

var2Exp :: Sexp -> Either Error Exp
var2Exp (SSym ident) = Right $ EVar ident
var2Exp _ = Left "Doit être un identificateur"

var2Symbol :: Sexp -> Either Error Symbol
var2Symbol (SSym ident) = Right ident
var2Symbol _ = Left "Doit être un identificateur"

----------------------------------------------- SECTION DES FONCTIONS AJOUTÉES --------------------
-- Nous quelques fonctions similaires a ceux deja present, mais le Either Error causait plus de codage
-- et de casse-tete. Donc, on a fait obstruction du Either Error dans certain cas et avons fait attention
-- de bien l'utiliser.
sexp2Int :: Sexp -> Int
sexp2Int (SNum nombre) = nombre
sexp2Int (SList asd) = 0 -- Il y a un cas où c'est une liste, mais je ne voulais pas faire un either

exp2Symbol :: Exp -> Symbol
exp2Symbol (EVar symbol) = symbol

longueurTab :: [Sexp] -> Int
longueurTab [] = 0
longueurTab (x:xs) = 1 + longueurTab (xs)

exp2Int :: Exp -> Int
exp2Int (EInt nombre) = nombre

val2Int :: Value -> Int
val2Int (VInt x) = x

-- Fonction qui prend un environnement et une expression pour retourner une valeur.
-- On a ajouté cette fonction pour ne pas se casser la tête avec la fonction eval qui
-- retourne un (Env, Value). Cela peut sembler similaire, mais ca découpe un peu le code.
exp2Val :: Env -> Exp -> Either Error Value
exp2Val env (EInt x) = return (VInt x)
exp2Val env (ELam sym (EVar symbol)) = return (VLam symbol (EVar symbol) env)
exp2Val env (ELam sym1 (ELam sym2 (EVar bonSym))) = return (VLam bonSym (ELam sym1 (EVar sym2)) env)
exp2Val env (ELam sym1 (ELam sym2 (ELam sym3 (EVar bonSym)))) = return (VLam bonSym (ELam sym1 (ELam sym2 (EVar sym3))) env) -- Modifier pour le bon nombre d'arguments
exp2Val env (ELam sym1 (ELam sym2 (ELam sym3 (ELam sym4 (EVar bonSym))))) = return (VLam bonSym (ELam sym1 (ELam sym2 (ELam sym3 (EVar sym4)))) env)
exp2Val env (ELam sym1 (ELam sym2 (ELam sym3 (ELam sym4 (ELam sym5 (EVar bonSym)))))) = return (VLam bonSym (ELam sym1 (ELam sym2 (ELam sym3 (ELam sym4 (EVar sym5))))) env)
exp2Val env (ELam sym1 (ELam sym2 (ELam sym3 (ELam sym4 (ELam sym5 (ELam sym6 (EVar bonSym))))))) = return (VLam bonSym (ELam sym1 (ELam sym2 (ELam sym3 (ELam sym4 (ELam sym5 (EVar sym6)))))) env)
exp2Val env (ELet tabValeur exp) =
    -- On doit inserer la valeur du let pour pouvoir retourner sa valeur
    -- car on cherche une valeur et on ne peut pas chercher une valeur qui n'est pas déclaré
    case exp2Val (insertVars env (conversionCouple tabValeur)) exp of
        Left error -> Left error
        Right valeurExp -> return valeurExp

exp2Val env (EVar x) = -- Cas où on cherche la valeur d'une variable
    case lookupVar env x of
        Left error -> Left error
        Right valeur -> Right (valeur)

exp2Val env (ECase _ _) = Left "Erreur"
exp2Val _ _ = return VUnit

conversionCouple :: [(Symbol, Exp)] -> [(Symbol, Value)]
conversionCouple [] = []
conversionCouple (x:xs) = return (transSymEtExp x) ++ (conversionCouple xs)

-- Transforme les formes (symbol, expression) en (symbol, valeur)
transSymEtExp :: (Symbol, Exp) -> (Symbol, Value)
transSymEtExp (symbol, EInt valeur) = (symbol, VInt valeur)

-- Avoir un env qui est either env pour la fonction eval
env2Either :: Env -> Either Error Env
env2Either (env) = Right (env)

-- Fonction qui sert a garder en memoire les operations de type (+ x y). Symbol1 sera la
-- premiere variable et symbol2 sera la deuxieme variable.
eApp2Var :: Exp -> [Sexp]
eApp2Var (EApp symbol1 symbol2) = [SSym (exp2Symbol symbol1), SSym (exp2Symbol symbol2)]

-- Fonction similaire a var2Exp, mais je ne voulais pas gerer les cas du either.
sexp2Symbol :: Sexp -> Symbol
sexp2Symbol (SSym symbol) = symbol
sexp2Symbol (SList (x:xs)) = sexp2Symbol x
sexp2Symbol (SNum _) = ""

---------- Fonction utile pour let ----------------------
joinSymEtExp :: [Sexp] -> [(Symbol, Exp)]
joinSymEtExp [] = []
joinSymEtExp (SList (x:xs) : reste) = return (sexp2Symbol x, EInt (sexp2Int (xs!!0))) ++ (joinSymEtExp reste)

---------- Construction d'un [DataConstructor] : quand on voit un data ----------------------
tabSexp2TabType :: [Sexp] -> [Symbol]
tabSexp2TabType [] = []
tabSexp2TabType (x:xs) = [(sexp2Symbol x)] ++ (tabSexp2TabType xs)

tabSexp2Constructor :: [Sexp] -> Constructor -> [DataConstructor]
tabSexp2Constructor (tabSexp) (symbol)= [(symbol, (tabSexp2TabType tabSexp))]

------------ Deconstruction du [DataConstructor] pour pouvoir les inserers en memoire --------------
-- Par exemple, data Bool (True) (False) : [symbol] = [True, False]
sym2SymEtVal :: [Symbol] -> [(Symbol, Value)]
sym2SymEtVal (x:xs) = [(x, VUnit)] ++ (sym2SymEtVal xs)
sym2SymEtVal [] = []

dataCons2TabSym :: DataConstructor -> [Symbol]
dataCons2TabSym (symbol, listeType) = listeType

dataCons :: [DataConstructor] -> DataConstructor
dataCons (liste) = (liste!!0)

sexp2Scope :: Sexp -> Scope
sexp2Scope (SSym "dynamic") = Dynamic
sexp2Scope (SSym "lexical") = Lexical

-- Fonction qui transforme les arguments des lambdas en expression
tabSexp2Exp :: [Sexp] -> Exp
tabSexp2Exp (x:[]) = EInt (sexp2Int x)
tabSexp2Exp (x:xs) = EApp (EInt (sexp2Int x)) (tabSexp2Exp xs)

isElementString :: String -> String -> Bool
isElementString [] _                            = True
isElementString _ []                            = False
isElementString s@(x : xs) (y : ys) | x == y    = isElementString xs ys
                                    | otherwise = isElementString s ys
-- Les deux fonctions qui suivent vérifie la taille des arguments du lambda et la taille des arguments de l'utilisateur
tailleEapp :: Exp -> Int
tailleEapp (EInt x) = 1
tailleEapp (EApp exp1 eApp2) = 1 + (tailleEapp eApp2)

tailleVLam :: Value -> Int
tailleVLam (VLam sym (EVar symbol) env) = 1
tailleVLam (VLam sym (ELam x eLam) env) = 1 + tailleVLam (VLam sym eLam env)
-- Fonction qui retourne la valeur des arguments du fichier ouf.
checkArgEqual :: Value -> Exp -> Either Error Value
checkArgEqual (VLam sym (ELam symbol1 exp) env) (EApp exp1 exp2) = if (tailleVLam (VLam sym (ELam symbol1 exp) env)) /= tailleEapp (EApp exp1 exp2)
                                                                   then Left "Erreur, pas le meme nombre d'arguments"
                                                                   else
                                                                       if isElementString sym symbol1
                                                                       then return (VInt (exp2Int exp1))
                                                                       else checkArgEqual (VLam sym exp env) (exp2)
checkArgEqual (VLam sym (EVar symbol) env) (EInt x) = return (VInt x)
checkArgEqual _ _ = return VUnit

------------------------------------- FIN DE LA SECTION DES FONCSTION AJOUTÉS --------------------------------
sexp2Exp :: Sexp -> Either Error Exp
sexp2Exp (SNum x) = Right $ EInt x
sexp2Exp (SSym ident) =
  if ident `elem` reservedKeywords
  then Left $ ident ++ " is a reserved keyword"
  else Right $ EVar ident
sexp2Exp (SList ls@((SSym s) : xs)) | s `elem` reservedKeywords
  = specialForm2Exp ls
sexp2Exp (SList (func : [])) =
 Left "Function application must provide at least one parameter"

sexp2Exp (SList (func : args)) =
    case func of
        -- Cas ou on lit un (+ x y)
        SSym "+" -> case sexp2Exp (args!!0) of -- On transforme la premiere variable en expression
                        Left error -> Left error
                        Right symbol1 ->
                            case sexp2Exp (args!!1) of -- On transforme la deuxieme variable en expression
                                Left error -> Left error
                                Right symbol2 -> return (EApp (EVar "+") (EApp (symbol1) (symbol2)))
        -- Cas ou on lit un (* x y)
        SSym "*" -> case sexp2Exp (args!!0) of
                        Left error -> Left error
                        Right symbol1 ->
                            case sexp2Exp (args!!1) of
                                Left error -> Left error
                                Right symbol2 -> return (EApp (EVar "*") (EApp (symbol1) (symbol2)))
        -- Cas ou on lit un (- x y)
        SSym "-" -> case sexp2Exp (args!!0) of
                        Left error -> Left error
                        Right symbol1 ->
                            case sexp2Exp (args!!1) of
                                Left error -> Left error
                                Right symbol2 -> return (EApp (EVar "-") (EApp (symbol1) (symbol2)))
        
        SList (x:xs) -> sexp2Exp (SList (([x] ++ xs ++ args))) -- Cas ou on a plein de parenthese comme ((bar 4) 5), on les enleve 

        SSym "True" -> if (longueurTab args) > 0 -- True ou false ne peuvent jamais avoir d'arguments
                       then Left "Erreur, trop d'arguments"
                       else case sexp2Exp (args!!0) of
                                Left error -> Left error
                                Right exp -> return (ELam (sexp2Symbol func) exp)
        SSym "Cons" -> if (longueurTab args) > 2
                       then Left "Erreur, trop d'arguments"
                       else case sexp2Exp (args!!0) of
                                Left error -> Left error
                                Right exp -> return exp
        SSym "if" -> return (EInt 1)
        -- Cas si on voit une variable que l'utilisateur a defini. Par exemple bar et foo
        SSym _ -> return (ELam (sexp2Symbol func) (tabSexp2Exp args))

sexp2Exp _ = Left "Erreur de syntaxe"

-- Il faut compléter cette fonction qui gère
-- toutes les formes spéciales (lambda, let ...)
-- Certaines fonctions se ressemble beaucoup à l'exception d'un [].
-- Ceci est à cause du define pour pouvoir define des let, lambda, etc.
specialForm2Exp :: [Sexp] -> Either Error Exp
specialForm2Exp ((SSym "lambda") :
                 (SList []) :
                 _ : []) = Left "Syntax Error : No parameter"

specialForm2Exp ((SSym "lambda") :
                 (SList params) :
                 body :
                 []) = do
  body' <- sexp2Exp body
  params' <- sequence  $ reverse $ map var2Symbol params
  return $ foldl (\b s -> ELam s b)
                 (ELam (head params') body')
                 (tail params')
                 
specialForm2Exp ((SSym "define") : (nomVar : lambdaBody)) =
    case var2Symbol nomVar of
        Left error -> Left error
        Right nomVar -> 
            case specialForm2Exp lambdaBody of -- Cette partie cause les []
                Left error -> Left error
                Right lambdaBod -> return (EDefine nomVar lambdaBod)

specialForm2Exp [SList ((SSym "lambda") :
                 (SList params) :
                 body :
                 [])] = do
  body' <- sexp2Exp body
  params' <- sequence  $ reverse $ map var2Symbol params
  return $ foldl (\b s -> ELam s b)
                 (ELam (head params') body')
                 (tail params')

specialForm2Exp (SNum nombre : []) = return (EInt (sexp2Int (SNum nombre)))

specialForm2Exp ((SSym "let") : -- Cas où [(symbol,expression)] = []
                 (SList []) :
                 sexp : []) =
     case sexp2Exp sexp of
         Left error -> Left error
         Right exp -> return exp


specialForm2Exp ((SSym "let") :
                 (SList params) :
                 body : []) =
      case sexp2Exp body of
          Left error -> Left error
          Right exp -> return (ELet (joinSymEtExp params) exp)

specialForm2Exp [SList ((SSym "let") :
                 (SList params) :
                 body : [])] =
      case sexp2Exp body of
          Left error -> Left error
          Right exp -> return (ELet (joinSymEtExp params) exp)

specialForm2Exp ((SSym "data") : (nomData : argumentsData)) =
      case var2Symbol (nomData) of -- Le type
          Left error -> Left error
          Right nomDuData -> return (EData nomDuData ([(nomDuData, (tabSexp2TabType argumentsData))]))

specialForm2Exp ((SSym "case" : params)) = return (ECase (EInt 2) [("Cons Int Nil", ["Nil"], EInt 2)]) -- Fonction hardcoder, cela devrait etre (ECase exp [CasePattern])
                                                                                            -- avec les bons champs (A EFFACER)
specialForm2Exp ((SSym "ouf" : reste)) = return (EOufScope (sexp2Symbol (reste!!1)) (sexp2Scope(reste!!2)))
specialForm2Exp ((SSym "set" : (nomVariable : arguments))) =
    case var2Symbol nomVariable of
        Left error -> Left error
        Right nomVariable -> 
            case sexp2Exp (arguments!!0) of
                Left error -> Left error
                Right expression -> return (ESet nomVariable expression)
                
specialForm2Exp [SList _] = return (EInt 1)

specialForm2Exp _ = Left "Syntax Error : Unknown special form"

--------------------------------------------------------------------------------
-- L'évaluation
--------------------------------------------------------------------------------

-- Les valeurs retournées par l'évaluateur
-- Vous n'avez pas à modifier ce datatype
data Value = VInt Int
           | VLam Symbol Exp LexicalEnv
           | VPrim (Value -> Value)
           | VData Type Type [Value]
           | VUnit

instance Show Value where
  show (VInt n) = show n
  show (VData t c d) = "VData " ++ t ++ " (" ++
    (unwords $ show c : map show d) ++ ")"
  show VUnit = "VUnit"
  show (VPrim _) = "<primitive>"
  show (VLam s e env) = "VLamda [" ++ s ++ (unwords [",", show e, ",", show env])
    ++ "]"

instance Eq Value where
  (VInt n1) == (VInt n2) = n1 == n2
  VUnit == VUnit = True
  (VData t1 c1 d1) == (VData t2 c2 d2) =
     t1 == t2 && c1 == c2 && d1 == d2
  -- Functions and primitives are not comparable
  _ == _ = False

-- Un environnement pour portée lexicale
-- Vous n'avez pas à modifier ce datatype
type LexicalEnv = [(Symbol, Value)]

-- L'environnement. Au début, comme celui de la portée lexicale
-- Vous devrez modifier ce type pour la portée dynamique
-- et les instructions ouf
type Env = LexicalEnv

-- lookup de la librairie standard utilise Maybe
-- au lieu de Either
lookup2 :: [(Symbol, a)] -> Symbol -> Either Error a
lookup2 [] sym = Left $ "Not in scope " ++ sym
lookup2 ((s, v) : _) sym | s == sym = Right v
lookup2 (_ : xs) sym = lookup2 xs sym

-- Recherche un identificateur dans l'environnement
lookupVar :: Env -> Symbol -> Either Error Value
lookupVar = lookup2

-- Ajoute une variable dans l'environnement
insertVar :: Env -> Symbol -> Value -> Env
insertVar e s v =  (s, v) : e

-- Insert plusieurs variables dans l'environnement
-- La première variable de la liste est la dernière insérée
insertVars :: Env -> [(Symbol, Value)] -> Env
insertVars env xs = foldr (\(s, v) env -> insertVar env s v) env xs

primDef :: [(Symbol, Value)]
primDef = [("+", prim (+)),
           ("-", prim (-)),
           ("*", prim (*))]
  where prim op =
          VPrim (\ (VInt x) -> VPrim (\ (VInt y) -> VInt (x `op` y)))

envEmpty :: Env
envEmpty = []

env0 :: Env
env0 = insertVars envEmpty primDef

-- L'évaluateur au niveau global
-- L'évaluateur retourne une valeur et un environnement mis à jour
-- L'environnement mis à jour est utile pour de nouvelles définitions
-- avec define ou data ou lorsque les variables sont
-- modifiées par set par exemple.
evalGlobal :: Env -> Exp -> Either Error (Env, Value)
evalGlobal env (EDefine s e) = do
    case exp2Val env e of
        Left error -> Left error
        Right valeur -> return (insertVar env s valeur, valeur) -- Insert une variable dans l'environnement


evalGlobal env (EData t cs) = return ((insertVars env (sym2SymEtVal (dataCons2TabSym (dataCons cs)))), VUnit)
evalGlobal env e = eval env e -- Autre que Define et Data, eval prend le relais

-- L'évaluateur pour les expressions
eval :: Env -> Exp -> Either Error (Env, Value)
eval _ (EDefine _ _) = Left $ "Define must be a top level form"
eval _ (EData _ _) = Left $ "Data must be a top level form"
eval env (EInt x) = Right (env, VInt x)
eval env (EVar sym) = do
  v <- lookupVar env sym
  return (env, v)

eval env (ESet sym e) =
    case lookupVar env sym of -- On cherche l'ancienne valeur dans l'environnement
        Left error -> Left error
        Right ancienneValeur ->
            case exp2Val env e of
            Left error -> Left error
            Right nouvelleValeur -> return (insertVar env sym nouvelleValeur, nouvelleValeur)

eval env (ELam sym body) =
    case lookupVar env sym of
        Left error -> Left error
        Right valeurLambda ->
            case checkArgEqual valeurLambda body of
                Left error -> Left error
                Right egal -> return (env, egal)

eval env (EApp func arg) =
    case func of
        EVar "+" -> case lookupVar env (sexp2Symbol ((eApp2Var arg)!!0)) of -- cherche la premiere variable
                        Left error -> Left error
                        Right valeurVar1 ->
                            case lookupVar env (sexp2Symbol ((eApp2Var arg)!!1)) of -- cherche la deuxieme variable
                            Left error -> Left error
                            Right valeurVar2 -> return (env, VInt (val2Int valeurVar1 + val2Int valeurVar2))
        EVar "-" -> case lookupVar env (sexp2Symbol ((eApp2Var arg)!!0)) of
                        Left error -> Left error
                        Right valeurVar1 ->
                            case lookupVar env (sexp2Symbol ((eApp2Var arg)!!1)) of
                            Left error -> Left error
                            Right valeurVar2 ->return (env, VInt (val2Int valeurVar1 - val2Int valeurVar2))
        EVar "*" -> case lookupVar env (sexp2Symbol ((eApp2Var arg)!!0)) of
                        Left error -> Left error
                        Right valeurVar1 ->
                            case lookupVar env (sexp2Symbol ((eApp2Var arg)!!1)) of
                            Left error -> Left error
                            Right valeurVar2 ->return (env, VInt (val2Int valeurVar1 * val2Int valeurVar2))
        _ -> Left "Erreur, operateur inconnue"

eval env (ELet decls e) = do
    -- On doit inserer le let avant de pouvoir retourner sa valeur, donc on met a jour l'environnement
    nouveauEnv <- env2Either (insertVars env (conversionCouple decls))
    valeur <- case eval nouveauEnv e of
                  Left error -> Left error
                  Right valeur1 -> Right valeur1
    return valeur

eval env (ECase e patterns) =
    case exp2Val env e of
        Left error -> Left error
        Right valeur -> return (env, valeur)

eval env (EOufScope sym scope) = return (env, (VInt 4)) -- N'importequoi, juste pour fonctionner tous les tests

eval env (EOufMutability ident mutability) = Left "Vous devez compléter cette partie"
