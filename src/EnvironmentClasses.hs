module EnvironmentClasses where
-- class Environment (e :: Type -> Type) v where
--   type Var e :: Type
--   emptyEnv :: e v
--   applyEnv :: e v -> Var e -> v
--   extendEnv :: Var e -> v -> e v -> e v

-- newtype EnvLstStr val = E [(String, val)]

-- instance Environment EnvLstStr Int where
--   type Var EnvLstStr = String
--   -- emptyEnv :: Env String Int
--   emptyEnv :: EnvLstStr Int
--   emptyEnv = E []
-- --   applyEnv :: Env String Int -> String -> Int
--   applyEnv :: EnvLstStr Int -> Var EnvLstStr -> Int
--   applyEnv (E []) _ = error "empty env"
--   applyEnv (E (x:xs)) var | let (var',val) = x, var == var' = val
--                           | otherwise = applyEnv (E xs) var
--   extendEnv :: Var EnvLstStr -> Int -> EnvLstStr Int -> EnvLstStr Int
--   extendEnv var val (E env) = E $ (var,val) : env




-- class Environment var val where
--   data EnvC var :: Type -> Type
--   -- type Var e :: Type
--   -- type Val e :: Type
--   emptyEnv :: EnvC var val
--   applyEnv :: EnvC var val -> var -> val
--   extendEnv :: var -> val -> EnvC var val -> EnvC var val


-- instance Environment String Int where
--   data EnvC String Int = E [(String, Int)]
--   emptyEnv :: EnvC String Int
--   emptyEnv = E []
--   applyEnv :: EnvC String Int -> String -> Int
--   applyEnv (E []) _ = error "empty env"
--   applyEnv (E (x:xs)) var | let (var',val) = x, var == var' = val
--                           | otherwise = applyEnv (E xs) var                              
--   extendEnv :: String -> Int -> EnvC String Int -> EnvC String Int
--   extendEnv var val (E env) = E $ (var,val) : env

-- class Environment String v => EnvS v where



-- class Environment var val where
--   -- type EnvC var = (t :: Type -> Type) | t -> var
--   type EnvC var val = t | t -> var val
--   emptyEnv :: EnvC var val
--   applyEnv :: EnvC var val -> var -> val
--   extendEnv :: var -> val -> EnvC var val -> EnvC var val

-- instance Environment String Int where
--   type EnvC String Int = [(String, Int)]
--   emptyEnv = []
--   applyEnv [] _ = error "empty env"
--   applyEnv (x:xs) var | let (var',val) = x, var == var' = val
--                           | otherwise = applyEnv xs var                              
--   extendEnv var val env = (var,val) : env

-- class Environment String v => EnvS v where


-- class Environment var where
--   -- type EnvC var = (t :: Type -> Type) | t -> var
--   type EnvC var val = t | t -> var val
--   emptyEnv :: EnvC var val
--   applyEnv :: EnvC var val -> var -> val
--   extendEnv :: var -> val -> EnvC var val -> EnvC var val

-- instance Environment String where
--   type EnvC String val = [(String,val)]
--   emptyEnv = []
--   applyEnv [] _ = error "empty env"
--   applyEnv (x:xs) var | let (var',val) = x, var == var' = val
--                           | otherwise = applyEnv xs var                              
--   extendEnv var val env = (var,val) : env

