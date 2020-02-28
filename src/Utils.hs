{-|
Shoving away gross stuff into this one module.
-}
module Utils where

import Control.Monad.Trans.State
import Data.ByteString.Short (ShortByteString)
import Data.Functor.Identity
import Data.List
import LLVM.AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Internal.SnocList

moduleSoFar :: MonadModuleBuilder m => ShortByteString -> m Module
moduleSoFar nm = do
    s <- liftModuleState get
    let ds = getSnocList (builderDefs s)
    return $ defaultModule { moduleName = nm, moduleDefinitions = ds }

removeDef :: MonadModuleBuilder m => Definition -> m ()
removeDef def = liftModuleState (modify update)
    where
        update (ModuleBuilderState defs typeDefs) =
            let newDefs = SnocList (delete def (getSnocList defs))
            in ModuleBuilderState newDefs typeDefs

mostRecentDef :: Monad m => ModuleBuilderT m Definition
mostRecentDef = last . getSnocList . builderDefs <$> liftModuleState get

hoist :: Monad m => ModuleBuilder a -> ModuleBuilderT m a
hoist m = ModuleBuilderT $ StateT $
    return . runIdentity . runStateT (unModuleBuilderT m)