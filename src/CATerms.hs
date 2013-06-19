{-# LANGUAGE    DeriveGeneric              #-}
{-# LANGUAGE    StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module CATerms where

import ATerm.Generics
import ATerm.Unshared
import GHC.Generics
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Name
import Language.C.Data.Position

deriving instance Generic Name

deriving instance Generic NodeInfo

deriving instance Generic CAssignOp
deriving instance Generic CBinaryOp
deriving instance Generic CUnaryOp

deriving instance Generic Ident


deriving instance Generic CChar
deriving instance Generic CIntRepr
deriving instance Generic CIntFlag
deriving instance Generic CInteger
deriving instance Generic CFloat
deriving instance Generic CString
deriving instance Generic (Flags a)

deriving instance Generic (CTranslationUnit a)
deriving instance Generic (CExternalDeclaration a)
deriving instance Generic (CFunctionDef a)
deriving instance Generic (CDeclaration a)
deriving instance Generic (CDeclarator a)
deriving instance Generic (CDerivedDeclarator a)
deriving instance Generic (CArraySize a)
deriving instance Generic (CStatement a)
deriving instance Generic (CAssemblyStatement a)
deriving instance Generic (CAssemblyOperand a)
deriving instance Generic (CCompoundBlockItem a)
deriving instance Generic (CDeclarationSpecifier a)
deriving instance Generic (CStorageSpecifier a)
deriving instance Generic (CTypeSpecifier a)
deriving instance Generic (CTypeQualifier a)
deriving instance Generic (CStructureUnion a)
deriving instance Generic (CStructTag)
deriving instance Generic (CEnumeration a)
deriving instance Generic (CInitializer a)
deriving instance Generic (CPartDesignator a)
deriving instance Generic (CAttribute a)
deriving instance Generic (CExpression a)
deriving instance Generic (CBuiltinThing a)
deriving instance Generic (CConstant a)
deriving instance Generic (CStringLiteral a)

instance ToATerm Position where toATerm     _  = AAppl "Position" [] []
                                toATermList xs = AList (map toATerm xs) []

instance                ToATerm NodeInfo
instance                ToATerm Name

instance                ToATerm CChar
instance                ToATerm CIntRepr
instance                ToATerm CIntFlag
instance                ToATerm CInteger
instance                ToATerm CFloat
instance                ToATerm CString
instance (ToATerm a) => ToATerm (Flags a)
instance                ToATerm CAssignOp
instance                ToATerm CBinaryOp
instance                ToATerm CUnaryOp

instance                ToATerm Ident

instance (ToATerm a) => ToATerm (CTranslationUnit a) where
instance (ToATerm a) => ToATerm (CExternalDeclaration a) where
instance (ToATerm a) => ToATerm (CFunctionDef a) where
instance (ToATerm a) => ToATerm (CDeclaration a) where
instance (ToATerm a) => ToATerm (CDeclarator a) where
instance (ToATerm a) => ToATerm (CDerivedDeclarator a) where
instance (ToATerm a) => ToATerm (CArraySize a) where
instance (ToATerm a) => ToATerm (CStatement a) where
instance (ToATerm a) => ToATerm (CAssemblyStatement a) where
instance (ToATerm a) => ToATerm (CAssemblyOperand a) where
instance (ToATerm a) => ToATerm (CCompoundBlockItem a) where
instance (ToATerm a) => ToATerm (CDeclarationSpecifier a) where
instance (ToATerm a) => ToATerm (CStorageSpecifier a) where
instance (ToATerm a) => ToATerm (CTypeSpecifier a) where
instance (ToATerm a) => ToATerm (CTypeQualifier a) where
instance (ToATerm a) => ToATerm (CStructureUnion a) where
instance                ToATerm (CStructTag) where
instance (ToATerm a) => ToATerm (CEnumeration a) where
instance (ToATerm a) => ToATerm (CInitializer a) where
instance (ToATerm a) => ToATerm (CPartDesignator a) where
instance (ToATerm a) => ToATerm (CAttribute a) where
instance (ToATerm a) => ToATerm (CExpression a) where
instance (ToATerm a) => ToATerm (CBuiltinThing a) where
instance (ToATerm a) => ToATerm (CConstant a) where
instance (ToATerm a) => ToATerm (CStringLiteral a) where


instance FromATerm Position where fromATerm (AAppl "Position" _ _) = Just nopos
                                  fromATerm _                      = Nothing
                                  fromATermList = atermToList

instance                  FromATerm NodeInfo
instance                  FromATerm Name

instance                  FromATerm CChar
instance                  FromATerm CIntRepr
instance                  FromATerm CIntFlag
instance                  FromATerm CInteger
instance                  FromATerm CFloat
instance                  FromATerm CString
instance (FromATerm a) => FromATerm (Flags a)
instance                  FromATerm CAssignOp
instance                  FromATerm CBinaryOp
instance                  FromATerm CUnaryOp

instance                  FromATerm Ident

instance (FromATerm a) => FromATerm (CTranslationUnit a) where
instance (FromATerm a) => FromATerm (CExternalDeclaration a) where
instance (FromATerm a) => FromATerm (CFunctionDef a) where
instance (FromATerm a) => FromATerm (CDeclaration a) where
instance (FromATerm a) => FromATerm (CDeclarator a) where
instance (FromATerm a) => FromATerm (CDerivedDeclarator a) where
instance (FromATerm a) => FromATerm (CArraySize a) where
instance (FromATerm a) => FromATerm (CStatement a) where
instance (FromATerm a) => FromATerm (CAssemblyStatement a) where
instance (FromATerm a) => FromATerm (CAssemblyOperand a) where
instance (FromATerm a) => FromATerm (CCompoundBlockItem a) where
instance (FromATerm a) => FromATerm (CDeclarationSpecifier a) where
instance (FromATerm a) => FromATerm (CStorageSpecifier a) where
instance (FromATerm a) => FromATerm (CTypeSpecifier a) where
instance (FromATerm a) => FromATerm (CTypeQualifier a) where
instance (FromATerm a) => FromATerm (CStructureUnion a) where
instance                  FromATerm (CStructTag) where
instance (FromATerm a) => FromATerm (CEnumeration a) where
instance (FromATerm a) => FromATerm (CInitializer a) where
instance (FromATerm a) => FromATerm (CPartDesignator a) where
instance (FromATerm a) => FromATerm (CAttribute a) where
instance (FromATerm a) => FromATerm (CExpression a) where
instance (FromATerm a) => FromATerm (CBuiltinThing a) where
instance (FromATerm a) => FromATerm (CConstant a) where
instance (FromATerm a) => FromATerm (CStringLiteral a) where
