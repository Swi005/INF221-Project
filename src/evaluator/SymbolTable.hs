module SymbolTable where
import Data.Map (Map)
import qualified Data.Map as Map

type Identifier = String

data SymbolTable a = SymbolTable {
                        id :: Int,
                        symbol_table_name::Identifier,
                        symbol_map:: Map Identifier a
                        } deriving (Show, Eq)
