# .d.ts → Husk IR Interop Research

## Overview

This document analyzes the requirements for implementing interop import functionality that would parse TypeScript declaration files (.d.ts) and convert them to Husk IR for seamless module composition.

## Current State

### Existing Infrastructure
- ✅ **Path rewriting utilities** in `crates/interop/src/path_rewriting.rs`
- ✅ **Type IR scaffolding** in `crates/types/` with basic primitive/tuple/enum support
- ✅ **Resolver integration** with type checking capabilities
- ✅ **Import statement parsing** in AST with Import/Export support

### Current Import Handling
The resolver currently has basic import handling:
```rust
Item::Import(import) => {
    match &import.items {
        ImportClause::Named(items) => {
            for item in items {
                let symbol = Symbol {
                    name: item.local.clone(),
                    kind: SymbolKind::Import {
                        module: import.path.clone(),
                        original_name: item.name.clone(),
                    },
                    span,
                };
                self.declare_symbol(symbol);
            }
        }
        // ... other import types
    }
}
```

## TypeScript Declaration File Structure

### Key Elements to Parse
1. **Interface declarations**
   ```typescript
   interface User {
     id: number;
     name: string;
     active: boolean;
   }
   ```

2. **Type aliases**
   ```typescript
   type Status = 'active' | 'inactive' | 'pending';
   type UserOrNull = User | null;
   ```

3. **Enum declarations**
   ```typescript
   enum Color {
     Red = 'red',
     Green = 'green',
     Blue = 'blue'
   }
   ```

4. **Function signatures**
   ```typescript
   function createUser(name: string): User;
   declare function fetchData<T>(): Promise<T>;
   ```

5. **Module/namespace declarations**
   ```typescript
   declare module 'react' {
     interface Component<P = {}> { ... }
     function createElement(): ReactElement;
   }
   ```

6. **Export statements**
   ```typescript
   export { User, Status };
   export default createUser;
   ```

## Mapping to Husk IR

### Type System Mappings

| TypeScript | Husk Type IR | Notes |
|------------|--------------|-------|
| `string` | `Type::Primitive(String)` | Direct mapping |
| `number` | `Type::Primitive(Number)` | Direct mapping |
| `boolean` | `Type::Primitive(Bool)` | Direct mapping |
| `T \| U` | `Type::Enum` | Union → Sum type |
| `{ a: T, b: U }` | `Type::Tuple` or custom struct | Object → Product type |
| `T[]` | `Type::Array(T)` | Need to add Array type |
| `(x: T) => U` | `Type::Function([T], U)` | Function type |
| `Promise<T>` | Generic handling | Future work |

### Complex Mappings

#### Interfaces → Struct-like Enums
```typescript
interface User { id: number; name: string }
```
Could map to:
```rust
// In Husk IR:
Type::Enum {
    name: "User".to_string(),
    variants: vec![
        EnumVariant {
            name: "User".to_string(),
            field_types: vec![
                Type::Primitive(Number), // id
                Type::Primitive(String), // name
            ]
        }
    ]
}
```

#### Union Types → Enums
```typescript
type Status = 'active' | 'inactive' | 'pending'
```
Maps to:
```rust
Type::Enum {
    name: "Status".to_string(),
    variants: vec![
        EnumVariant { name: "active".to_string(), field_types: vec![] },
        EnumVariant { name: "inactive".to_string(), field_types: vec![] },
        EnumVariant { name: "pending".to_string(), field_types: vec![] },
    ]
}
```

## Implementation Strategy

### Phase 1: Basic .d.ts Parser
Create a TypeScript declaration parser that can handle:
- Basic type annotations
- Interface declarations  
- Simple union types
- Export statements

**Dependencies needed:**
- TypeScript AST parser (could use `swc_ecma_parser` or `tree-sitter-typescript`)
- AST traversal utilities

### Phase 2: IR Conversion
Extend `types` crate with:
```rust
pub struct ExternalModule {
    pub name: String,
    pub types: HashMap<String, Type>,
    pub functions: HashMap<String, Type>, // function signatures
    pub exports: HashSet<String>,
}

impl TypeChecker {
    pub fn register_external_module(&mut self, module: ExternalModule) { ... }
    pub fn resolve_imported_type(&self, module: &str, name: &str) -> Option<&Type> { ... }
}
```

### Phase 3: Resolver Integration
Enhance import resolution:
```rust
// In resolver, when encountering imports:
Item::Import(import) => {
    if import.path.ends_with(".d.ts") || is_npm_package(&import.path) {
        let external_module = self.parse_dts_file(&import.path)?;
        self.type_checker.register_external_module(external_module);
        
        // Register imported symbols with proper type information
        for item in &import.items {
            if let Some(imported_type) = self.type_checker.resolve_imported_type(&import.path, &item.name) {
                // Create symbol with full type information instead of just heuristics
                let symbol = Symbol {
                    name: item.local.clone(),
                    kind: SymbolKind::ImportedType {
                        module: import.path.clone(),
                        original_name: item.name.clone(),
                        type_info: imported_type.clone(),
                    },
                    span,
                };
                self.declare_symbol(symbol);
            }
        }
    }
    // ... existing logic for .hk imports
}
```

### Phase 4: Enhanced Diagnostics
With full type information from .d.ts:
- **Better exhaustiveness checking**: Know exact enum memberships from external types
- **Type compatibility validation**: Ensure imported types match usage
- **Improved error messages**: Show actual expected types from imports

## Benefits After Implementation

### Before (Current State)
```husk
import { useState } from 'react';

// Resolver uses uppercase heuristic - useState might be treated as variant
// No type information available for useState
// Match exhaustiveness can't validate against external enum variants
```

### After (With .d.ts Interop)
```husk
import { useState } from 'react';

// Resolver knows useState is a function type: <T>(T) => [T, (T) => void]
// Type checker validates usage against actual React types
// Exhaustiveness checking works with imported enum types
```

## Next Steps

1. **Choose TypeScript parser** - Research `swc_ecma_parser` vs alternatives
2. **Extend Type IR** - Add Array, Generic, and other missing type constructs  
3. **Implement basic .d.ts parser** - Start with simple interfaces and exports
4. **Create integration tests** - Test with real .d.ts files from popular packages
5. **Enhance resolver import logic** - Replace heuristics with actual type information

## Dependencies to Add
```toml
[dependencies]
swc_ecma_parser = "0.x" # For parsing .d.ts files
swc_ecma_ast = "0.x"    # TypeScript AST types
swc_common = "0.x"      # Common utilities
```

## Related Work
- **TypeScript Compiler API**: Reference implementation for .d.ts parsing
- **Rome/Biome**: Modern TypeScript parser for inspiration  
- **SWC**: Fast TypeScript/JavaScript parser written in Rust
- **Tree-sitter**: Alternative parsing approach

This research provides the foundation for implementing robust .d.ts → Husk IR interop that would enable seamless use of existing TypeScript ecosystem while maintaining Husk's type safety and pattern matching capabilities.