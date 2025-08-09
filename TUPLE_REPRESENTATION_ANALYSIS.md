# Tuple Representation Consistency Analysis

## Current Issue

There is a critical inconsistency in how tuples are represented between JavaScript generation and TypeScript declaration generation:

### JavaScript Generation (crates/emit_js/src/lib.rs)
- **Tuples are represented as objects** with numeric keys: `{0: arg0, 1: arg1, 2: arg2}`
- Pattern matching checks for: `typeof _s === 'object' && _s !== null && !Array.isArray(_s)`
- Accessed via bracket notation: `_s[0]`, `_s[1]`, etc.

### TypeScript Declaration Generation (crates/emit_dts/src/lib.rs:209-212)
- **Tuples are represented as arrays**: `[type1, type2, type3]`
- Uses TypeScript's native tuple syntax

## Root Cause Analysis

1. **JavaScript Runtime Representation**: Tuples are implemented as objects to distinguish them from regular JavaScript arrays
2. **TypeScript Type Declaration**: Uses native TypeScript tuple syntax for better type checking
3. **Inconsistency**: Runtime objects with TS array types will cause type mismatches

## Current Test Evidence

The existing test `test_emit_tuple_pattern_matching()` demonstrates the JavaScript behavior:
- Input: `fn test(point: (number, string)) { match point { (x, "center") => x, ... } }`
- Generated JS checks: `typeof _s === 'object'` and `_s[1] === "center"`
- This confirms object representation at runtime

## Expected Behavior vs Reality

**Expected (Consistent)**: 
- JS: `[arg0, arg1, arg2]` (arrays)
- DTS: `[type1, type2, type3]` (array types)

**OR**

- JS: `{0: arg0, 1: arg1, 2: arg2}` (objects)  
- DTS: `{0: type1, 1: type2, 2: type3}` (object types)

**Current (Inconsistent)**:
- JS: `{0: arg0, 1: arg1, 2: arg2}` (objects)
- DTS: `[type1, type2, type3]` (array types) ❌

## Impact

1. **TypeScript compiler errors** when interfacing with generated JavaScript
2. **Runtime type mismatches** in mixed Husk/TypeScript codebases  
3. **IntelliSense/IDE confusion** about tuple structure
4. **Difficult debugging** due to representation mismatch

## Recommended Fix

**Option 1 (Preferred)**: Change DTS generation to match JS object representation
```typescript
// Instead of: [string, number]
// Generate: {0: string, 1: number}
```

**Option 2**: Change JS generation to use arrays (may break existing code)
```javascript
// Instead of: {0: "hello", 1: 42}
// Generate: ["hello", 42]
```

## Testing Required

1. **Runtime compatibility**: Ensure existing pattern matching still works
2. **TypeScript interop**: Verify generated .d.ts files work with TypeScript compiler
3. **IDE support**: Confirm IntelliSense works correctly with chosen representation

## Files to Modify

- `crates/emit_dts/src/lib.rs` (emit_type function for Type::Tuple)
- Add comprehensive tests for tuple representation consistency
- Update existing tests if representation changes