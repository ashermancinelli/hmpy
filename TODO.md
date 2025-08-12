# Type Checker Improvements Roadmap

## 1. Pattern Matching Exhaustiveness & Redundancy Checking
**Goal**: Detect incomplete pattern matches and unreachable cases
**Implementation**: 
- Add pattern completeness analysis similar to OCaml's `parmatch.ml`
- Track which constructors/values are covered in match expressions
- Warn on missing cases, error on unreachable code
**Example**:
```python
# Should warn: missing case for Some(x)
match maybe_val:
    case None: return 0
    # Missing: case Some(x): ...
```

## 2. Polymorphic Variants & Records  
**Goal**: Support structural types beyond basic int/bool
**Implementation**:
- Add variant types: `type color = Red | Green | Blue`
- Add record types: `type person = {name: string, age: int}`
- Extend type inference to handle constructor patterns
**Example**:
```python
# Variant declaration
VariantDecl("color", ["Red", "Green", "Blue"])
# Record syntax
Record([("name", StringType), ("age", IntType)])
```

## 3. Better Error Messages with Context
**Goal**: Explain WHY types are required (like OCaml's `type_forcing_context`)
**Implementation**:
- Track context where type constraints arise (if-condition, function argument, etc.)
- Include context in error messages: "Expected bool in if-condition, got int"
- Add error trace for complex unification failures

## 4. Module System with Signatures
**Goal**: Support namespacing and interface specifications
**Implementation**:
- Add module declarations and signatures
- Type checking across module boundaries
- Interface conformance checking

## 5. Generalized Algebraic Data Types (GADTs)
**Goal**: More expressive type relationships
**Implementation**:
- Allow constructors to constrain type parameters
- Enable type refinement in pattern matching
**Example**:
```python
# GADT allowing length-indexed lists
type vec[n] = 
  | Nil : vec[0]
  | Cons : (a * vec[n]) -> vec[n+1]
```

## 6. Row Polymorphism
**Goal**: Extensible records and variants
**Implementation**: 
- Support "at least these fields" record types
- Open variant types that can be extended

## Next Steps
Start with #1 (pattern exhaustiveness) as it provides immediate practical value and builds on existing `Match` AST nodes.