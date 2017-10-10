# WLP-based Automated Testing

Prerequisites: Back-end SMT solver already installed (e.g. Z3)

To setup: `stack setup`

To build: `stack build`

To execute interactively: `stack exec -- wlp-test-exe -i`

To execute with input file: `stack exec -- wlp-test-exe <input_file>`

# TODO
- [x] Path calculation
  * `calculatePaths :: Program -> [ProgramPath]`
- [x] WLP Transformer
  * `wlp :: Program -> Expr`
- [x] Normalization on Imply
  * `P ==> P' ==> Q` ~> `P /\ P' ==> Q`
- [x] Predicate to arithmetic intervals
  * `P1(x, y) /\ P2(y) /\ P3(x)` ~> `[Range]`
- [x] Program testing
  * [x] Base implementation
  * [ ] Arrays
- [ ] Extensions
  * [x] Array Assignment
    - add `repby` to `Expr`
    - add `g -> t | f` to `Expr`
  * [ ] Program call
    - add `vars := prog_name([Expression])` to `Stmt`
  * [x] Loop invariants
    - add `{ invariant } while ...` to `Stmt`
    - utilize extra logical information
- [ ] Report
  * [ ] Problem
  * [ ] Proposed Solution
  * [ ] Results
  * [ ] Related Work
  * [ ] Conclusion
- [ ] Presentation
