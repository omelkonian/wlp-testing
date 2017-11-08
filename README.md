# WLP-based Automated Testing

Prerequisites: Back-end SMT solver already installed (e.g. Z3, MathSAT)

To setup: `stack setup`

To build: `stack build`

To execute: `stack exec -- wlp <args>`

To check available args: `stack exec -- wlp -h`

```bash
  Usage: wlp (-i|--input INPUT_FILE) [-s INT] [-e INT] [-d|--debug] [-A|--all]
  WLP-based testing

  Available options:
  -h,--help                Show this help text
  -i,--input INPUT_FILE    GCL program
  -s INT                   Min depth (default: 1)
  -e INT                   Max depth (default: 100)
  -d,--debug               Whether to print debug info
  -A,--all                 Whether to verify all programs
```

To run the provided test suite: `stack test`
