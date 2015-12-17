package main

import (
    "flag"
    "os"
    "compile"
)
import "fmt"

func main() {
  files := os.Args
  fmt.Println(len(os.Args))
  compile.Compile()
}
