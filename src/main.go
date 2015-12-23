package main

import (
  "fmt"
  "os"
  //"parse"
)



// constants

const (
  stdin = "stdin"
  objectFileExtension = ".o"
)



// functions

func main() {
  args := os.Args[1:]
  mainModuleName := stdin

  if len(args) == 1 {
    mainModuleName = args[0]
  } else if len(args) != 0 {
    os.Exit(1)
  }

  fmt.Println("Compiling", mainModuleFilename)

  if mainModuleName == stdin {
    interpretModule(os.Stdin)
  } else {
    file := os.Open(mainModuleName)
    defer file.Close()

    compileModule(file).WriteTo(mainModuleName + objectFileExtension)
  }
}


func interpretModule(file os.File) {
  errorMessages := make(chan string)
  go printErrorMessages(errorMessages)

  nodes := make(chan parse.AstNode)
  go parse.Parse(file, nodes, errorMessages)

  for node := range nodes {
  }
}


func compileModule(file os.File) module.Module {
  errorMessages := make(chan string)
  go printErrorMessages(errorMessages)

  nodes := make(chan parse.AstNode)
  go parse.Parse(file, nodes, )
}


func printErrorMessages(errorMessages <-chan string) {
  for errorMessage := range errorMessages {
    printErrorMessage(errorMessage)
  }
}


func printErrorMessage(errorMessage string) {
  if _, err := fmt.Fprintln(os.Stderr, errorMessage); err != nil {
    panic(err)
  }
}
