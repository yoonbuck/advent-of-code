#!/bin/bash

cmd_error() {
  echo "Error: Unrecognized command $1" >&2
  echo "Try: run | test | debug | set | init" >&2
  exit 1
}

require_argument() {
  # require an argument
  if [ -z "$2" ]; then
    echo "Error: $1 requires an argument" >&2
    exit 1
  fi
}

get_module_name() {
  # pad with leading zero
  export MODULE_NAME="Day$(printf "%02d" $1)"
}

cmd_set_day() {
  require_argument "set" $1

  PREV_MODULE_NAME=$MODULE_NAME
  get_module_name $1

  # check if the module named "$MODULE_NAME.hs" doesn't exist
  if [ ! -f "$MODULE_NAME.hs" ]; then
    echo "Error: Module $MODULE_NAME.hs does not exist" >&2
    echo "Try running: aoc init $1" >&2
    echo "Keeping previous module $PREV_MODULE_NAME" >&2
    export MODULE_NAME=$PREV_MODULE_NAME
    exit 1
  fi

  export MODULE_NAME=$MODULE_NAME
  echo "Switched to module $MODULE_NAME."
}

cmd_initialize_day() {
  require_argument "init" $1

  PREV_MODULE_NAME=$MODULE_NAME
  get_module_name $1

  # check if the module named "$MODULE_NAME.hs" already exists
  if [ -f "$MODULE_NAME.hs" ]; then
    echo "Module $MODULE_NAME.hs already exists, not overwriting!" >&2
    # switch to the module instead
    cmd_set_day $1
    return
  fi

  # if $AOC_TEMPLATE_FILE is undefined, set it to template.hs
  if [ -z "$AOC_TEMPLATE_FILE" ]; then
    export AOC_TEMPLATE_FILE="template.hs"
  fi

  # check that the template file template.hs exists
  if [ ! -f $AOC_TEMPLATE_FILE ]; then
    echo "Error: Template file \"$AOC_TEMPLATE_FILE\" not found" >&2
    echo "Are you in the right working directory?" >&2
    echo "If this isn't the template file you expect, set AOC_TEMPLATE_FILE." >&2
    exit 1
  fi

  # create the module
  echo "module $MODULE_NAME where" > $MODULE_NAME.hs
  cat template.hs >> $MODULE_NAME.hs
  echo "Created and switched to module $MODULE_NAME."

  code $MODULE_NAME.hs input/*
}

run_setup() {
  case $1 in
    "b"|"B"|"2")
      export SOLVE_FN="solveB"
      ;;
    *)
      export SOLVE_FN="solveA"
      ;;
  esac

  # create the scaffolding file
  echo "module Main where
import $MODULE_NAME (parse, $SOLVE_FN)

main = interact ((++ \"\\n\") . show . $SOLVE_FN . parse)
" > main.hs
}

run_execute() {
  RESULT=$(runhaskell main.hs < $1)

  # if it succeeded, print the result and copy to the clipboard.
  if [ $? -eq 0 ]; then
    echo
    echo "Result: $RESULT"
    echo "(Copied to the clipboard.)"
    echo "$RESULT" | pbcopy
  else
    echo
    echo "Failed."
  fi
}

cmd_run() {
  run_setup $1

  if [ -z "$AOC_RUN_INPUT" ]; then
    export AOC_RUN_INPUT="input/full"
  fi

  if [ ! -f $AOC_RUN_INPUT ]; then
    echo "Error: Input file \"$AOC_RUN_INPUT\" not found" >&2
    echo "If this isn't the input file you expect, set AOC_RUN_INPUT." >&2
    exit 1
  fi

  echo "Running $MODULE_NAME's $SOLVE_FN with input file $AOC_RUN_INPUT..."
  run_execute $AOC_RUN_INPUT
}

cmd_test() {
  run_setup $1

  if [ -z "$AOC_TEST_INPUT" ]; then
    export AOC_TEST_INPUT="input/test"
  fi

  if [ ! -f $AOC_TEST_INPUT ]; then
    echo "Error: Input file \"$AOC_TEST_INPUT\" not found" >&2
    echo "If this isn't the input file you expect, set AOC_TEST_INPUT." >&2
    exit 1
  fi

  echo "Running $MODULE_NAME's $SOLVE_FN with input file $AOC_TEST_INPUT..."
  run_execute $AOC_TEST_INPUT
}

do_parse() {
  echo "module Main where
import $MODULE_NAME (parse)

main = interact (show . parse)
" > main.hs

  export PARSE_RESULT=$(runhaskell main.hs < $INPUT_FILE)
}

cmd_debug() {
  MODULE=$MODULE_NAME
  # if an argument was given, parse input
  if [ ! -z "$1" ]; then
    INPUT_FILE=$1
    if [ ! -f $INPUT_FILE ]; then
      INPUT_FILE="input/$1"
    fi
    if [ ! -f $INPUT_FILE ]; then
      echo "Error: Input file \"$INPUT_FILE\" not found" >&2
      exit 1
    fi
    do_parse
    echo "module Debug where
import $MODULE_NAME
input = $PARSE_RESULT" > debug.hs
    MODULE="Debug"
  fi

  echo "Starting ghci with module $MODULE_NAME..."
  echo "(Ctrl-D to exit)"
  ghci $MODULE
}

cmd_parse() {
  require_argument "parse" $1
  export INPUT_FILE=$1
  if [ ! -f $INPUT_FILE ]; then
    export INPUT_FILE="input/$1"
  fi
  if [ ! -f $INPUT_FILE ]; then
    echo "Error: Input file \"$INPUT_FILE\" not found" >&2
    exit 1
  fi

  echo "Running $MODULE_NAME's parse with input file $INPUT_FILE..."
  
  do_parse

  # if it succeeded, print the result and copy to the clipboard.
  if [ $? -eq 0 ]; then
    echo
    echo "Result: $PARSE_RESULT"
    echo "(Copied to the clipboard.)"
    echo "$PARSE_RESULT" | pbcopy
  else
    echo
    echo "Failed."
  fi
}


# if .aocmodule exists, load it into $MODULE_NAME
if [ -f .aocmodule ]; then
  export MODULE_NAME=$(cat .aocmodule)
fi

case $1 in
  "run")
    cmd_run $2
    ;;
  "test")
    cmd_test $2
    ;;
  "debug")
    cmd_debug $2
    ;;
  "parse")
    cmd_parse $2
    ;;
  "set")
    cmd_set_day $2
    ;;
  "init")
    cmd_initialize_day $2
    ;;
  *)
    cmd_error $1
    ;;
esac

# if MODULE_NAME is set, save it to .aocmodule
if [ -n "$MODULE_NAME" ]; then
  echo $MODULE_NAME > .aocmodule
fi
if [ -f main.hs ]; then
  rm main.hs
fi
if [ -f debug.hs ]; then
  rm debug.hs
fi