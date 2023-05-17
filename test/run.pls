#!/usr/bin/env polaris
options {
    "-s" "--sync" as sync: "Run tests synchronously rather than in parallel"
}

let forSync(list, f) = match list {
    [] -> ()
    (x :: xs) -> {
        f(x)
        forSync(xs, f)
    }
}

let forConcurrent(xs, f) = {
    let promises = [(async f(x)) | let x <- xs]
    forSync(promises, \p -> await p)
}

let for = if sync then forSync else forConcurrent


let errors = ref 0

let files = lines(!find (scriptLocal("cases")) "-name" "*.flora")

!dune "build"

for(files, \file -> {
    let expectation = !grep "-Po" "(?<=-- EXPECT: ).+" file

    let result = !_build/default/bin/main.exe file

    if result == expectation then {
        !echo "-e" ("\x1b[32m[" ~ file ~ "]: passed\x1b[0m")
        ()
    } else {
        errors := errors! + 1
        !echo "-e" ("\x1b[31m[" ~ file ~ "]: FAILED!")
        !echo "-e" ("EXPECTED: " ~ expectation)
        !echo "-e" ("ACTUAL: " ~ result ~ "\x1b[0m")
        ()
    }
    ()
})

if errors! == 0 then {
    !echo "-e" ("\x1b[32m\x1b[38;2;0;255;0mAll tests passed\x1b[0m")
    ()
} else {
    !echo "-e" ("\x1b[31m\x1b[38;2;255;0;0m" ~ toString(errors!) ~ " TESTS FAILED\x1b[0m")
    ()
}
