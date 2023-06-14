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

    let result = try { 
        Passed (!_build/default/bin/main.exe file)
    } with {
        CommandFailure(failure) -> Failed (failure.stdout)
    }


    let onError(result) = {
        errors := errors! + 1
        !echo "-e" ("\x1b[31m[" ~ file ~ "]: FAILED!"
            ~ "\nEXPECTED: " ~ expectation
            ~ "\nACTUAL: " ~ result ~ "\x1b[0m")
        ()

    }
    match result {
        Passed(result) -> {
            if result == expectation then {
                !echo "-e" ("\x1b[32m[" ~ file ~ "]: passed\x1b[0m")
                ()
            } else {
                onError(result)
            }
        }
        Failed(err) -> onError(err)
    }
})

if errors! == 0 then {
    !echo "-e" ("\x1b[32m\x1b[38;2;0;255;0mAll tests passed\x1b[0m")
    ()
} else {
    !echo "-e" ("\x1b[31m\x1b[38;2;255;0;0m" ~ toString(errors!) ~ " TESTS FAILED\x1b[0m")
    ()
}
