default:
    echo 'Hello, world!'

document: 
    Rscript -e rextendr::document\(\)

watch:
    cargo watch --workdir=src/rust -c -x "update" -s "just document" -x "check --tests"
# TODO: Add sensitivity to the stuff in `src/rust`, atleast Makevars and source code.