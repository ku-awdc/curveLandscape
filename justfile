default:
    echo 'Hello, world!'

document: 
    Rscript -e rextendr::document\(\)

watch:
    cargo watch --workdir=src/rust -c -s "just document" -x "check --tests"
