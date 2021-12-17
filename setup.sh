function build (){
    stack build
}

function run() {
    arg=$1
    if [[ $arg != "" ]] then
        arg="-- $arg"
    fi
    stack run `echo $arg`
}
