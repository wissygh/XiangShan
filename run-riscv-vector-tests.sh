
# export NOOP_HOME=`pwd`
EMU=build/emu 
DIFF=/nfs/home/chengguanghui/projects/NEMU/build/riscv64-nemu-interpreter-so
TEST_PATH=/nfs/home/youzhaoyang/rv-tests/isa/build/
LOG_PATH=./CSR_tests

mkdir -p $LOG_PATH

# TESTS={
#     vse*
#     vle*
#     vlse*
#     vlss*
#     vsse*
#     vlox*
#     vlux*
#     vsox*
#     vsux*
#     vl[0-9]re*
#     vs[0-9]r*
#     vlm*
#     vsm*
#     vssseg*
# }

# for test in ${TESTS[@]}; do
#     echo $test;
#     for bin in $TEST_PATH/$test*.bin; do 
#         file=${bin##*/};
#         cmd="$EMU --diff $DIFF -i $bin"
#         echo "$cmd 2> $LOG_PATH/perf-$file.log 1> $LOG_PATH/run-$file.log"
#         $cmd 2> $LOG_PATH/perf-$file.log 1> $LOG_PATH/run-$file.log
#     done;
# done

for bin in $TEST_PATH/*.bin; do 
    file=${bin##*/};
    cmd="$EMU --diff $DIFF -i $bin"
    echo "$cmd 2> $LOG_PATH/perf-$file.log 1> $LOG_PATH/run-$file.log"
    $cmd 2> $LOG_PATH/perf-$file.log 1> $LOG_PATH/run-$file.log
done;
