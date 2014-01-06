#!/bin/bash


PLT=plt/kanren-$RANDOM.plt
echo "PLT File $PLT"
export PATH=$PATH:/usr/local/bin:/usr/bin
echo "Building PLT, may take a few minutes"
dialyzer  --build_plt --apps kernel stdlib\
       --output_plt $PLT > /dev/null
echo "********************************************************************************"
echo ""

dialyzer	ebin/		\
    -Werror_handling		\
    -Wno_undefined_callbacks	\
    -Wrace_conditions		\
    --fullpath			\
    -n                          \
    --plt $PLT #  -Wunmatched_returns 
#  



