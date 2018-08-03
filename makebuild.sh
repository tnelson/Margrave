#!/bin/sh
# Margrave build script

MARGRAVE_PROJECT_ROOT=`pwd`
echo Beginning build at Margrave root directory: ${MARGRAVE_PROJECT_ROOT}

##############################

echo Removing old files...

rm -rf ${MARGRAVE_PROJECT_ROOT}/build-temp

##############################
echo Copying files for build...

mkdir ${MARGRAVE_PROJECT_ROOT}/build-temp
cd ${MARGRAVE_PROJECT_ROOT}/build-temp

mkdir margrave
mkdir margrave/IOS-parser

# should be /lib, not /bin
mkdir margrave/bin
mkdir margrave/tests
mkdir margrave/examples
mkdir margrave/scribblings
mkdir margrave/licenses
mkdir margrave/lang


# The main Racket source for Margrave
cp ${MARGRAVE_PROJECT_ROOT}/margrave/*.rkt margrave

# The IOS parser was originally scheme, hence the .ss extension
cp ${MARGRAVE_PROJECT_ROOT}/margrave/IOS-parser/*.ss margrave/IOS-parser

cp ${MARGRAVE_PROJECT_ROOT}/margrave/COPYING.LESSER margrave/licenses
cp ${MARGRAVE_PROJECT_ROOT}/margrave/*LICENSE margrave/licenses

cp ${MARGRAVE_PROJECT_ROOT}/margrave/bin/*.jar margrave/bin

cp ${MARGRAVE_PROJECT_ROOT}/margrave/lang/*.rkt margrave/lang

# Odd behavior: raco setup looks in the module root for the css.
#cp ${MARGRAVE_PROJECT_ROOT}/margrave/scribblings/*.css margrave
#cp ${MARGRAVE_PROJECT_ROOT}/margrave/scribblings/*.css margrave/scribblings
#cp ${MARGRAVE_PROJECT_ROOT}/margrave/scribblings/*.png margrave/scribblings
cp ${MARGRAVE_PROJECT_ROOT}/margrave/scribblings/*.rkt margrave/scribblings
cp ${MARGRAVE_PROJECT_ROOT}/margrave/scribblings/*.scrbl margrave/scribblings

cp ${MARGRAVE_PROJECT_ROOT}/margrave/tests/*.rkt margrave/tests
cp ${MARGRAVE_PROJECT_ROOT}/margrave/tests/*.p margrave/tests
cp ${MARGRAVE_PROJECT_ROOT}/margrave/tests/*.v margrave/tests

cp -r ${MARGRAVE_PROJECT_ROOT}/margrave/examples/* margrave/examples

##############################

echo Building fresh Java .class files...

# !!!!!!!!!!!!!!!!!!!!!!!!!!!
# MAKE CHANGES TO JAVA OPTIONS HERE:
VERBOSE=0
DEBUG=1

MJLIB=${MARGRAVE_PROJECT_ROOT}/build-temp/margrave/bin

# -verbose for verbose compiler output
# -g to include debugging info

if [ $VERBOSE = 1 ]; then
    VFLAG="-verbose"
else
    VFLAG=""
fi

if [ $DEBUG = 1 ]; then
    DFLAG="-g"
else
    DFLAG=""
fi

# Force compatability with JRE 1.6.

JAVA_C="javac -target 1.6 -source 1.6 $VFLAG $DFLAG -classpath ${MJLIB}:.:${MJLIB}/kodkod.jar:${MJLIB}/sunxacml.jar:${MJLIB}/json.jar:${MJLIB}/org.sat4j.core.jar:${MJLIB}/commons-io-2.0.1.jar -d ${MJLIB}"
${JAVA_C} ${MARGRAVE_PROJECT_ROOT}/javasrc/edu/wpi/margrave/*.java

# JAR the class files
# Note that NOT cding and using ${MJLIB}/edu would not give us the desired outcome.
cd ${MJLIB}
jar cf ${MJLIB}/margrave.jar edu
# Add the v option for verbose output.

# Clean up by removing all class files
rm -r ${MJLIB}/edu

# Copy a fresh version of the .jar files to
# the DEVELOPMENT folder. Git will ignore.

#mkdir ${MARGRAVE_PROJECT_ROOT}/racket/lib
#cp ${MJLIB}/*.jar ${MARGRAVE_PROJECT_ROOT}/racket/lib

##############################
#echo Creating PLT archive for distribution
# Need to CD to appropriate folder again:
cd ${MARGRAVE_PROJECT_ROOT}/build-temp
raco pack --replace --at-plt ++setup margrave testbuild.plt margrave

##############################
#echo "Don't forget to insert safety checks. Did compilation complete ok? etc."
#echo "Also can we build in the date?"
