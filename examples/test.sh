#!/bin/sh
# Shell script to test some examples

CURRYHOME=..
CURRYBIN=$CURRYHOME/bin

C2J=jucs

# Hide standard options:
CJOPTIONS=
export CJOPTIONS

# Check for Julia system:
JULIA=`which julia`
if [ "$JULIA" = "" ] ; then
  echo "Command 'julia' not found, skipping all tests!"
  exit
fi

# Clean old stuff:
clean() {
  cleancurry -r
  for P in $PROGRAMS ; do
    /bin/rm -f $P.jl
  done
}

run() {
  for P in $PROGRAMS ; do
    $C2J $CJOPTS $P
  done
}

testall() {
  TESTRESULT=$1
  echo "TESTING ALL PROGRAMS WITH OPTIONS: $CJOPTS"
  LOGFILETEE=xxx$$
  LOGFILE=XXX$$
  clean
  run | tee $LOGFILETEE
  clean
  sed -e '/NUMBER OF.*/d' $LOGFILETEE > $LOGFILE
  
  # Check differences:
  DIFF=diff$$
  diff $TESTRESULT $LOGFILE > $DIFF
  if [ "`cat $DIFF`" = "" ] ; then
    echo
    echo "REGRESSION TEST SUCCESSFULLY EXECUTED!"
    /bin/rm -f $LOGFILETEE $LOGFILE $DIFF
  else
    echo
    echo "DIFFERENCES IN REGRESSION TEST OCCURRED:"
    cat $DIFF
    /bin/rm -f $DIFF $LOGFILETEE
    /bin/mv -f $LOGFILE LOGFILE
    echo "Test output saved in file 'LOGFILE'."
    exit 1
  fi
}


# Tests with all run-time systems but without the Prelude:
PROGRAMS="Colormap Data FreeBool Half InfList NonDet Perm PermSort Rev Xor Zip"
CJOPTS="-x --noprelude"
testall TESTNOPRELUDE.txt
CJOPTS="-x --noprelude --pulltabonly"
testall TESTNOPRELUDE.txt
CJOPTS="-x --noprelude --pulltab"
testall TESTNOPRELUDE.txt
CJOPTS="-x --noprelude --backtrack"
testall TESTNOPRELUDE.txt

# Tests with the Prelude:
PROGRAMS="Fac CaseLiteral ColormapFree Higher Last"
CJOPTS="-x"
testall TESTPRELUDE.txt
CJOPTS="-x --backtrack"
testall TESTPRELUDE.txt

# Tests with functional patterns:
PROGRAMS="FunPatsLast FunPatsPali FunPatsExpSimp FunPatsExpVar"
CJOPTS="-x"
testall TESTFUNPATS.txt

# Tests where BFS strategy is relevant:
PROGRAMS="NDNums Strategy"
CJOPTS="-x --bfs --first"
testall TESTBFS.txt
