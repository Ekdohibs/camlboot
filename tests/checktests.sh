function test() {
  echo "Checking $1"
  cd $1
  ocamlc $(cat files) -o check.byte
  ./check.byte
  rm *.cm* check.byte
  echo "done with $1."
}

if [ ! -z $1 ]
then
	test $1
else
	for f in */
	do
		test $f
	done
fi
