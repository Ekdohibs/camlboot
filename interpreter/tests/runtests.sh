function test() {
  echo "Testing $1"
  cd $1
  OCAMLINTERP_DEBUG=0 OCAMLRUNPARAM=b \
    OCAMLINTERP_SRC_PATH=../../../ocaml-src \
    OCAMLINTERP_STDLIB_PATH=$OCAMLINTERP_SRC_PATH/stdlib \
    OCAMLINTERP_COMMAND=files ../../interpopt $(cat files)
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
