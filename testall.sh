for file in $(find $KOKA_ROOT/{lib/std,samples,test,util} -name '*.kk'); do
  if tree-sitter parse $file | grep ERROR >/dev/null; then
    echo $file
  fi
done
