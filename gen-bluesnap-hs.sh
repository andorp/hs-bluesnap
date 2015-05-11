#!/bin/bash

XSD_DIR=./xsd
HS_DIR=./src/Bluesnap/API
HS_MODULE="Bluesnap.API"

# Returns the file name without extension
function file_name {
  name=$(echo $(basename $1) | cut -d"." -f1)
  echo $name
}

for file in $(ls -a $XSD_DIR/*.xsd)
do
  hs_name=$(echo $(file_name $file) | sed "s/-/_/g" | python -c "print raw_input().capitalize()")
  echo "Creating $hs_name ..."
  XsdToHaskell $file | tail -n +9 | sed "s/'/_/g" | sed "s/V__xsd_//g" | sed "s/_xsd//g" | sed "s/oneOf_/oneOf'/g" | runhaskell FixModules.hs $HS_MODULE > $HS_DIR/$hs_name.hs
done
