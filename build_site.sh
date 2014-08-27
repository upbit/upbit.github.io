#!/bin/bash

# run _plugin/*.rb
rm -Rf archive
rm -Rf _site
jekyll build

# gen archive/ and rss.xml
cp -Rvf _site/archive ./

# compress CSS
CSS_NAME_LIST="main normalize screen syntax"

cd assets/css/
for css in ${CSS_NAME_LIST}
do
	rm -vf ${css}.min.css
	java -jar yuicompressor-2.4.8.jar ${css}.css -o ${css}.min.css
done
cd ../../
