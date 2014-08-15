#!/bin/bash

# put compress CSS name here
CSS_NAME_LIST="main normalize screen syntax"

for css in ${CSS_NAME_LIST}
do
	rm -vf ${css}.min.css
	java -jar yuicompressor-2.4.8.jar ${css}.css -o ${css}.min.css
done
