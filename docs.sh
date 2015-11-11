#!/bin/bash

set -e

DOCS="$( stack path --local-doc-root )"
RESOLVER="$( sed -ne 's/^resolver: *//p' stack.yaml )"
STACKDOCS="http:\/\/haddock.stackage.org\/${RESOLVER}\/"
README="<div id=\"description\">$( stack exec pandoc -- -t html README.md | sed 's:[/&\]:\\&:g;s/$/\\/')<\\/div>"
cd "$DOCS"
sed -i "s/<div id=\"content\"><div id=\"module-list\">/<div id=\"content\">${README}<div id=\"module-list\">/" index.html
find . -type f -name \*.html -exec sed -i "s/href=\"..\//href=\"${STACKDOCS}/g" {} \;
git init
git config user.name "Travis CI"
git config user.email "docbuilder@example.com"
git config push.default simple
git add .
git commit -m "Build docs for $TRAVIS_COMMIT ($TRAVIS_BRANCH)" || true
git push --force --quiet "https://${GH_AUTH}@${GH_REF}" master:gh-pages #> /dev/null 2>&1 || true
