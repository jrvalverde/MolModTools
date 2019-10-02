cat $1 | sed -e 's#/opt/structure/modeller/bin/mod9v3#/home/jr/contrib/modeller/bin/mod9.11#g' > k
cat k > $1
rm k

