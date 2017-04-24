#read JSON in a shell script.

TEXT=""
TFILE=$(mktemp)
tr -d '\n' < $1 > $TFILE
sed 's/$/\n/' < $TFILE
IFS="," read -ra ARR < $TFILE
echo "${ARR[@]}"
for i in "${ARR[@]}"; 
do
	res="$(echo $i | sed 's/{//; s/}//; s/":"/=/; s/"//g; s/$/\n/')"
	TEXT="${TEXT}${res}\n"
done

echo -e $TEXT
