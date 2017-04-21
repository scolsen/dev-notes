#read file line by line
input="/my/file.txt"
while read -r line
do 
	echo "$line";
done < "$input"

#alternative, use IFS option to prevent lead/trail whitespace trimming
while IFS= read -r line
do
	echo "$line"
done < "myFile.txt"
