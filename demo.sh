#!/bin/bash
echo -e "###################   PROGETTO LINGUAGGI E COMPILATORI   ###################n"
echo -e "Parte4 - Gruppo5\n"
echo -e "###################           Esempi corretti           ###################\n"
for i in test_cases/corretti/* ; do
	echo -e "----------------           Begin test           ---------------\n"
	echo "File $i:"
	echo -e "\n\n";
	./Main < "$i"
	echo -e "\n-----------------           End test           ----------------\n"
done
echo -e "\n###################         Esempi sbagliati           ###################\n"
for i in test_cases/sbagliati/* ; do
	echo -e "----------------           Begin test           ---------------\n"
	echo "File $i:"
	echo -e "\n\n";
	./Main < "$i"
	echo -e "\n-----------------           End test           ----------------\n"
done
