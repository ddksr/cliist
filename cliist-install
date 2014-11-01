#!/bin/bash

if [[ -f settings.py ]]; then
	echo "Already installed."
	exit 0
fi;

echo -n "Specify your API token: "
read token
sed "s/API_TOKEN=''/API_TOKEN='$token'/g" settings.py.template > settings.py
echo "Installed."
exit 0
