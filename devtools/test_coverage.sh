#! /bin/bash

set -ex

coverage run --branch --source ./lae_site $(which trial) ./lae_site
coverage html
python -c 'import webbrowser; webbrowser.open("./htmlcov/index.html")'
