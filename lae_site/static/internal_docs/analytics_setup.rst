sudo apt-get install mysql-server mysql-client php5 php5-gd php5-cgi php5-mysql nginx-full unzip
(enter $MYSQL_ROOT_PASSWORD twice when prompted)

sudo /etc/init.d/apache2 stop
mkdir init.d.old
sudo mv /etc/init.d/apache2 init.d.old
sudo update-rc.d apache2 remove

(edit /etc/nginx/nginx.conf to change keepalive_timeout to 2)

(edit /etc/php5/cli/php.ini and /etc/php5/cgi/php.ini, for each
find the line ";cgi.fix_pathinfo=1" and replace it with "cgi.fix_pathinfo=0" [no ';'].
add the following lines in the "Dynamic Extensions" section:

extension=mysqli.so
extension=pdo.so
extension=pdo_mysql.so
)

XXX (create /etc/init.d/php-fcgi)

sudo adduser analytics
(enter password, name as "Analytics", and leave other fields blank)

cd /home/analytics
sudo mkdir public_html run
sudo chown analytics:www-data public_html run
sudo chmod +s,o-rwx public_html run

sudo service mysql start

sudo su - analytics

mysql -u root -p
(enter $MYSQL_ROOT_PASSWORD)
CREATE DATABASE piwik;
CREATE USER 'piwik'@'localhost' IDENTIFIED BY '$MYSQL_PIWIK_PASSWORD'
GRANT ALL PRIVILEGES ON piwik_db_name_here.* TO 'piwik'@'localhost' WITH GRANT OPTION;
Ctrl-D

cd public_html
wget https://builds.piwik.org/latest.zip
unzip latest.zip
rm latest.zip
exit


sudo invoke-rc.d nginx start
sudo invoke-rc.d php-fcgi start
