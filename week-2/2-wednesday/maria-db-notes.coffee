# INSTRUCTIONS:
# https://mariadb.com/kb/en/mariadb/building-mariadb-on-mac-os-x-using-homebrew/

# To start mysqld at boot time you have to copy
# support-files/mysql.server to the right place for your system
#
# PLEASE REMEMBER TO SET A PASSWORD FOR THE MariaDB root USER !
# To do so, start the server, then issue the following commands:
#
# '/usr/local/Cellar/mariadb/10.1.17/bin/mysqladmin' -u root password 'new-password'
# '/usr/local/Cellar/mariadb/10.1.17/bin/mysqladmin' -u root -h Patricks-MacBook-Pro.local password 'new-password'
#
# Alternatively you can run:
# '/usr/local/Cellar/mariadb/10.1.17/bin/mysql_secure_installation'
#
# which will also give you the option of removing the test
# databases and anonymous user created by default.  This is
# strongly recommended for production servers.
#
# See the MariaDB Knowledgebase at http://mariadb.com/kb or the
# MySQL manual for more instructions.
#
# You can start the MariaDB daemon with:
# cd '/usr/local/Cellar/mariadb/10.1.17' ; /usr/local/Cellar/mariadb/10.1.17/bin/mysqld_safe --datadir='/usr/local/var/mysql'
#
# You can test the MariaDB daemon with mysql-test-run.pl
# cd '/usr/local/Cellar/mariadb/10.1.17/mysql-test' ; perl mysql-test-run.pl
#
# Please report any problems at http://mariadb.org/jira
#
# The latest information about MariaDB is available at http://mariadb.org/.
# You can find additional information about the MySQL part at:
# http://dev.mysql.com
# Support MariaDB development by buying support/new features from MariaDB
# Corporation Ab. You can contact us about this at sales@mariadb.com.
# Alternatively consider joining our community based development effort:
# http://mariadb.com/kb/en/contributing-to-the-mariadb-project/

# ssh-keygen -t rsa -b 4096 -C "allenpatrick86@gmail.com"
# /Users/patrickallen/.ssh/id_rsa

#################### GITHUB SSH NOTES ###############################
SHA256:iEDHDG1kYDyUDAWAJFSIA1whsRxP/omAU/Da9ElzcDk allenpatrick86@gmail.com
The key's randomart image is:
+---[RSA 4096]----+
|/#&&* ..         |
|BB%o=oE          |
|o=+=o ..         |
| =.+o=..         |
|. ..+o. S        |
|                 |
|                 |
|                 |
|                 |
+----[SHA256]-----+
