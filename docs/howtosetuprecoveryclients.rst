Install git.

Setup a user account for backup:  e.g. 'laebackup'.

Create an ssh keypair for that user.  i.e. run 'ssh-keygen' as that user.

Don't create a passphrase for the private key in that keypair.

Put the ssh public key in: 

  'theta.leastauthority.com:/home/websitebackup/.ssh/authorized_keys'

Run:

``cd /home/${BACKUPUSER} && git clone websitebackup@theta.leastauthority.com:/home/websitebackup/website``

``crontab -e``

add the following line to the crontab for ${BACKUPUSER}:

``$MINUTE $HOUR * * * /home/production_backup/disasterrecovery/clientcronbackupjob.sh``

to run the client backup script once per day.
