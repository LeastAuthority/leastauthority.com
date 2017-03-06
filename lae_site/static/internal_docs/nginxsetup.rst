
===========
nginx setup
===========

Here's how to install nginx -- the stable branch, packaged by the upstream
nginx developers/company -- on Ubuntu lucid. See below at the end for how to
uninstall other versions of nginx that we previously used.

for reference: http://www.nginx.org/en/download.html

Cut and paste the nginx signing key from below (that's better than wgetting
it yourself).::

	$ wget http://nginx.org/keys/nginx_signing.key
	$ cat > nginx_signing.key
	-----BEGIN PGP PUBLIC KEY BLOCK-----
	Version: GnuPG v1.4.11 (FreeBSD)
	
	mQENBE5OMmIBCAD+FPYKGriGGf7NqwKfWC83cBV01gabgVWQmZbMcFzeW+hMsgxH
	W6iimD0RsfZ9oEbfJCPG0CRSZ7ppq5pKamYs2+EJ8Q2ysOFHHwpGrA2C8zyNAs4I
	QxnZZIbETgcSwFtDun0XiqPwPZgyuXVm9PAbLZRbfBzm8wR/3SWygqZBBLdQk5TE
	fDR+Eny/M1RVR4xClECONF9UBB2ejFdI1LD45APbP2hsN/piFByU1t7yK2gpFyRt
	97WzGHn9MV5/TL7AmRPM4pcr3JacmtCnxXeCZ8nLqedoSuHFuhwyDnlAbu8I16O5
	XRrfzhrHRJFM1JnIiGmzZi6zBvH0ItfyX6ttABEBAAG0KW5naW54IHNpZ25pbmcg
	a2V5IDxzaWduaW5nLWtleUBuZ2lueC5jb20+iQE+BBMBAgAoBQJOTjJiAhsDBQkJ
	ZgGABgsJCAcDAgYVCAIJCgsEFgIDAQIeAQIXgAAKCRCr9b2Ce9m/YpvjB/98uV4t
	94d0oEh5XlqEZzVMrcTgPQ3BZt05N5xVuYaglv7OQtdlErMXmRWaFZEqDaMHdniC
	sF63jWMd29vC4xpzIfmsLK3ce9oYo4t9o4WWqBUdf0Ff1LMz1dfLG2HDtKPfYg3C
	8NESud09zuP5NohaE8Qzj/4p6rWDiRpuZ++4fnL3Dt3N6jXILwr/TM/Ma7jvaXGP
	DO3kzm4dNKp5b5bn2nT2QWLPnEKxvOg5Zoej8l9+KFsUnXoWoYCkMQ2QTpZQFNwF
	xwJGoAz8K3PwVPUrIL6b1lsiNovDgcgP0eDgzvwLynWKBPkRRjtgmWLoeaS9FAZV
	ccXJMmANXJFuCf26iQEcBBABAgAGBQJOTkelAAoJEKZP1bF62zmo79oH/1XDb29S
	YtWp+MTJTPFEwlWRiyRuDXy3wBd/BpwBRIWfWzMs1gnCjNjk0EVBVGa2grvy9Jtx
	JKMd6l/PWXVucSt+U/+GO8rBkw14SdhqxaS2l14v6gyMeUrSbY3XfToGfwHC4sa/
	Thn8X4jFaQ2XN5dAIzJGU1s5JA0tjEzUwCnmrKmyMlXZaoQVrmORGjCuH0I0aAFk
	RS0UtnB9HPpxhGVbs24xXZQnZDNbUQeulFxS4uP3OLDBAeCHl+v4t/uotIad8v6J
	SO93vc1evIje6lguE81HHmJn9noxPItvOvSMb2yPsE8mH4cJHRTFNSEhPW6ghmlf
	Wa9ZwiVX5igxcvaIRgQQEQIABgUCTk5b0gAKCRDs8OkLLBcgg1G+AKCnacLb/+W6
	cflirUIExgZdUJqoogCeNPVwXiHEIVqithAM1pdY/gcaQZmIRgQQEQIABgUCTk5f
	YQAKCRCpN2E5pSTFPnNWAJ9gUozyiS+9jf2rJvqmJSeWuCgVRwCcCUFhXRCpQO2Y
	Va3l3WuB+rgKjsQ=
	=A015
	-----END PGP PUBLIC KEY BLOCK-----

add it to the apt keyring::

	$ sudo apt-key add nginx_signing.key

if you want to see the fingerprint...::

	$ sudo apt-key finger
	/etc/apt/trusted.gpg
	-------------------
	pub   1024D/437D05B5 2004-09-12
	      Key fingerprint = 6302 39CC 130E 1A7F D81A  27B1 4097 6EAF 437D 05B5
	uid                  Ubuntu Archive Automatic Signing Key <ftpmaster@ubuntu.com>
	sub   2048g/79164387 2004-09-12
	
	pub   1024D/FBB75451 2004-12-30
	      Key fingerprint = C598 6B4F 1257 FFA8 6632  CBA7 4618 1433 FBB7 5451
	uid                  Ubuntu CD Image Automatic Signing Key <cdimage@ubuntu.com>
	
	pub   2048R/7BD9BF62 2011-08-19 [expires: 2016-08-17]
	      Key fingerprint = 573B FD6B 3D8F BC64 1079  A6AB ABF5 BD82 7BD9 BF62
	uid                  nginx signing key <signing-key@nginx.com>

optional: what version of nginx are you currently primed to get?::

	$ apt-cache policy nginx

Now add the apt repo::

	$ sudo add-apt-repository "deb http://nginx.org/packages/ubuntu/ lucid nginx"
	$ echo "deb http://nginx.org/packages/ubuntu/ lucid nginx" > nginx-stable-lucid.list
	$ sudo mv nginx-stable-lucid.list /etc/apt/sources.list.d/nginx-stable-lucid.list
	$ sudo apt-get update

note: when you run "apt-get update" it should *not* say anything about
inability to verify anything due to failure of public key digital signature
check! If it does, stop and figure out why.

optional: what version of nginx are you now primed to get?::

	$ apt-cache policy nginx
	$ sudo apt-get install nginx

done!

Optional: uninstalling old nginx repos

If the system previously had a different version of nginx installed, you can
tell from the "apt-cache policy" output, e.g.::

	$ apt-cache policy nginx
	nginx:
	  Installed: 1.2.0-1ppa1~lucid
	  Candidate: 1.2.0-1ppa1~lucid
	  Version table:
	 *** 1.2.0-1ppa1~lucid 0
    	    500 http://ppa.launchpad.net/nginx/stable/ubuntu/ lucid/main Packages
        	100 /var/lib/dpkg/status
	     1.2.0-1~lucid 0
    	    500 http://nginx.org/packages/ubuntu/ lucid/nginx Packages
	     1.0.15-1ppa3~lucid 0
	        500 http://ppa.launchpad.net/nginx/stable/ubuntu/ lucid/main Packages
	     0.7.65-1ubuntu2.2 0
	        500 http://us.archive.ubuntu.com/ubuntu/ lucid-updates/universe Packages
    	 0.7.65-1ubuntu2.1 0
	        500 http://security.ubuntu.com/ubuntu/ lucid-security/universe Packages
	     0.7.65-1ubuntu2 0
	        500 http://us.archive.ubuntu.com/ubuntu/ lucid/universe Packages

That ppa.launchpad.net is a different packaging by volunteers, let's remove
that one in favor of the official nginx.com one::

	$ grep -r nginx /etc/apt/sources.list*
	/etc/apt/sources.list:deb http://nginx.org/packages/ubuntu/ lucid nginx
	/etc/apt/sources.list.d/nginx-stable-lucid.list:deb http://ppa.launchpad.net/nginx/stable/ubuntu lucid main
	/etc/apt/sources.list.d/nginx-stable-lucid.list.save:deb http://ppa.launchpad.net/nginx/stable/ubuntu lucid main
	$ sudo rm /etc/apt/sources.list.d/nginx-stable-lucid.list*
	$ sudo apt-get update > /dev/null
	$ apt-cache policy nginx
	nginx:
	  Installed: 1.2.0-1ppa1~lucid
	  Candidate: 1.2.0-1ppa1~lucid
	  Version table:
	 *** 1.2.0-1ppa1~lucid 0
	        100 /var/lib/dpkg/status
	     1.2.0-1~lucid 0
	        500 http://nginx.org/packages/ubuntu/ lucid/nginx Packages
	     0.7.65-1ubuntu2.2 0
	        500 http://us.archive.ubuntu.com/ubuntu/ lucid-updates/universe Packages
	     0.7.65-1ubuntu2.1 0
	        500 http://security.ubuntu.com/ubuntu/ lucid-security/universe Packages
	     0.7.65-1ubuntu2 0
	        500 http://us.archive.ubuntu.com/ubuntu/ lucid/universe Packages

Huh-oh, the old one is still installed locally, and it has a new enough
version number that it won't automatically get replaced with the current
one. Let's manually remove it.::

	$ sudo apt-get remove nginx nginx-common nginx-full && sudo apt-get install nginx

optional: finding the sha256 hash of the .deb.

If you write down the sha256 hash of the .deb that you installed, then if in
the future it ever turns out that the GPG private key of the nginx developers
was compromised, you can check whether the actual specific .deb that you
installed was one of the legitimate ones or not.::

	$ find /var/cache/apt/ -iname '*nginx*'
	/var/cache/apt/archives/nginx_1.2.0-1~lucid_i386.deb
	$ sha256sum /var/cache/apt/archives/nginx_1.2.0-1~lucid_i386.deb
	1d78969eed12af6bb4066b05c2d9ba1cbd97d0356e5d845a8e0244fbdfb2d704  /var/cache/apt/archives/nginx_1.2.0-1~lucid_i386.deb
