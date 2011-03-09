LAFS-on-S3: reliable and secure storage
=======================================

**What is LAFS-on-S3?**

  *LAFS-on-S3* is a storage service offered by Least Authority
  Enterprises. It provides a reliable and scalable storage back-end
  for use with the Tahoe-LAFS client software.

**What is Tahoe-LAFS?**

  Tahoe-LAFS is a Free Software, Open Source cloud storage system. It
  encrypts and cryptographically integrity-checks your files for
  *provider-independent security*. That means that the confidentiality
  and integrity of your files cannot be violated by anyone—not even
  employees of the storage service provider. For more information see
  Tahoe-LAFS.org_.

  The Tahoe-LAFS project comes with complete source code, including
  client and server. If you use the *LAFS-on-S3* service then you need
  to run only the Tahoe-LAFS client software—we provide the
  cloud. However, the *LAFS-on-S3* service is completely open and
  transparent so that you always have the option to migrate your data
  from *LAFS-on-S3* to your own private Tahoe-LAFS cloud.

**What is it good for?**

  It is good for securely backing up your data off-site. The *"tahoe
  backup"* command inspects your local filesystem for files that have
  changed since the last time you ran it. It uploads each file that
  has changed and it creates a directory in Tahoe-LAFS to hold the
  current "snapshot"—the current version of each of the files. You can
  browse or access old versions just by browsing the old "snapshot"
  directories.

**Where is the data stored?**

  Your data, encrypted, is stored on Amazon's Simple Storage Service
  (S3_), which is a convenient, reliable, and widely understood
  platform for storage.

**How much does LAFS-on-S3 cost?**

  $0.30 (USD) per gigabyte per month.

**What is the relationship between Least Authority Enterprises and the Tahoe-LAFS open source project?**

  Zooko Wilcox-O'Hearn, founder of Least Authority Enterprises, was
  one of the inventors of Tahoe-LAFS and continues to actively
  contribute to the project an architect and coder. Least Authority
  Enterprises intend to contribute all of our work to the open source
  project.

.. _Tahoe-LAFS.org: http://tahoe-lafs.org
.. _S3: http://en.wikipedia.org/wiki/Amazon_S3
