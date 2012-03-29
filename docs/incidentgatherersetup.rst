==========================
Setup of Incident Gatherer
==========================

 The incident gatherer (and potentially other gatherers in the future) lives
 on monitoring.   The gatherers have their own user "``gatherer``".  

 The file "``misc/incident-gatherer/support_classifiers.py``" appears to have
 been renamed "``misc/incident-gatherer/classify_tahoe.py``".

 It's worth noting that an incident gatherer monitoring tahoe nodes need not
 be colocated with a tahoe repo.  foolscap and twistd (upon which foolscap
 depends) are sufficient to run an incident gatherer.  Perhaps in the
 possibly ``classify_tahoe.py`` should be moved into the foolscap package?
 The correct disposition is unclear to me.

 
 When I typo the dump command I get this very unhelpful traceback:

 ::

  0 /home/arc/myLAEgateway/logs/incidents 506 $ flogtool dimp incident-2012-02-10--03\:50\:31Z-on5dnui.flog.bz2 
  Traceback (most recent call last):
    File "/usr/local/bin/flogtool", line 5, in <module>
      pkg_resources.run_script('foolscap==0.6.1', 'flogtool')
    File "/usr/lib/python2.7/dist-packages/pkg_resources.py", line 467, in run_script
      self.require(requires)[0].run_script(script_name, ns)
    File "/usr/lib/python2.7/dist-packages/pkg_resources.py", line 1194, in run_script
      raise ResolutionError("No script named %r" % script_name)
  pkg_resources.ResolutionError: No script named 'flogtool'

 And now a different level of indentation.
