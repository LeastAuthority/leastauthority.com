How Documents Get From Za to LAE Staging
========================================

On my laptop there's a single git repository rooted at:

``/home/arc/pyvirtenvs/source/leastauthority.com/content/static/internal_docs/issue_investigation/``

It contains 2 branches "``master``" and "``working``".

While I am working I have "``working``" checked out.  I run "``add``\'s"
periodic "``commits``" etc, in this context.  In the case of experiments gone
wrong, I can recover by reverting to "``master``".  

When the process has reached a milestone e.g. 'end of a workday, task, or
deliverable', I push this repository to the remote repository at:

``theta.leastauthority.com:/home/zancas/repos/workbench_docs`` .

When documentation reaches a review-ready state (i.e. me re-reading it is
producing diminishing returns) I "``darcs record``" the above repository, and
pull that patch into "staging". 
