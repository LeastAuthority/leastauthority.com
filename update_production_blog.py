"""This script:
(1) updates a production blog instance.
"""

import sys

from lae_automation.server import check_branch_and_update_blog


check_branch_and_update_blog(branch='in_production',
                             host='leastauthority.com',
                             blog_repo_workdir='../blog_source',
                             secret_config_repo_workdir='../secret_config',
                             stdout=sys.stdout)
