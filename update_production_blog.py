"""This script:
(1) updates a production blog instance.
"""

import sys

from lae_automation.server import check_branch_and_update_blog


check_branch_and_update_blog(branch='in_production',
                             host='leastauthority.com',
                             blog_repo_path='../blog_source/.git',
                             secret_config_path='../secret_config/.git',
                             stdout=sys.stdout)
