import subprocess

rm_blog_source_string = '/usr/bin/git rm -r ./blog_source'
subprocess.check_call(rm_blog_source_string.split())

rm_author_string = '/usr/bin/git rm -r ./content/blog/author/*.html'
subprocess.check_call(rm_author_string.split())

rm_category_string = '/usr/bin/git rm -r ./content/blog/category/*.html'
subprocess.check_call(rm_category_string.split())

rm_tag_string = '/usr/bin/git rm -r ./content/blog/tag/*.html'
subprocess.check_call(rm_tag_string.split())

rm_theme_string = '/usr/bin/git rm -r ./content/blog/theme/*.html'
subprocess.check_call(rm_theme_string.split())

rm_bloghtml_string = '/usr/bin/git rm -r ./content/blog/*.html'
subprocess.check_call(rm_theme_string.split())
