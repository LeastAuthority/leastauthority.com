# -*- coding: utf-8 -*-
from jinja2 import Environment, BaseLoader, TemplateNotFound
import os
import sys
import codecs
import subprocess


class MyLoader(BaseLoader):
    def __init__(self, path):
        self.path = path

    def get_source(self, environment, template):
        path = os.path.join(self.path, template)
        if not os.path.exists(path):
            raise TemplateNotFound(template)
        mtime = os.path.getmtime(path)
        with file(path) as f:
            source = f.read().decode('utf-8')
        return source, path, lambda: mtime == os.path.getmtime(path)


def render_all(template_path=None, output_path=None, static_path=None):
    if not template_path:
        template_path = 'lae_site/templates/'
    if not output_path:
        output_path = 'lae_rendered/'
    if not static_path:
        static_path = 'content/static/'
    env = Environment(loader=MyLoader(template_path))
    # find all the files inside all the subdirectories of the template path
    all_the_things = os.walk(template_path)
    for root, dirs, files in all_the_things:
        for f in files:
            # check that it's a template file
            if f[-5:] == '.html' and f[:1] != '_':
                full_path = os.path.join(root,f)
                # path relative to template_path
                relative_path = full_path[len(template_path):]
                print "Rendering " + relative_path
                # render the template
                template = env.get_template(relative_path)
                # calculate directory output should go in
                dirname = os.path.dirname(output_path + '/' + relative_path)
                # and if it doesn't exist yet, create it
                if not os.path.exists(dirname):
                    os.makedirs(dirname)
                # make rendered html file
                with codecs.open(output_path + '/' + relative_path, 'w', 'utf-8') as render_file:
                    for line in template.render():
                        render_file.write(line)
    if not os.path.isdir(output_path + 'static'):
        subprocess.check_call(['ln', '-s', '../' + static_path, 'static'], cwd=os.getcwd() + '/' + output_path)
    print "Made symlink to static files."
    subprocess.check_call(['python', '-m', 'SimpleHTTPServer', '8002'], cwd=os.getcwd() + '/' + output_path)
if __name__ == "__main__":
    render_all(*sys.argv[1:])
