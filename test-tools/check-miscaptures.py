#!/usr/bin/env python

# Usage:
#
#    ./check-miscaptures.py [file or directory; default ${PWD}]
#
# Report on Python source files with respect to their use of closures over
# loop variables.  A closure over a loop variable is most often a mistake.
#
# For example, consider these two snippets (both within some enclosing function):
#
#    ys = []
#    for x in xs:
#        def f():
#            return x
#        ys.append(f)
#
# versus
#
#    ys = []
#    for x in xs:
#        def f(value=x):
#            return value
#        ys.append(f)
#
# The functions created inside the loop in the first snippet close over ``x``.
# However, the value of ``x`` varies over the execution of the loop and is
# left with the final value of the ``xs`` iterable.  Therefore all functions
# in ``ys`` will return the final value of the ``xs`` iterable.
#
# The functions created inside the loop in the second snippet do not close
# over ``x``.  Instead, they use a common trick for avoiding the above
# described behavior.  They bind the value of ``x`` as the default for the
# ``value`` parameter.  These functions, when called with no arguments, will
# return that default.  Since the value is bound at function definition time,
# each iteration will produce a function which returns a subsequent value from
# the ``xs`` iterable.
#
# The former code will provoke a report like the following from this tool:
#
#     ./path/to/source.py:10 <lambda> captures 'x' assigned at line 5
#     1 suspiciously captured variables in 1 out of 1 file(s).


import os, sys, compiler
from compiler.ast import Node, For, While, ListComp, AssName, Name, Lambda, Function


def check_source(source):
    return check_thing(compiler.parse, source)

def check_file(path):
    return check_thing(compiler.parseFile, path)

def check_thing(parser, thing):
    try:
        ast = parser(thing)
    except SyntaxError, e:
        return e
    else:
        results = []
        check_ast(ast, results)
        return results

def check_ast(ast, results):
    """Check a node outside a loop."""
    if isinstance(ast, (For, While, ListComp)):
        check_loop(ast, results)
    else:
        for child in ast.getChildNodes():
            if isinstance(ast, Node):
                check_ast(child, results)

def check_loop(ast, results):
    """Check a particular outer loop."""

    # List comprehensions have a poorly designed AST of the form
    # ListComp(exprNode, [ListCompFor(...), ...]), in which the
    # result expression is outside the ListCompFor node even though
    # it is logically inside the loop(s).
    # There may be multiple ListCompFor nodes (in cases such as
    #   [lambda: (a,b) for a in ... for b in ...]
    # ), and that case they are not nested in the AST. But these
    # warts (nonobviously) happen not to matter for our analysis.

    assigned = {}  # maps name to lineno of topmost assignment
    nested = set()
    collect_assigned_and_nested(ast, assigned, nested)

    # For each nested function...
    for funcnode in nested:
        # Check for captured variables in this function.
        captured = set()
        collect_captured(funcnode, assigned, captured, False)
        for name in captured:
            # We want to report the outermost capturing function
            # (since that is where the workaround will need to be
            # added), and the topmost assignment to the variable.
            # Just one report per capturing function per variable
            # will do.
            results.append(make_result(funcnode, name, assigned[name]))

        # Check each node in the function body in case it
        # contains another 'for' loop.
        childnodes = funcnode.getChildNodes()[len(funcnode.defaults):]
        for child in childnodes:
            check_ast(funcnode, results)

def collect_assigned_and_nested(ast, assigned, nested):
    """
    Collect the names assigned in this loop, not including names
    assigned in nested functions. Also collect the nodes of functions
    that are nested one level deep.
    """
    if isinstance(ast, AssName):
        if ast.name not in assigned or assigned[ast.name] > ast.lineno:
            assigned[ast.name] = ast.lineno
    else:
        childnodes = ast.getChildNodes()
        if isinstance(ast, (Lambda, Function)):
            nested.add(ast)

            # The default argument expressions are "outside" the
            # function, even though they are children of the
            # Lambda or Function node.
            childnodes = childnodes[:len(ast.defaults)]

        for child in childnodes:
            if isinstance(ast, Node):
                collect_assigned_and_nested(child, assigned, nested)

def collect_captured(ast, assigned, captured, in_function_yet):
    """Collect any captured variables that are also in assigned."""
    if isinstance(ast, Name):
        if ast.name in assigned:
            captured.add(ast.name)
    else:
        childnodes = ast.getChildNodes()
        if isinstance(ast, (Lambda, Function)):
            # Formal parameters of the function are excluded from
            # captures we care about in subnodes of the function body.
            new_assigned = assigned.copy()
            remove_argnames(ast.argnames, new_assigned)

            if len(new_assigned) > 0:
                for child in childnodes[len(ast.defaults):]:
                    collect_captured(child, new_assigned, captured, True)

            # The default argument expressions are "outside" *this*
            # function, even though they are children of the Lambda or
            # Function node.
            if not in_function_yet:
                return
            childnodes = childnodes[:len(ast.defaults)]

        for child in childnodes:
            if isinstance(ast, Node):
                collect_captured(child, assigned, captured, True)


def remove_argnames(names, fromset):
    for element in names:
        if element in fromset:
            del fromset[element]
        elif isinstance(element, (tuple, list)):
            remove_argnames(element, fromset)


def make_result(funcnode, var_name, var_lineno):
    if hasattr(funcnode, 'name'):
        func_name = 'function %r' % (funcnode.name,)
    else:
        func_name = '<lambda>'
    return (funcnode.lineno, func_name, var_name, var_lineno)

def report(out, path, results):
    for r in results:
        print >>out, path + (":%r %s captures %r assigned at line %d" % r)

def check(sources, out):
    class Counts:
        n = 0
        processed_files = 0
        suspect_files = 0
        error_files = 0
    counts = Counts()

    def _process(path):
        results = check_file(path)
        if isinstance(results, SyntaxError):
            print >>out, path + (" NOT ANALYSED due to syntax error: %s" % results)
            counts.error_files += 1
        else:
            report(out, path, results)
            counts.n += len(results)
            counts.processed_files += 1
            if len(results) > 0:
                counts.suspect_files += 1

    for source in sources:
        print >>out, "Checking %s..." % (source,)
        if os.path.isfile(source):
            _process(source)
        else:
            for (dirpath, dirnames, filenames) in os.walk(source):
                for fn in filenames:
                    (basename, ext) = os.path.splitext(fn)
                    if ext == '.py':
                        _process(os.path.join(dirpath, fn))

    print >>out, ("%d suspiciously captured variables in %d out of %d file(s)."
                  % (counts.n, counts.suspect_files, counts.processed_files))
    if counts.error_files > 0:
        print >>out, ("%d file(s) not processed due to syntax errors."
                      % (counts.error_files,))
    return counts.n


sources = ['.']
if len(sys.argv) > 1:
    sources = sys.argv[1:]
if check(sources, sys.stderr) > 0:
    sys.exit(1)
