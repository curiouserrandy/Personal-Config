#!/usr/bin/env python 
# At least on the mac, I really don't trust python to be in a consistent 
# place, so I'm using the path.

import re
import os.path
import sys
from optparse import OptionParser

sys.path.append('/Users/rdsmith/Sandboxen/python-dot')

import pythondot

enable_debugging = 0

## TODO(rdsmith): Enable surround parameter passing.

(program_directory, program_name) = os.path.split(sys.argv[0])
help_string = """
%s is a program specialized for creating a state transition diagram for
HttpCacheTransaction.  It takes either one or two arguments.  All arguments
should be file paths to the http_cache_transaction.cc file.  With one 
argument a simple state transition diagram is created; with two, the 
differences in the state transition diagram are shown.
""" % program_name

usage_string = "%s basefile [newfile]"

parser = OptionParser(description=help_string, usage=usage_string)
parser.add_option("-?", action="help")
parser.add_option("-D", "--debug", help="Enable script debugging",
                  type="int", dest="debug", default=0, metavar="DBGLVL")
## Further options here; template follows.  
## All args are optional; if action is store_true/false, no extra
## args are consumed.
## parser.add_option({opts-short or long}, ..., dest={}, help={},
##		     type={}, default={}, metavar={},
##		     nargs={}, choices={}, callback={}, 

(options, args) = parser.parse_args()

# Process any non-flag arguments
if len(args) > 2:
    print >> sys.stderr, "Expected usage: %s" % usage_string
    sys.exit(2)

fn_re = re.compile("^[a-z]")
state_re = re.compile("(next_state_ = |TransitionToState\\()STATE_([A-Z_]*)")
check_state_re = re.compile("next_state_ = |TransitionToState")
function_name_re = re.compile("HttpCache::Transaction::([A-Za-z]*)\(")

def function_name(function_line):
    mo = function_name_re.search(function_line)
    if not mo:
        return function_line[:-1]
    return mo.group(1)

def process_function(function_name):
    if (function_name[0:2] == "Do" and function_name[2].isupper() and
        function_name <> "DoLoop"):
        return re.sub("([A-Z])", "_\\1", function_name).upper()[4:]
    return function_name

## TODO(rdsmith): Make handle stdin & multiple files properly.

## TODO(rdsmith) Raise a flag of some sort for direct calls to state
## functions/make a distinction between direct and state transition calls. 
def make_network(filename):
    """General a network (as defined in python-dot/lib.py) representing the
passed filename.  That network will embody a call graph, with the following
modifications:
	* Functions implementing a particular state will instead of the 
 	  function name use the name of the state.
	* Setting states will be treated as function calls.
	* Calls to and from DoLoop are ignored."""

    functions = []
    ## First pass to get list of functions
    for line in open(filename):
        if fn_re.search(line):
            mo = function_name_re.search(line)
            if mo:
                fname = mo.group(1)
                fname = function_name(line)
                functions.append(fname)
                continue

    ## Construct RE catching functions
    defined_functions_re = \
        re.compile("\\b(?<![.>])(" + "|".join(functions) + ")\\(")

    ## Second pass to construct first data structure.  This does only
    ## functions and notes state transitions separately.

    functions_to_states = []

    node_list = functions
    edge_list = []
    last_source = ""
    for line in open(filename):
        if "//" in line:
            line = line[:line.find("//")]
            line += "\n"
        if not line:
            continue
        if fn_re.search(line):
            mo = function_name_re.search(line)
            if mo:
                last_source = mo.group(1)
                continue

        if line[0] == '}':
            last_source = ""
            continue

        if not last_source:
            continue

        mo = state_re.search(line)
        if mo:
            # Special case for multi-line ?: instance.
            if last_source == "DoDoomEntryComplete":
                functions_to_states.append(
                    (last_source, "HEADERS_PHASE_CANNOT_PROCEED"))
                functions_to_states.append(
                    (last_source, "CREATE_ENTRY"))
                continue
            functions_to_states.append((last_source, mo.group(2)))
            continue

        mo = defined_functions_re.search(line)
        if mo:
            edge_list.append((last_source, mo.group(1)))

    ## Network editing

    ## Returns the name to use instead of the provided name, or None
    ## if this node/edge shouldn't be included in the network.
    def name_filter(name):
        if name == "DoLoop":
            return None

        name = process_function(name)
        
        if name == "CACHE_TOGGLE_UNUSED_SINCE_PREFETCH":
            return "TOGGLE_UNUSED_SINCE_PREFETCH"
        if name == "CACHE_TOGGLE_UNUSED_SINCE_PREFETCH_COMPLETE":
            return "TOGGLE_UNUSED_SINCE_PREFETCH_COMPLETE"

        return name

    new_node_list = []
    new_edge_list = []
    node_list = filter(lambda n: n is not None,
                       [name_filter(n) for n in node_list])
    edge_list = filter(lambda e: e[0] is not None and e[1] is not None,
                       [(name_filter(e[0]), name_filter(e[1]))
                        for e in edge_list])
    functions_to_states = filter(lambda e: (e[0] is not None and
                                            e[1] is not None),
                                 [(name_filter(e[0]), name_filter(e[1]))
                                  for e in functions_to_states])

    # Raise a flag if there are any calls directly to state functions
    for e in edge_list:
        if state_re.search(e[1]):
            print >> sys.stderr, ("Found direct call to state function: %s -> %s"
                                  % (e[0], e[1]))

    # Add in state transitions
    edge_list += functions_to_states

    # Prune entries that don't lead to a state transition
    modified = True
    raw_state_re = re.compile("^[A-Z_]+$")
    while modified:
        modified = False
        from_nodes = set([e[0] for e in edge_list])
        num_edges = len(edge_list)
        edge_list = [e for e in edge_list
                     if e[1] in from_nodes or raw_state_re.match(e[1])]
        modified = num_edges != len(edge_list)

    node_list = list(set([e[0] for e in edge_list] + [e[1] for e in edge_list]))

    return { 'nodes': node_list, 'edges' : edge_list }
       
## TODO(rdsmith): Handle collapsing intermediate functions between state
## transition functions.
## Also see above about marking function calls and state transitions
## differently.

if len(args) == 1:
    ## Just one argument, make a simple map
    network = make_network(args[0])
    pythondot.output_dotfile(open("tmp.dot", "w"), network)
elif len(args) == 2:
    ## Create a diff between the two maps
    network1 = make_network(args[0])
    network2 = make_network(args[1])
    pythondot.output_difference_dotfile(open("tmp.dot", "w"), network1, network2, 1)

os.system("open tmp.dot")

    
