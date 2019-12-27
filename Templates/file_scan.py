#!/usr/bin/env python

## Not a very clean script, but an example of what's needed to parse a set of files and
## handle possible multi-line portions in that file.  The naming disambiguation is nothing
## to write home about; if that's necessary for something else it should probably be rewritten
## in a cleaner, tighter fashion.

import re
import sys

mainmatch = re.compile("^class (LbL7|SniBasicTest|SslPolicyTest|BackendServiceLongConnectionTest)")

inheritance_map = {}
names = {}

prefix = """
  # Whole test needed
  node [style=filled, fillcolor=green];
  LbL7BasicTest;
  LbL7GfeNegTest;
  LbL7GfeTest;
  LbL7HttpsBasicTest;
  lb_l7_https_gfe_test_LbL7HttpsGFETest;
  LbL7MigrationTest;
  LbL7NegTest;
  LbL7QuicGFETest;
  LbL7RigTest;
  SniBasicTest;
  SslPolicyTest;

  # Partially needed
  node [style=filled, fillcolor=lightblue];
  BackendServiceLongConnectionTest;
  LbL7GfeNetworkingTest;

  # Need uncertain
  node [style=filled, fillcolor=yellow];
  LbL7GfeAffinityTest;

  # Everything else unneeded.
  node [style=solid];
"""

while len(sys.argv) > 1:
  fname = sys.argv[1]
  sys.argv = [sys.argv[0]] + sys.argv[2:]
  f = open(fname, 'r')
  fname = fname[:-3]
  for line in f:
    if not mainmatch.search(line):
      continue

    while ')' not in line:
      line = line[:-1]          # Remove trailing newline
      line += f.next()
    line = line[:-1]          # Remove trailing newline
    mo = re.match('class (\w*)\(\s*([\w\.]*)\s*\):', line)
    if not mo:
      print "Can't match line: ", line
      sys.exit(1)
    (derived, base) = mo.group(1,2)
    mo = re.match("^(\w+)\.(\w+)$", base)
    if mo:
      (baseContext, base) = mo.group(1,2)
    else:
      baseContext = fname
    assert "." not in base
    assert (fname, derived) not in inheritance_map, (fname, derived)
    # TODO: Support multiple inheritance
    inheritance_map[(fname, derived)] = (baseContext, base)
    # names is a two level dict defaulting to {} then 0, which
    # counts the number of times a particular context has been
    # recorded for a particular name.  The goal is actually tracking
    # the number of unique contexts; i.e. the data read from names is
    # len(names[name]) > 1.
    names.setdefault(derived, {}).setdefault(fname, 0)
    names[derived][fname] += 1
    names.setdefault(base, {}).setdefault(baseContext, 0)
    names[base][baseContext] += 1

print "digraph inheritance {"
print prefix
for k in inheritance_map:
  derived_name = k[0] + "_" + k[1] if len(names[k[1]]) > 1 else k[1]
  base = inheritance_map[k]
  base_name = base[0] + "_" + base[1] if len(names[base[1]]) > 1 else base[1]
  print "  ", derived_name, " -> ", base_name, ";"
print "}"
