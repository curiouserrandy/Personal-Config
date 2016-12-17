import argparse
import lldb
import shlex

def log(frame, bp_loc, variables):
    result = ""
    result += "Logging: "
    result += bp_loc.GetAddress().GetFunction().GetName()
    result += "("
    result += bp_loc.GetAddress().GetLineEntry().GetFileSpec().GetFilename()
    result += ":"
    result += str(bp_loc.GetAddress().GetLineEntry().GetLine())
    result += "): "
    for v in variables:
        result += '"%s"' % v
        result += ":"
        result += frame.EvaluateExpression(v).GetValue()
        result += " "
    result += "\n"
    print result
    return False

function_iterator = 0

# TODO: Define a python function that takes the following args when aliased
# from the command line:
#	-f <fnname> 
#	-l <l#>
#	-v <variable to log> # may be specified multiple times.
# 
# and sets a breakpoint that outputs:
# "Logging <functionname> (fnname:l#): <variable>: <value>"
# and continues whenever that location is hit.
def log_line(debugger, command, result, internal_dict):
    "Set a breakpoint for logging; prints information and continues."
    
    args=shlex.split(command)

    parser = argparse.ArgumentParser(
        prog='<embedded interpretter>',
        description='Parse args to log_line function.')
    parser.add_argument('-f', dest='filename', 
                        required=True, help="File containing line to log.")
    parser.add_argument('-l', dest='lineno', type=int, required=True,
                        help="Line to log.")
    parser.add_argument('-v', dest='variables', action='append',
                        help="Variable value to log.")

    args_ns = parser.parse_args(args=args)

    ## TODO: Shift over to using python arguments instead of going through
    ## command line.  Requires understanding how callbacks are used in python. 

    bp = debugger.GetSelectedTarget().BreakpointCreateByLocation(
       args_ns.filename, args_ns.lineno)
    bp.SetScriptCallbackBody(
        "logging.log(frame, bp_loc, " + `args_ns.variables` + ")\n" +
        "return False")

lldb.debugger.HandleCommand('command script add -f logging.log_line trace')
