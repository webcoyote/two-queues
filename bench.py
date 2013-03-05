#!/usr/bin/env python

from os import mkdir
from os.path import join
from multiprocessing import cpu_count
from sys import stdout
import subprocess

# Add check_output; CentOS 6.3 uses Python 2.6, which doesn't have this
if "check_output" not in dir( subprocess ): # duck punch it in!
    def f(*popenargs, **kwargs):
        if 'stdout' in kwargs:
            raise ValueError('stdout argument not allowed, it will be overridden.')
        process = subprocess.Popen(stdout=subprocess.PIPE, *popenargs, **kwargs)
        output, unused_err = process.communicate()
        retcode = process.poll()
        if retcode:
            cmd = kwargs.get("args")
            if cmd is None:
                cmd = popenargs[0]
            raise subprocess.CalledProcessError(retcode, cmd)
        return output
    subprocess.check_output = f


def popen_args(filename, *args):
    """
    Returns the initial popen args for a given Python or Go file.
    """
    args = ["--quiet"] + list(args)
    lang = filename.split(".")[-1]
    if lang == "py":
        return ["python", filename] + list(args)
    elif lang == "go":
        return ["go", "run", filename] + list(args)
    elif lang == "exs":
        return ["mix", "run"] + list(args)
    else:
        raise NameError('Unknown extension', lang)

def run_clients(lang, *args):
    """
    Runs the test_client program for Python or Go, for the range
    from 1 to cpus * 2 as the number of clients, returning the
    median messsages per second for each.
    """
    cmd = popen_args("test_client.%s" % lang, *args)
    print " ".join(cmd)

    broker = None
    if lang == "exs":
        subprocess.check_output(["mix", "compile"], stderr=subprocess.PIPE)
    elif "--redis" not in args:
        broker = subprocess.Popen(popen_args("run_broker.%s" % lang), stderr=subprocess.PIPE)

    results = []
    num_runs = cpu_count() * 2
    for clients in range(1, num_runs + 1):
        bar = ("#" * clients).ljust(num_runs)
        stdout.write("\r[%s] %s/%s " % (bar, clients, num_runs))
        stdout.flush()
        out = subprocess.check_output(cmd + ["--num-clients=%s" % clients] + ["--num-seconds=10"], stderr=subprocess.PIPE)
        results.append(out.split(" ")[0].strip())
    stdout.write("\n")

    if broker is not None:
        broker.kill()
    return results

# All test_client runs and their cli args.
runs = {
    "elixir": ["exs"],
    "py_redis": ["py", "--redis", "--unbuffered"],
    "py_redis_buffered": ["py", "--redis"],
    "py_zmq": ["py"],
    "go_redis": ["go", "--redis"],
    "go_zmq": ["go"],
}

# Consistent graph colours defined for each of the runs.
colours = {
    "elixir": "cyan",
    "py_redis": "red",
    "py_redis_buffered": "green",
    "py_zmq": "blue",
    "go_redis": "violet",
    "go_zmq": "orange",
}

# Groups of runs mapped to each graph.
plots = {
    "two-queues-1": ["py_zmq", "py_redis"],
    "two-queues-2": ["py_zmq", "py_redis", "py_redis_buffered"],
    "two-queues-3": ["py_zmq", "py_redis", "py_redis_buffered",
                     "go_zmq", "go_redis", "elixir"],
}

# Store all results in an output directory.
output_path = lambda s="": join("output", s)
try:
    mkdir(output_path())
except OSError:
    pass

# Store results from each test_client run into files.
for name, args in runs.items():
    with open(output_path(name + ".dat"), "w") as f:
        f.write("\n".join(run_clients(*args)))

# Generate graphs.
with open("plot.p", "r") as f:
    plotfile = f.read()
line = '"output/%s.dat" using ($0+1):1 with lines title "%s" lw 2 lt rgb "%s"'
for name, names in plots.items():
    name = output_path(name)
    with open(output_path(names[0] + ".dat"), "r") as f:
        clients = len(f.read().split())
    with open(name + ".p", "w") as f:
        lines = ", ".join([line % (l, l.replace("_", " "), colours[l])
                           for l in names])
        f.write(plotfile % {"name": name, "lines": lines, "clients": clients})
    subprocess.Popen(["gnuplot", name + ".p"], stderr=subprocess.PIPE)
