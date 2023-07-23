#!/usr/bin/env python3
#: * @todo
#: ** @topublish
#:
#: ** We can also monitor the GPU usage, CPU RAM, and CPU usage.
#: ** add --color mode
#:
#: * @initialPrompt Write a Python script that launches a command and monitors its CUDA memory usage. It should do the monitoring every `--interval` seconds, and should return a JSON that contains all the sampled memory usages along with the maximum usage.
##
import argparse
import json
import psutil
import subprocess
import time
from pynight.common_dict import simple_obj
from pynight.common_files import mkdir
import pynvml
import humanize
from icecream import ic, colorize as ic_colorize

ic.configureOutput(outputFunction=lambda s: print(ic_colorize(s)))

# initialize NVML library
pynvml.nvmlInit()


def get_gpu_memory(pids):
    """Get the current GPU memory usage of a given process."""
    pids = set(pids)
    pid_memories = {}

    # Iterate over all GPUs
    for i in range(pynvml.nvmlDeviceGetCount()):
        handle = pynvml.nvmlDeviceGetHandleByIndex(i)
        info = pynvml.nvmlDeviceGetMemoryInfo(handle)

        # Iterate over all processes running on the GPU
        for proc in pynvml.nvmlDeviceGetComputeRunningProcesses(handle):
            if proc.pid in pids:
                pid_memories[proc.pid] = int(proc.usedGpuMemory) #: in bytes

    total_memory = 0
    for pid, pid_memory in pid_memories.items():
        total_memory += pid_memory

    return simple_obj(
        total_memory=total_memory,
        pid_memories=pid_memories,
    )


def get_child_processes(parent_pid):
    """Get all child processes of a given process."""
    parent = psutil.Process(parent_pid)
    return parent.children(recursive=True)


def monitor_process(process, interval, verbose):
    """Monitor the memory usage of a process and its children."""
    pid = process.pid

    usages = []
    max_usage = 0
    usages_all = []
    max_usage_all = 0

    print(f"\nmonitoring process {pid} ...\n")

    #: [[https://stackoverflow.com/questions/2995983/using-subprocess-wait-and-poll][python - Using subprocess wait() and poll() - Stack Overflow]]
    while process.poll() is None:
        children = get_child_processes(pid)
        usage = get_gpu_memory([pid] + [child.pid for child in children])

        if verbose:
            pid_memories_humanized = {
                pid: humanize.naturalsize(pid_memory, binary=True) for pid, pid_memory in usage.pid_memories.items()
            }
            ic(pid_memories_humanized)

        usage = usage.total_memory
        usages.append(usage)
        if usage > max_usage:
            max_usage = usage

        usage_all = pynvml.nvmlDeviceGetMemoryInfo(
            pynvml.nvmlDeviceGetHandleByIndex(0)
        ).used
        usages_all.append(usage_all)
        if usage_all > max_usage_all:
            max_usage_all = usage_all

        time.sleep(interval)

    print(f"\nprocess {pid} exited\n")

    return usages, max_usage, usages_all, max_usage_all


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--interval", type=int, default=1, help="Sampling interval in seconds"
    )
    parser.add_argument("--output", type=str, default="-", help="Output file")
    parser.add_argument(
        "--verbose",
        type=bool,
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Whether to print additional info",
    )
    parser.add_argument("command", nargs=argparse.REMAINDER, help="Command to launch")
    args = parser.parse_args()

    # check if command was given
    if not args.command:
        parser.error("You must specify a command to launch!")

    if args.command[0] == "--":
        # remove the "--" from the command
        command = args.command[1:]

    # launch the command
    process = subprocess.Popen(command)

    # monitor the memory usage
    usages, max_usage, usages_all, max_usage_all = monitor_process(
        process, args.interval, verbose=args.verbose
    )

    # finalize NVML
    pynvml.nvmlShutdown()

    # create results
    results = {
        "pid": process.pid,
        "usages": usages,
        "max_usage": max_usage,
        "max_usage_humanized": humanize.naturalsize(max_usage, binary=True),
        "usages_all": usages_all,
        "max_usage_all": max_usage_all,
        "max_usage_all_humanized": humanize.naturalsize(max_usage_all, binary=True),
    }

    # save or print results
    if args.output != "-":
        mkdir(args.output, do_dirname=True)
        with open(args.output, "w") as f:
            json.dump(results, f, indent=2)
    else:
        print(json.dumps(results, indent=2))


if __name__ == "__main__":
    main()
