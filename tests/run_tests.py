#!/usr/bin/env python

import os, subprocess

# Go to base dir
os.chdir(os.path.dirname(os.path.realpath(__file__)) + "/..")

if not os.path.exists("./build"):
    subprocess.run(["cmake", "-Bbuild", os.getcwd()])

subprocess.run(["cmake", "--build", "build"])

if os.name == "nt":
    if os.path.exists("build/tsc.exe"):
        compiler_exe = "build/tsc.exe"
    elif os.path.exists("build/Debug/tsc.exe"):
        compiler_exe = "build/Debug/tsc.exe"
    elif os.path.exists("build/Release/tsc.exe"):
        compiler_exe = "build/Release/tsc.exe"
else:
    compiler_exe = "./build/tsc"

failed_tests = []

def run_proc(cmd_line):
    print("     Running:", cmd_line)
    return subprocess.run(cmd_line.split(" ")).returncode == 0

def test_dir(dir, expected_result):
    if expected_result:
        print("\n=== Running VALID tests ===")
    else:
        print("\n=== Running INVALID tests ===")

    outdir = dir + '_out'

    for filename in os.listdir(dir):
        splitname = os.path.splitext(filename)
        if splitname[1] != ".hlsl":
            continue

        no_ext_name = splitname[0]
        fullpath = os.path.join(dir, filename)
        out_path = os.path.join(outdir, no_ext_name + ".spv")
        dxc_out_path = os.path.join(outdir, no_ext_name + ".dxc.spv")

        stage = None
        dxc_stage = None
        if no_ext_name.endswith(".frag"):
            stage = "fragment"
            dxc_stage = "ps_6_0"
        elif no_ext_name.endswith(".vert"):
            stage = "vertex"
            dxc_stage = "vs_6_0"
        elif no_ext_name.endswith(".comp"):
            stage = "compute"
            dxc_stage = "cs_6_0"
        else:
            continue

        print("  => Testing:", fullpath)

        failed = False

        # Run DXC
        success = run_proc(
            f"dxc {fullpath} -Od -E main -T {dxc_stage} -Fo {dxc_out_path} -spirv -fspv-target-env=vulkan1.1")
        if success != expected_result:
            failed = True

        if success:
            success = run_proc(f"spirv-val {dxc_out_path}")
            if success != expected_result: failed = True

        # Run tinyshader
        success = run_proc(
            f"{compiler_exe} -E main -T {stage} -o {out_path} {fullpath}")
        if success != expected_result:
            failed = True

        if success:
            success = run_proc(f"spirv-val {out_path}")
            if success != expected_result: failed = True


        if failed:
            failed_tests.append(fullpath)

test_dir("./tests/valid", True)
test_dir("./tests/invalid", False)

print("\n=== RESULTS ===")

if len(failed_tests) > 0:
    print("Tests failed:")
    for f in failed_tests:
        print(f)
    exit(1)
else:
    print("All tests passed")
    exit(0)
