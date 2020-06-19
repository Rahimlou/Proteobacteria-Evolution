#! /usr/bin/env python
import argparse
import subprocess
import os
def Do_Alignment2(seqfile, in_path, out_path):
    asmbl = os.listdir(in_path)
    for line in asmbl:
        f_content = line
        f_content_out = line + ".out"
        in_dir = os.path.join(in_path, f_content)
        out_dir = os.path.join(out_path, f_content_out)
        subprocess.call(["nhmmer", "-A", out_dir, seqfile, in_dir])
        
def main():
    parser=argparse.ArgumentParser(description="Making database out of a genome assembly")
    parser.add_argument("-seqfile", help="Query sequence file directory.", type=str, required=True)
    parser.add_argument("-in_path", help="Assembly files directory.", type=str, required=True)
    parser.add_argument("-out_path", help="Output files directory.", type=str, required=True)
    args=parser.parse_args()
        
    Do_Alignment2(args.seqfile, args.in_path, args.out_path)

if __name__=="__main__":
	main()

