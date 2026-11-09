#!/usr/bin/env python3
"""Verify public replication source hashes, original R3 output manifest and gates."""
from pathlib import Path
import json,csv,hashlib,argparse,math
ROOT=Path(__file__).resolve().parents[1]

def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()

def readcsv(p):
    with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))

def verify():
    m=json.loads((ROOT/'RELEASE_MANIFEST.json').read_text())
    files=m['files_sha256']
    observed={p.relative_to(ROOT).as_posix() for p in ROOT.rglob('*') if p.is_file() and '.git' not in p.parts and '__pycache__' not in p.parts}
    if observed != set(files)|{'RELEASE_MANIFEST.json'}:
        raise ValueError(f'Manifest membership error: missing={sorted(set(files)-observed)}, extra={sorted(observed-(set(files)|{"RELEASE_MANIFEST.json"}))}')
    for name,h in files.items():
        if sha(ROOT/name)!=h:raise ValueError(f'Manifest mismatch: {name}')
    for name,h in m['frozen_scientific_sha256'].items():
        if sha(ROOT/name)!=h:raise ValueError(f'Original scientific file modified: {name}')
    rows=readcsv(ROOT/'output/release_244/output_manifest_244.csv')
    assert len(rows)==22
    for row in rows:
        p=ROOT/'output/release_244'/row['file']
        if sha(p)!=row['sha256'] or p.stat().st_size!=int(row['bytes']):
            raise ValueError(f'Frozen R3 output mismatch: {p}')
    gates=readcsv(ROOT/'output/release_244/validation_report_244.csv')
    assert len(gates)==28 and all(x['status']=='PASS' for x in gates)
    assert m['expected_region_count']==244 and m['expected_validation_gates']==28
    assert 'DATA_ATTRIBUTION.md' in files
    print(f'PASS: {len(files)} file hashes, 37 original-science SHA hashes, 22 output payloads, 28 frozen R3 gates')

def compare(directory):
    original=ROOT/'output/release_244'
    run=Path(directory)
    for record in readcsv(original/'output_manifest_244.csv'):
        name=record['file']
        if not name.endswith('.csv') or name in ('data_vintage_metadata_244.csv','output_manifest_244.csv'):continue
        l=readcsv(original/name);r=readcsv(run/name)
        if len(l)!=len(r) or (l and list(l[0])!=list(r[0])):raise ValueError('CSV schema mismatch: '+name)
        for i,(a,b) in enumerate(zip(l,r),2):
            for k,v in a.items():
                if v==b[k]:continue
                try:
                    x=float(v);y=float(b[k]);
                    if math.isfinite(x) and math.isfinite(y) and abs(x-y)<=1e-8*max(1,abs(x),abs(y)):continue
                except (ValueError,TypeError):pass
                raise ValueError(f'R3 mismatch: {name}:{i} {k}')
    gates=readcsv(run/'validation_report_244.csv')
    if len(gates)!=28 or any(x['status']!='PASS' for x in gates):raise ValueError('R3 validation failed')
    print('PASS: independent R3 validation (28 gates) and scientific CSV comparison')

if __name__=='__main__':
    ap=argparse.ArgumentParser();ap.add_argument('--compare-rerun',default=None)
    opts=ap.parse_args();verify()
    if opts.compare_rerun:compare(opts.compare_rerun)
