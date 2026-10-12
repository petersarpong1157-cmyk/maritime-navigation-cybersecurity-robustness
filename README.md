# Maritime Navigation Cybersecurity: Alarm Persistence and Noise Shift

Reproducible R-based simulation study of alarm persistence,
sensor-noise distribution shift, and GPS spoofing detection
in maritime navigation.

## Study scope

The primary study uses a custom synthetic multi-sensor
navigation simulator. A separate supplementary evaluation
uses the independently developed MARSIM GPS-spoofing
simulation dataset.

The results do not establish operational detection performance
on physical vessels or real maritime navigation equipment.

## Repository structure

- `R/`: analysis and verification scripts
- `data/`: frozen inputs and processed benchmark data
- `results/`: numerical results and bootstrap outputs
- `docs/`: computational environment documentation

## Reproduction

### Primary study

The primary R simulation script is:
`R/maritime_analysis_reproduction.R`.
It contains the main experimental and statistical workflow.

### Paired noise-shift experiment

Install the required packages listed in `docs/SOFTWARE.md`.
From the repository root, run:

```r
source('R/verify_paired_reproduction.R')
```

The verification compares 36,000 observations against
the archived reference and reports
`PAIRED REPRODUCTION: PASS` when successful.

### MARSIM GPS-spoofing benchmark

The frozen selection manifest, thresholds, detector
configuration, held-out results, and bootstrap summaries
are included in this repository.

The processed dataset `marsim_benchmark_final.rds` is
temporarily excluded while redistribution conditions
are reviewed.

Consequently, `R/verify_marsim_benchmark.R` cannot run
from this repository alone at present.

The original MARSIM PCAP dataset is available from
https://zenodo.org/records/8202936.

See `docs/SOFTWARE.md` for the recorded environment.

## Main results

The primary synthetic experiment found that a detector
with low familiar-condition FPR could produce substantially
more false alarms under stronger unseen sensor noise.

The MARSIM GPS-only benchmark found:

- k=1: recall 8.14%; FPR 1.05%; F1 0.1463
- k=2: recall 2.69%; FPR 0.34%; F1 0.0520

The MARSIM results are not direct validation of the
primary multi-sensor detector.

## MARSIM dataset

The original MARSIM dataset is available at:
https://zenodo.org/records/8202936

Only selected benchmark metadata and processed analysis
data are included here. Original PCAP files are not included.

The benchmark selection uses disjoint parameter groups
for training, validation and held-out testing.

## Citation and archival status

A Zenodo DOI will be added after the GitHub repository
has been finalized and a versioned release archived.

## Limitations

The original multi-sensor data are entirely synthetic.
MARSIM is also a simulation-based dataset.
The external test contains six parameter groups,
limiting statistical generalization.
The MARSIM bootstrap intervals are conditional on
the selected simulation scenarios and parameter groups.

