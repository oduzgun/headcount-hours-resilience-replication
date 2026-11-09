#!/usr/bin/env python3
"""Build the compact RSRS Early Career online appendix from frozen results."""

from __future__ import annotations

import csv
import re
from decimal import Decimal, ROUND_HALF_UP
from pathlib import Path


TABLES = Path("prism-uploads/tables_244")
OUTPUT = Path("online_appendix_ecr.tex")
SECTOR_FIGURE = Path("output/figure_3_sector_wedges.pdf")


def read_table(filename: str) -> list[dict[str, str]]:
    with (TABLES / filename).open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def esc(value: object) -> str:
    replacements = {
        "\\": r"\textbackslash{}",
        "&": r"\&",
        "%": r"\%",
        "$": r"\$",
        "#": r"\#",
        "_": r"\_",
        "{": r"\{",
        "}": r"\}",
        "~": r"\textasciitilde{}",
        "^": r"\textasciicircum{}",
    }
    return re.sub(r"[\\&%$#_{}~^]", lambda match: replacements[match.group()], str(value))


def breakable_hash(value: str, chunk_size: int = 8) -> str:
    chunks = [value[index : index + chunk_size] for index in range(0, len(value), chunk_size)]
    return r"\texttt{" + r"\allowbreak{}".join(chunks) + "}"


def num(value: str, digits: int = 2) -> str:
    if value is None or value == "":
        return "--"
    return f"{float(value):.{digits}f}"


def integer(value: str) -> str:
    if value is None or value == "":
        return "--"
    return f"{int(float(value)):,}"


def pct(value: str, digits: int = 1) -> str:
    if value is None or value == "":
        return "--"
    return f"{float(value):.{digits}f}\\%"


def pct_half_up(value: str, digits: int = 1) -> str:
    if value is None or value == "":
        return "--"
    quantum = Decimal(1).scaleb(-digits)
    rounded = Decimal(value).quantize(quantum, rounding=ROUND_HALF_UP)
    return f"{rounded:.{digits}f}\\%"


def stability_interval(low: str, high: str, digits: int = 2) -> str:
    if not low or not high:
        return "--"
    return f"[{num(low, digits)}, {num(high, digits)}]"


def period(value: str) -> str:
    return {
        "acute": "2019--2020",
        "adaptation": "2020--2021",
        "pandemic_net": "2019--2021",
    }.get(value, value.replace("-", "--"))


def sample_label(value: str) -> str:
    return {
        "Complete 2019-2021 event universe": "Event universe",
        "Former common 2008-2009/2019-2023 sample": "Former common sample",
        "Strictly balanced 2000-2021 panel": "Balanced panel",
    }.get(value, value)


def tex_row(cells: list[str]) -> str:
    return " & ".join(cells) + r" \\"


def longtable(
    caption: str,
    label: str,
    columns: str,
    header: str,
    body: list[str],
    notes: str | None = None,
) -> str:
    ncols = len(columns.replace("@{}", "").replace("L", "p").split())
    # Explicit column counts are more reliable than parsing LaTeX column specs.
    ncols = header.count("&") + 1
    out = [
        r"\begin{longtable}{" + columns + "}",
        r"\caption{" + caption + r"}\label{" + label + r"}\\",
        r"\toprule",
        header + r" \\",
        r"\midrule",
        r"\endfirsthead",
        r"\multicolumn{" + str(ncols) + r"}{c}{\tablename\ \thetable\ continued}\\",
        r"\toprule",
        header + r" \\",
        r"\midrule",
        r"\endhead",
        r"\midrule",
        r"\multicolumn{" + str(ncols) + r"}{r}{Continued on next page}\\",
        r"\endfoot",
        r"\bottomrule",
    ]
    if notes:
        out.append(
            r"\multicolumn{"
            + str(ncols)
            + r"}{p{0.96\linewidth}}{\footnotesize Notes: "
            + notes
            + r"}\\"
        )
    out.extend([r"\endlastfoot", *body, r"\end{longtable}"])
    return "\n".join(out)


def validate_comparability_rows(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    """Validate the imported R3 robustness rows without changing their content."""
    required = {
        "Main 2019-2021 event universe",
        "Excluding capital regions",
        "Excluding capital and outermost regions",
        "Excluding Croatia's regional 2020 break",
        "Excluding nine NUTS-2024-affected regions",
        "Excluding NUTS-2024-affected regions and Croatia's break",
        "Conservative endpoint b-flag exclusion (Poland and Croatia)",
    }
    labels = [row["restriction"] for row in rows]
    missing = required - set(labels)
    duplicates = {label for label in labels if labels.count(label) > 1}
    if missing or duplicates:
        raise ValueError(
            "Invalid imported R3 robustness rows: "
            f"missing={sorted(missing)}, duplicates={sorted(duplicates)}"
        )
    for row in rows:
        for field in ("regions", "spearman_rho", "broad_category_disagreement_pct"):
            if row[field]:
                float(row[field])
    return rows


def build() -> str:
    if not SECTOR_FIGURE.exists():
        raise FileNotFoundError(SECTOR_FIGURE)

    sample = read_table("table_1_sample_244.csv")
    metadata = read_table("data_vintage_metadata_244.csv")
    main_results = read_table("table_2_resilience_agreement_244.csv")
    decomposition = read_table("table_3_status_decomposition_244.csv")
    figure3 = read_table("figure_3_sector_data_244.csv")
    status = read_table("table_4c_status_contrasts_244.csv")
    historical = read_table("table_A1_historical_resilience_audit_244.csv")
    balanced = read_table("table_A1b_balanced_historical_resilience_audit_244.csv")
    break_history = read_table("table_A1c_end_year_break_audit_244.csv")
    robustness = validate_comparability_rows(
        read_table("table_A2a_acute_robustness_244.csv")
    )
    leave_one_out = read_table("table_A2b_leave_one_country_out_244.csv")
    minimum_cells = read_table("table_A3_minimum_cell_size_244.csv")
    sample_definition = read_table("table_A4_sample_definition_244.csv")
    validation = read_table("validation_report_244.csv")

    sample_map = {row["item"]: row["value"] for row in sample}
    meta_map = {row["field"]: row["value"] for row in metadata}
    if not validation or any(row["status"] != "PASS" for row in validation):
        raise ValueError("Imported R3 validation report does not contain only PASS gates")
    validation_map = {row["gate"]: row["observed"] for row in validation}
    if len(figure3) != 20:
        raise ValueError("Imported R3 Figure 3 data must contain exactly 20 rows")
    figure3_periods = {row["period"] for row in figure3}
    if figure3_periods != {"acute", "pandemic_net"}:
        raise ValueError("Imported R3 Figure 3 data must contain acute and pandemic_net only")
    if len(historical) != 23:
        raise ValueError("Imported R3 Table A1 must contain exactly 23 rows")

    event_row = next(row for row in main_results if row["period"] == "acute")
    historical_n = [int(row["regions"]) for row in historical]
    balanced_regions = int(meta_map["balanced_historical_regions"])
    balanced_countries = int(validation_map["Balanced historical countries"])
    break_pre = [row for row in break_history if int(row["end"]) < 2020]
    break_event = next(row for row in break_history if row["period"] == "2019-2020")
    break_extrema = {
        "rho": min(float(row["spearman_rho"]) for row in break_pre),
        "broad": max(float(row["broad_disagreement_pct"]) for row in break_pre),
        "median": max(float(row["median_rank_displacement"]) for row in break_pre),
        "p90": max(float(row["p90_rank_displacement"]) for row in break_pre),
    }
    endpoint_break_row = next(
        row
        for row in robustness
        if row["restriction"]
        == "Conservative endpoint b-flag exclusion (Poland and Croatia)"
    )
    acute_figure3 = [row for row in figure3 if row["period"] == "acute"]
    acute_positive_sectors = sum(
        float(row["country_median_D_logpp"]) > 0 for row in acute_figure3
    )
    acute_minimum_cells = [row for row in minimum_cells if row["period"] == "acute"]
    acute_cell_medians = [float(row["median_D_logpp"]) for row in acute_minimum_cells]
    maximum_cell_threshold = max(
        float(row["minimum_self_employed_thousand"]) for row in acute_minimum_cells
    )

    out: list[str] = [
        r"\documentclass[10pt,a4paper]{article}",
        r"\usepackage[margin=1.75cm]{geometry}",
        r"\usepackage[T1]{fontenc}",
        r"\usepackage[utf8]{inputenc}",
        r"\usepackage{lmodern}",
        r"\usepackage{microtype}",
        r"\usepackage{booktabs,longtable,array}",
        r"\usepackage{graphicx}",
        r"\usepackage{caption}",
        r"\usepackage[hidelinks]{hyperref}",
        r"\graphicspath{{output/}}",
        r"\setlength{\parindent}{0pt}",
        r"\setlength{\parskip}{0.35em}",
        r"\setlength{\tabcolsep}{2.2pt}",
        r"\setlength{\LTpre}{0.3em}",
        r"\setlength{\LTpost}{0.4em}",
        r"\renewcommand{\arraystretch}{1.02}",
        r"\newcolumntype{L}[1]{>{\raggedright\arraybackslash}p{#1}}",
        r"\captionsetup{font=small,labelfont=bf}",
        r"\title{Online Appendix to\\\textit{Headcount or Hours Resilience?\\Measure-Dependent Regional Rankings during the COVID-19 Shock}}",
        r"\author{Onur D{\"u}zg{\"u}n}",
        r"\date{August 2026}",
        r"\begin{document}",
        r"\maketitle",
        r"\tableofcontents",
        r"\clearpage",
        r"\appendix",
        r"\makeatletter",
        r"\@addtoreset{table}{section}",
        r"\@addtoreset{figure}{section}",
        r"\makeatother",
        r"\renewcommand{\thetable}{\Alph{section}\arabic{table}}",
        r"\renewcommand{\thefigure}{\Alph{section}\arabic{figure}}",
        r"\section{Data construction and release metadata}",
        (
            "The ECR appendix prioritises a compact audit trail. The primary "
            f"event universe is the complete set of {event_row['regions']} "
            "EU-27 NUTS-2 regions with positive persons and hours observations "
            "for 2019--2021. "
            "Historical comparisons use pair-specific coverage; the balanced "
            "panel is a stress test. Frozen CSVs and figure assets are not "
            "modified by this builder."
        ),
    ]

    merged_metadata = [
        ("Persons dataset", esc(meta_map["persons_dataset"])),
        ("Hours dataset", esc(meta_map["hours_dataset"])),
        ("Retrieval date", esc(meta_map["retrieval_date"])),
        ("Persons source updated", esc(meta_map["persons_source_updated"])),
        ("Hours source updated", esc(meta_map["hours_source_updated"])),
        ("Persons source MD5", esc(meta_map["persons_json_md5"])),
        ("Hours source MD5", esc(meta_map["hours_json_md5"])),
        ("Event regions", integer(meta_map["event_regions"])),
        ("Countries", integer(meta_map["countries"])),
        ("Historical years", f"{meta_map['first_year']}--{meta_map['last_year']}"),
        ("Balanced historical regions", integer(meta_map["balanced_historical_regions"])),
        ("Bootstrap repetitions", integer(meta_map["bootstrap_repetitions"])),
        ("Random seed", integer(meta_map["random_seed"])),
        (
            "Replication script SHA-256",
            breakable_hash(meta_map["replication_script_sha256"]),
        ),
        ("Sector-analysis provenance", esc(meta_map["sector_analysis_provenance"])),
        ("Acute total-economy pairs", integer(sample_map["Acute total-economy region pairs"])),
        ("Acute region-sector pairs", integer(sample_map["Acute region-sector pairs"])),
        (
            "Maximum status identity error, persons",
            esc(sample_map["Maximum EMP = SAL + SELF relative error, persons (%)"])
            + r"\%",
        ),
        (
            "Maximum status identity error, hours",
            esc(sample_map["Maximum EMP = SAL + SELF relative error, hours (%)"])
            + r"\%",
        ),
        ("Missing Great Recession region", esc(meta_map["historical_comparison_region_missing"])),
        ("Reason", esc(meta_map["historical_missing_reason"])),
    ]
    out.append(
        longtable(
            "Merged sample, vintage, and validation metadata",
            "tab:ecr-metadata",
            r"L{0.43\linewidth}L{0.51\linewidth}",
            "Item & Value",
            [tex_row([esc(key), value]) for key, value in merged_metadata],
            "Identity errors refer to 2019--2021.",
        )
    )

    out.extend(
        [
            r"\clearpage",
            r"\section{Historical measurement dependence}",
            (
                "Table~\\ref{tab:ecr-history-pair} retains the five quantities "
                "needed to audit historical unusualness. Coverage ranges from "
                f"{min(historical_n)} to {max(historical_n)} regions."
            ),
            r"\scriptsize",
        ]
    )
    history_body = [
        tex_row(
            [
                period(row["period"]),
                integer(row["regions"]),
                num(row["spearman_rho"], 3),
                pct(row["broad_disagreement_pct"]),
                num(row["median_rank_displacement"], 1),
                num(row["p90_rank_displacement"], 1),
            ]
        )
        for row in historical
    ]
    out.append(
        longtable(
            "Pair-specific historical persons--hours agreement",
            "tab:ecr-history-pair",
            "lrrrrr",
            r"Interval & $N$ & $\rho$ & Broad & Median & P90",
            history_body,
            "Broad is the percentage moving between the bottom quartile, middle "
            "half, and top quartile. Median and P90 are absolute rank positions. "
            "Rows after 2019--2020 are observed follow-up comparisons under "
            "overlapping shocks and are not interpreted as pandemic recovery.",
        )
    )
    out.append(r"\normalsize")

    pre_balanced = [row for row in balanced if int(row["end"]) < 2020]
    extrema = {
        "rho": min(float(row["spearman_rho"]) for row in pre_balanced),
        "broad": max(float(row["broad_disagreement_pct"]) for row in pre_balanced),
        "median": max(float(row["median_rank_displacement"]) for row in pre_balanced),
        "p90": max(float(row["p90_rank_displacement"]) for row in pre_balanced),
    }
    balanced_event = [
        row
        for row in sample_definition
        if row["sample"] == "Strictly balanced 2000-2021 panel"
    ]
    balanced_body = [
        tex_row(
            [
                "Pre-2020 extrema",
                str(balanced_regions),
                f"{extrema['rho']:.3f}",
                f"{extrema['broad']:.1f}\\%",
                f"{extrema['median']:.1f}",
                f"{extrema['p90']:.1f}",
            ]
        )
    ]
    for row in balanced_event:
        balanced_body.append(
            tex_row(
                [
                    period(row["period"]),
                    integer(row["regions"]),
                    num(row["spearman_rho"], 3),
                    pct(row["broad_disagreement_pct"]),
                    num(row["median_rank_displacement"], 1),
                    num(row["p90_rank_displacement"], 1),
                ]
            )
        )
    out.append(
        longtable(
            "Balanced-panel extrema and pandemic intervals",
            "tab:ecr-history-balanced",
            "lrrrrr",
            r"Comparison & $N$ & $\rho$ & Broad & Median & P90",
            balanced_body,
            "The pre-2020 row combines the minimum correlation with the maxima "
            "of the three disagreement statistics; the extrema need not occur in "
            "the same interval. The balanced panel contains "
            f"{balanced_regions} regions in {balanced_countries} countries.",
        )
    )
    out.append(
        "A separate pair-specific audit excluding end-year break flags gives a "
        f"pre-pandemic minimum correlation of {break_extrema['rho']:.4f} versus "
        f"{float(break_event['spearman_rho']):.4f} in 2019--2020; pre-pandemic "
        f"maxima of {break_extrema['broad']:.2f} percent for broad disagreement, "
        f"{break_extrema['median']:.1f} for median displacement, and "
        f"{break_extrema['p90']:.1f} for P90 compare with "
        f"{float(break_event['broad_disagreement_pct']):.2f} percent, "
        f"{float(break_event['median_rank_displacement']):.1f}, and "
        f"{float(break_event['p90_rank_displacement']):.1f} in the acute interval."
    )

    out.extend(
        [
            r"\clearpage",
            r"\section{Robustness and sample sensitivity}",
            (
                "Named restrictions retain the frozen result values and the "
                "completed geography-gate corrections. Leave-one-country-out "
                "correlations range from "
                f"{min(float(row['spearman_rho']) for row in leave_one_out):.3f} "
                "to "
                f"{max(float(row['spearman_rho']) for row in leave_one_out):.3f}."
            ),
            r"\scriptsize",
        ]
    )
    robust_body = []
    for row in robustness:
        if row["restriction"].startswith("Leave-one-country-out"):
            continue
        robust_body.append(
            tex_row(
                [
                    esc(row["restriction"]).replace("<=", r"$\leq$"),
                    integer(row["regions"]),
                    num(row["spearman_rho"], 3),
                    pct_half_up(row["broad_category_disagreement_pct"]),
                ]
            )
        )
    out.append(
        longtable(
            "Named restrictions for the acute ranking result",
            "tab:ecr-robustness",
            r"L{0.64\linewidth}rrr",
            r"Restriction & $N$ & $\rho$ & Broad",
            robust_body,
            f"The {integer(endpoint_break_row['regions'])}-region row removes "
            "endpoint break flags conservatively. All geography-gate rows are "
            "read unchanged from the imported R3 robustness table.",
        )
    )
    out.append(r"\normalsize")

    weighted_body = [
        tex_row(
            [
                period(row["period"]),
                pct(row["employment_weighted_eu_disagreement_pct"]),
                pct(row["employment_weighted_broad_category_disagreement_pct"]),
            ]
        )
        for row in main_results
    ]
    out.append(
        longtable(
            "Employment-weighted disagreement",
            "tab:ecr-weighted",
            "lrr",
            "Interval & EU benchmark & Broad category",
            weighted_body,
            "Weights are employed persons in the first year of each interval.",
        )
    )

    sample_body = [
        tex_row(
            [
                esc(sample_label(row["sample"])),
                period(row["period"]),
                integer(row["regions"]),
                num(row["spearman_rho"], 3),
                pct(row["broad_disagreement_pct"]),
            ]
        )
        for row in sample_definition
    ]
    out.append(
        longtable(
            "Compact event-sample sensitivity",
            "tab:ecr-sample-definition",
            r"L{0.28\linewidth}lrrr",
            r"Sample & Interval & $N$ & $\rho$ & Broad",
            sample_body,
            "The event universe is primary; the former common and balanced "
            "samples impose observations not required by the event estimand.",
        )
    )

    out.extend(
        [
            r"\clearpage",
            r"\section{Employment-status and sectoral anatomy}",
            (
                "These summaries locate the persons--hours gap. They do not "
                "additively decompose absolute regional rank displacement or "
                "explain individual rank movements."
            ),
        ]
    )
    eu_row = next(row for row in decomposition if row["level"] == "EU aggregate")
    shapley_body = [
        tex_row(["Employee average hours", num(eu_row["employee_hours_logpp"])]),
        tex_row(["Self-employed average hours", num(eu_row["self_employed_hours_logpp"])]),
        tex_row(["Employment-status composition", num(eu_row["composition_logpp"])]),
        tex_row(["Total persons--hours gap", num(eu_row["total_measurement_gap_logpp"])]),
    ]
    out.append(
        longtable(
            "EU Shapley accounting summary, 2019--2020",
            "tab:ecr-shapley",
            r"L{0.55\linewidth}r",
            "Component & Log percentage points",
            shapley_body,
            f"Self-employed persons were {float(eu_row['baseline_self_employment_pct']):.2f}\\% "
            f"of baseline employment and supplied {float(eu_row['self_employed_contribution_pct']):.1f}\\% "
            "of the acute decline in average realised hours.",
        )
    )

    status_body = []
    for row in status:
        if row["analysis_level"] == "EU aggregate":
            continue
        status_body.append(
            tex_row(
                [
                    period(row["period"]),
                    esc(row["analysis_level"].replace("NUTS-2 ", "")),
                    integer(row["observations"]),
                    num(row["median_D_logpp"]),
                    stability_interval(
                        row["median_stability_low_logpp"],
                        row["median_stability_high_logpp"],
                    ),
                    pct(row["share_positive_pct"]),
                    integer(row["countries_positive"]),
                ]
            )
        )
    out.append(r"\scriptsize")
    out.append(
        longtable(
            "Employment-status contrast summary",
            "tab:ecr-status",
            r"lL{0.24\linewidth}rrrrr",
            r"Interval & Level & $N$ & Median $D$ & 95\% interval & $D>0$ & Countries",
            status_body,
            "$D$ is in log percentage points. Intervals are bootstrap stability "
            "ranges, not superpopulation confidence intervals.",
        )
    )
    out.append(r"\normalsize")

    out.extend(
        [
            r"\clearpage",
            (
                r"Figure~\ref{fig:app-sector-wedges} reports the sector pattern "
                "removed from the ECR main manuscript. Acute medians are positive "
                f"in {acute_positive_sectors} of {len(acute_figure3)} sectors; "
                "agriculture is the exception."
            ),
            r"\begin{figure}[htbp]",
            r"\centering",
            r"\includegraphics[width=0.96\linewidth,clip]{figure_3_sector_wedges.pdf}",
            r"\caption{Employment-status contrasts by sector. Points are country-sector aggregate medians and horizontal lines are 95 percent bootstrap stability intervals. The contrast $D$ is the self-employed persons--hours gap minus the employee persons--hours gap; positive values indicate a larger decline, or smaller increase, in average realised hours among self-employed persons. Country coverage varies by sector.}",
            r"\label{fig:app-sector-wedges}",
            r"\end{figure}",
            r"\clearpage",
        ]
    )

    acute_sector = {row["nace_r2"]: row for row in figure3 if row["period"] == "acute"}
    net_sector = {
        row["nace_r2"]: row for row in figure3 if row["period"] == "pandemic_net"
    }
    sector_body = []
    for code, acute in acute_sector.items():
        net = net_sector[code]
        sector_body.append(
            tex_row(
                [
                    esc(acute["sector_name"]),
                    num(acute["country_median_D_logpp"]),
                    stability_interval(
                        acute["country_median_stability_low_logpp"],
                        acute["country_median_stability_high_logpp"],
                    ),
                    num(net["country_median_D_logpp"]),
                    stability_interval(
                        net["country_median_stability_low_logpp"],
                        net["country_median_stability_high_logpp"],
                    ),
                ]
            )
        )
    out.append(r"\scriptsize")
    out.append(
        longtable(
            "Compact sector medians",
            "tab:ecr-sector",
            r"L{0.34\linewidth}rL{0.20\linewidth}rL{0.20\linewidth}",
            r"Sector & Acute $D$ & Acute interval & Net $D$ & Net interval",
            sector_body,
            "Entries are country-sector medians in log percentage points and "
            "95 percent bootstrap stability intervals. Full country-sector "
            "machine-readable tables remain in the frozen CSV release.",
        )
    )
    out.append(r"\normalsize")

    out.append(r"\clearpage")
    cells_body = [
        tex_row(
            [
                period(row["period"]),
                integer(row["minimum_self_employed_thousand"]),
                integer(row["region_sector_pairs"]),
                num(row["median_D_logpp"]),
                pct(row["share_positive_pct"]),
                integer(row["sectors_positive"]),
            ]
        )
        for row in minimum_cells
    ]
    out.append(
        longtable(
            "Minimum self-employment cell-size sensitivity",
            "tab:ecr-minimum-cells",
            "lrrrrr",
            r"Interval & Minimum (000s) & Cells & Median $D$ & $D>0$ & Sectors",
            cells_body,
            "The threshold is baseline self-employed persons. Acute medians "
            f"remain between {min(acute_cell_medians):.2f} and "
            f"{max(acute_cell_medians):.2f} points through "
            f"{maximum_cell_threshold * 1000:,.0f} persons; net sector breadth "
            "weakens as the threshold rises.",
        )
    )

    out.extend(
        [
            r"\section{Release and replication status}",
            (
                f"The frozen R3 output release covers N={event_row['regions']} regions. "
                f"Its bundled validation report records {len(validation)} PASS gates "
                "from the external R 4.5.3 run; this appendix builder reads that "
                "report but does not rerun the gates or the R analysis. The release "
                "metadata records generating-script SHA-256 "
                + breakable_hash(meta_map["replication_script_sha256"])
                + ". The exact generating script is present as "
                r"\texttt{replication\_244.R}, and all six frozen inputs required "
                r"by that script are present under \texttt{data/frozen\_244/}. "
                "The recovered package was executed under R 4.5.3 on 1 September "
                "2026 and passed all 28 gates; this appendix builder reads the "
                "historical validation report but does not itself rerun R. Full "
                "country Shapley and country-sector summary tables remain available "
                "as frozen CSVs but are not repeated in the compact PDF. For the "
                "sector extension, cell statistics are recomputed from a validated "
                "N244 intermediate, while country-sector aggregates and minimum-cell-"
                "size checks use validated frozen release references because the "
                "complete original sector raw download is no longer intact. The "
                r"metadata field \texttt{input\_release\_status} is a historical "
                "frozen-input label, not the current execution status. A permanent "
                "repository identifier remains to be added on archival deposit."
            ),
            r"\end{document}",
        ]
    )
    return "\n\n".join(out) + "\n"


if __name__ == "__main__":
    OUTPUT.write_text(build(), encoding="utf-8")
    print(f"Wrote {OUTPUT} from frozen tables in {TABLES}")