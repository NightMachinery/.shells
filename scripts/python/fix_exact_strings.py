#!/usr/bin/env python3
"""
fix_exact_strings.py

Split the input on newlines, check each string against a ground-truth list (also newline-split),
and replace any non-exact entries with the nearest match found in the ground truth using a library
(difflib). Outputs the potentially replaced lines to the requested destination.

Usage:
  python fix_exact_strings.py [--input=- --out=<defaults to input>] [--cutoff=0.6] [--cutoff-threshold=<defaults to --cutoff>] [--basename-fallback/--no-basename-fallback] [--backup/--no-backup] [--verbose=0] GROUND_TRUTH.txt

Notes:
- "-" for --input means stdin; "-" for --out means stdout.
- If input and output refer to the same file path, the script will:
    1) Read the entire input first,
    2) Process it,
    3) Backup the original file to "<path>.bak" unless --no-backup,
    4) Write the new content only after successful processing.
- All diagnostic output is written to stderr.

Behavior regarding thresholds:
- The similarity "cutoff" controls acceptance of a match. If the best match similarity is BELOW the
  effective threshold (see --cutoff-threshold), the line is REMOVED from output.
- If --basename-fallback is enabled and the full-string similarity is below threshold, the tool will
  try again using only path basenames (+extensions) for both the query and candidates.
  When multiple candidates share the same basename, the one with the highest full-path
  similarity to the query is chosen.

Performance:
- Basename projections of the candidates are cached **per candidates identity** (keyed by id(candidates))
  to avoid rebuilding on every call to `nearest`. Multiple distinct candidate lists are cached separately.
"""
from __future__ import annotations
from pynight.common_icecream import ic  # optional debug; safe if not used

import argparse
import difflib
import enum
import io
from dataclasses import dataclass
from pathlib import Path
import shutil
import sys
from typing import Iterable, List, Optional, Sequence, Tuple, Dict


# ----------------------------
# Data structures & Enums
# ----------------------------


class IOKind(enum.Enum):
    STDIN = "stdin"
    STDOUT = "stdout"
    FILE = "file"


@dataclass(frozen=True)
class TextLines:
    lines: List[str]
    ended_with_newline: bool


@dataclass(frozen=True)
class MatchResult:
    original: str
    replacement: str  # "" means "removed"
    is_exact: bool


# ----------------------------
# String matching service
# ----------------------------


class StringMatcher:
    """Encapsulates nearest-neighbor string matching via difflib.

    cutoff: float in [0,1]. A candidate is accepted only if its similarity >= cutoff.
    basename_fallback: if True and the best full-string match is below cutoff, retry
                       matching on basenames (Path(x).name) only.

    Removal rule: If after (optional) basename fallback the best similarity is still below
                  cutoff, return "" to indicate the input line should be removed.

    Caching:
      - Per candidates object (keyed by id(candidates)), cache:
          * a list of basenames aligned with the candidates sequence
          * a mapping basename -> list of indices in the candidates
    """

    def __init__(self, *, cutoff: float = 0.6, basename_fallback: bool = False) -> None:
        self.cutoff = cutoff
        self.basename_fallback = basename_fallback
        # key: id(candidates) -> (cand_bases, base_to_indices)
        self._basename_cache: Dict[int, Tuple[List[str], Dict[str, List[int]]]] = {}

    @staticmethod
    def _best_match(query: str, candidates: Sequence[str]) -> Tuple[str, float]:
        """Return (best_candidate, similarity_ratio). Ratio is in [0,1]."""
        # Shortlist to reduce comparisons; then pick true best from shortlist.
        shortlist = difflib.get_close_matches(query, candidates, n=5, cutoff=0.0)
        pool = shortlist if shortlist else candidates
        best = ""
        best_ratio = -1.0
        for c in pool:
            r = difflib.SequenceMatcher(None, query, c).ratio()
            if r > best_ratio:
                best, best_ratio = c, r
        return best, (0.0 if best_ratio < 0.0 else best_ratio)

    def _get_cached_basename_structures(
        self, candidates: Sequence[str]
    ) -> Tuple[List[str], Dict[str, List[int]]]:
        """Return (cand_bases, base_to_indices) from cache, building and storing per-key if needed."""
        key = id(candidates)
        cached = self._basename_cache.get(key)
        if cached is not None:
            return cached

        cand_bases: List[str] = []
        base_to_indices: Dict[str, List[int]] = {}
        for i, c in enumerate(candidates):
            b = Path(c).name
            cand_bases.append(b)
            base_to_indices.setdefault(b, []).append(i)

        self._basename_cache[key] = (cand_bases, base_to_indices)
        return cand_bases, base_to_indices

    def nearest(self, *, query: str, candidates: Sequence[str]) -> str:
        """Return the nearest candidate if it meets the cutoff, optionally retrying
        with basename-only comparison. Otherwise return an empty string to DROP the line.

        If candidates is empty, return the original query unchanged.
        """
        if not candidates:
            return query

        # 1) Full-string comparison
        best, ratio = self._best_match(query, candidates)
        if ratio >= self.cutoff:
            return best

        # 2) Optional basename fallback (uses cached projections; handles duplicate basenames)
        if self.basename_fallback:
            cand_bases, base_to_indices = self._get_cached_basename_structures(candidates)

            q_base = Path(query).name
            best_base, base_ratio = self._best_match(q_base, cand_bases)
            if base_ratio >= self.cutoff:
                indices = base_to_indices.get(best_base, [])
                if indices:
                    # Among same-basename candidates, choose the one closest to the ORIGINAL query (full path)
                    best_idx = max(
                        indices,
                        key=lambda i: difflib.SequenceMatcher(None, query, candidates[i]).ratio(),
                    )
                    return candidates[best_idx]

        # 3) Below threshold after all attempts -> DROP the line
        return ""


# ----------------------------
# I/O helpers
# ----------------------------


def _infer_in_kind(path: str) -> IOKind:
    if path == "-":
        return IOKind.STDIN
    return IOKind.FILE


def _infer_out_kind(path: str) -> IOKind:
    if path == "-":
        return IOKind.STDOUT
    return IOKind.FILE


def _read_all_text(
    *, src_path: Optional[Path], kind: IOKind, encoding: str = "utf-8"
) -> str:
    match kind:
        case IOKind.STDIN:
            return sys.stdin.read()
        case IOKind.FILE:
            assert src_path is not None
            return src_path.read_text(encoding=encoding)
        case IOKind.STDOUT:
            # Reading from STDOUT is not supported by design.
            raise RuntimeError("Unsupported read target: STDOUT")
        case _:
            raise ValueError(f"Unknown IOKind for read: {kind}")


def read_lines(*, src: str, encoding: str = "utf-8") -> TextLines:
    """Read text and split into lines without keeping line separators."""
    kind = _infer_in_kind(src)
    src_path = None if kind != IOKind.FILE else Path(src)
    text = _read_all_text(src_path=src_path, kind=kind, encoding=encoding)
    # str.splitlines() keeps behavior consistent across platforms.
    lines = text.splitlines()
    ended_with_newline = text.endswith("\n")
    return TextLines(lines=lines, ended_with_newline=ended_with_newline)


def write_text(*, dst: str, text: str, encoding: str = "utf-8") -> None:
    """Write text to destination. "-" means stdout."""
    kind = _infer_out_kind(dst)
    match kind:
        case IOKind.STDOUT:
            sys.stdout.write(text)
            sys.stdout.flush()
        case IOKind.FILE:
            Path(dst).write_text(text, encoding=encoding)
        case IOKind.STDIN:
            # Writing to STDIN is not supported by design.
            raise RuntimeError("Unsupported write target: STDIN")
        case _:
            raise ValueError(f"Unknown IOKind for write: {kind}")


# ----------------------------
# Core logic
# ----------------------------


class Fixer:
    """Performs exactness correction against a ground-truth list."""

    def __init__(self, *, matcher: Optional[StringMatcher] = None) -> None:
        self.matcher = matcher or StringMatcher()

    def process(
        self,
        lines: Sequence[str],
        *,
        ground_truth: Sequence[str],
        verbosity=0,
    ) -> List[MatchResult]:
        gt_list = list(ground_truth)
        # ic(gt_list[:3])
        gt_set = set(gt_list)

        results: List[MatchResult] = []
        for s in lines:
            if s in gt_set:
                results.append(MatchResult(original=s, replacement=s, is_exact=True))
            else:
                nearest = self.matcher.nearest(query=s, candidates=gt_list)
                # ic(s, nearest)
                if nearest == "":
                    vprint(
                        f"\n{s} ->\n[REMOVED: best similarity below cutoff]\n",
                        level=1,
                        verbosity=verbosity,
                    )
                else:
                    vprint(
                        f"\n{s} ->\n{nearest}\n",
                        level=1,
                        verbosity=verbosity,
                    )
                results.append(
                    MatchResult(original=s, replacement=nearest, is_exact=False)
                )
        return results


def materialize_output(
    *, results: Sequence[MatchResult], ended_with_newline: bool
) -> str:
    # Omit lines whose replacement is "" (treat as removed).
    replaced_lines = [r.replacement for r in results if r.replacement != ""]
    body = "\n".join(replaced_lines)
    if ended_with_newline and (replaced_lines or body == ""):
        return body + "\n"
    return body


# ----------------------------
# CLI
# ----------------------------


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Replace non-exact strings with their nearest match from a ground-truth list.",
    )
    parser.add_argument(
        "ground_truth",
        metavar="GROUND_TRUTH",
        help="Path to a text file containing ground-truth strings, one per line.",
    )
    parser.add_argument(
        "--input",
        default="-",
        help="Input file path or '-' for stdin (default: %(default)s).",
    )
    parser.add_argument(
        "--out",
        default=None,
        help="Output file path or '-' for stdout. Defaults to the same as --input if omitted.",
    )
    parser.add_argument(
        "--cutoff",
        type=float,
        default=0.9,
        help="Similarity cutoff in [0.0, 1.0] for considering a close match (default: %(default)s).",
    )
    parser.add_argument(
        "--basename-fallback",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="If the full-string match is below the threshold, retry matching using only basenames of paths (default: %(default)s).",
    )
    parser.add_argument(
        "--backup",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Create a '.bak' when overwriting the input file (default: %(default)s). Use --no-backup to disable.",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        help="Increase verbosity level. Use -v for basic info, -vv for detailed info, -vvv for debug info (default: %(default)s).",
    )
    # parser.add_argument(
    #     "--verbose",
    #     type=int,
    #     default=0,
    #     help="Verbosity level 0=silent, higher is more verbose (default: %(default)s).",
    # )
    return parser


def vprint(msg: str, *, level: int = 0, verbosity: int = 0) -> None:
    if verbosity >= level:
        print(msg, file=sys.stderr)


def main(
    argv: Optional[Sequence[str]] = None, *, matcher: Optional[StringMatcher] = None
) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    input_spec: str = args.input
    out_spec: Optional[str] = args.out
    if out_spec is None:
        out_spec = input_spec

    # Load ground truth.
    gt_path = Path(args.ground_truth)
    if not gt_path.exists():
        print(f"ERROR: ground truth file not found: {gt_path}", file=sys.stderr)
        return 2

    gt_text = gt_path.read_text(encoding="utf-8")
    ground_truth_lines = gt_text.splitlines()

    if not ground_truth_lines:
        vprint(
            "WARNING: ground truth is empty; inputs will remain unchanged.",
            level=0,
            verbosity=args.verbose,
        )

    # Read input fully before any writing.
    input_payload = read_lines(src=input_spec, encoding="utf-8")

    # Determine effective cutoff threshold.
    effective_cutoff = (
        float(args.cutoff)
    )

    # Process.
    effective_matcher = matcher or StringMatcher(
        cutoff=effective_cutoff,
        basename_fallback=bool(args.basename_fallback),
    )
    fixer = Fixer(matcher=effective_matcher)
    results = fixer.process(
        input_payload.lines,
        ground_truth=ground_truth_lines,
        verbosity=args.verbose,
    )

    output_text = materialize_output(
        results=results, ended_with_newline=input_payload.ended_with_newline
    )

    # If input == output and is a file, prepare backup and write.
    in_kind = _infer_in_kind(input_spec)
    out_kind = _infer_out_kind(out_spec)

    same_file_overwrite = (
        in_kind == IOKind.FILE
        and out_kind == IOKind.FILE
        and Path(input_spec).resolve() == Path(out_spec).resolve()
    )

    if same_file_overwrite:
        src_path = Path(input_spec)
        if args.backup:
            bak_path = Path(str(src_path) + ".bak")
            try:
                shutil.copyfile(src_path, bak_path)
                vprint(
                    f"Backed up '{src_path}' to '{bak_path}'",
                    verbosity=args.verbose,
                    level=0,
                )
            except Exception as e:
                print(
                    f"ERROR: failed to create backup '{bak_path}': {e}", file=sys.stderr
                )
                return 3

        try:
            write_text(dst=str(src_path), text=output_text, encoding="utf-8")
            vprint(
                f"Wrote updated content to '{src_path}'",
                verbosity=args.verbose,
            )
        except Exception as e:
            print(
                f"ERROR: failed to write output to '{src_path}': {e}", file=sys.stderr
            )
            return 4
    else:
        # Simple write to desired destination.
        try:
            write_text(dst=out_spec, text=output_text, encoding="utf-8")
            vprint(
                f"Wrote output to '{out_spec}'",
                verbosity=args.verbose,
            )
        except Exception as e:
            print(
                f"ERROR: failed to write output to '{out_spec}': {e}", file=sys.stderr
            )
            return 4

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
