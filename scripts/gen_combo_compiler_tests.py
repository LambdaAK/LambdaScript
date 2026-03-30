#!/usr/bin/env python3
"""Generate test/compiler_cases/combo_NNN_*.ls — run from repo root:
   python3 scripts/gen_combo_compiler_tests.py"""

from __future__ import annotations

import os
import pathlib
import subprocess
import sys
import tempfile


ROOT = pathlib.Path(__file__).resolve().parents[1]
OUT_DIR = ROOT / "test" / "compiler_cases"
# Use the built exe directly (avoid `dune exec` per case — very slow).
COMPILE_EXE = ROOT / "_build" / "default" / "bin" / "compile_lambdascript.exe"


def compile_and_run(source: str) -> tuple[bool, str]:
    """On success return (True, stdout). On failure (False, error_or_stderr)."""
    with tempfile.TemporaryDirectory(dir=ROOT) as tds:
        td = pathlib.Path(tds)
        src = td / "t.ls"
        exe = td / "out.bin"
        src.write_text(source.rstrip() + "\n", encoding="utf-8")
        if not COMPILE_EXE.is_file():
            return False, f"missing {COMPILE_EXE}; run: dune build bin/compile_lambdascript.exe"
        p = subprocess.run(
            [str(COMPILE_EXE), str(src), str(exe)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            timeout=120,
        )
        if p.returncode != 0:
            msg = (p.stderr or p.stdout or "").strip() or f"compile exit {p.returncode}"
            return False, msg
        p2 = subprocess.run(
            [str(exe)],
            cwd=td,
            capture_output=True,
            text=True,
            timeout=30,
        )
        if p2.returncode != 0:
            return False, (p2.stderr or p2.stdout or "exe failed").strip()
        out = p2.stdout
        if out.endswith("\n"):
            out = out[:-1]
        return True, out


def write_case(path: pathlib.Path, expected: str, source: str) -> None:
    path.write_text(
        f"Expected:\n{expected}\n\nSource:\n{source.rstrip()}\n",
        encoding="utf-8",
    )


# Exactly 100 (slug, source) pairs — stress poly, partial, HO, recursion, arith, bool.
PROGRAMS: list[tuple[str, str]] = [
    ("apply_twice_poly", "let apply f x = f x\nlet twice x = x * 2\nlet () = println (int_to_str (apply twice 21))"),
    ("compose_add2_mul3", "let compose f g x = f (g x)\nlet add2 x = x + 2\nlet mul3 x = x * 3\nlet () = println (int_to_str (compose mul3 add2 4))"),
    ("flip_sub_10_3", "let flip f x y = f y x\nlet sub a b = a - b\nlet () = println (int_to_str (flip sub 10 3))"),
    ("const_hundred", "let const x _ = x\nlet () = println (int_to_str (const 100 0))"),
    ("second_seventyseven", "let second _ y = y\nlet () = println (int_to_str (second 0 77))"),
    ("id_bound_eight", "let id x = x\nlet h = id\nlet () = println (int_to_str (h 8))"),
    ("use_id_five", "let use k = k 5\nlet id x = x\nlet () = println (int_to_str (use id))"),
    ("partial_add3_1_2_40", "let add3 x y z = x + y + z\nlet p = add3 1\nlet q = p 2\nlet () = println (int_to_str (q 40))"),
    ("partial_sub_xyz", "let f x y z = x - y + z\nlet a = f 20\nlet b = a 7\nlet () = println (int_to_str (b 4))"),
    ("rec_fact_6", "let rec fact n = if n == 0 then 1 else n * fact (n - 1)\nlet () = println (int_to_str (fact 6))"),
    ("rec_sum_12", "let rec sum n = if n == 0 then 0 else n + sum (n - 1)\nlet () = println (int_to_str (sum 12))"),
    ("rec_pow2_8", "let rec pow2 n = if n == 0 then 1 else 2 * pow2 (n - 1)\nlet () = println (int_to_str (pow2 8))"),
    ("rec_gcd_84_30", "let rec gcd a b = if b == 0 then a else gcd b (a % b)\nlet () = println (int_to_str (gcd 84 30))"),
    ("rec_fib_10", "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)\nlet () = println (int_to_str (fib 10))"),
    ("mutrec_even_100", "let rec is_even n = if n == 0 then true else is_odd (n - 1)\nand is_odd n = if n == 0 then false else is_even (n - 1)\nlet () = println (int_to_str (if is_even 100 then 1 else 0))"),
    ("mutrec_odd_99", "let rec is_even n = if n == 0 then true else is_odd (n - 1)\nand is_odd n = if n == 0 then false else is_even (n - 1)\nlet () = println (int_to_str (if is_odd 99 then 1 else 0))"),
    ("abs_neg15", "let abs x = if x < 0 then 0 - x else x\nlet () = println (int_to_str (abs (0 - 15)))"),
    ("sign_zero", "let sign x = if x > 0 then 1 else if x < 0 then 0 - 1 else 0\nlet () = println (int_to_str (sign 0))"),
    ("clamp_hi", "let clamp lo hi x = if x < lo then lo else if x > hi then hi else x\nlet () = println (int_to_str (clamp 0 10 200))"),
    ("min_max_seven", "let min a b = if a < b then a else b\nlet max a b = if a > b then a else b\nlet () = println (int_to_str (min (max 3 9) 7))"),
    ("paren_seven", "let () = println (int_to_str ((1 + 2) * (3 + 4)))"),
    ("div_100_4", "let () = println (int_to_str (100 / 4))"),
    ("mod_17_5", "let () = println (int_to_str (17 % 5))"),
    ("cmp_mul_eq", "let () = println (int_to_str (if (3 * 4) + 2 == 14 then 1 else 0))"),
    ("bool_and_gt", "let () = println (int_to_str (if 10 > 9 && 2 < 3 then 42 else 0))"),
    ("bool_or_ff_t", "let () = println (int_to_str (if false || true then 7 else 0))"),
    ("arith_prec1", "let () = println (int_to_str (2 + 3 * 4))"),
    ("arith_prec2", "let () = println (int_to_str (2 * 3 + 4 * 5))"),
    ("arith_sub_div", "let () = println (int_to_str ((100 - 1) / 11))"),
    ("arith_sum1_6", "let () = println (int_to_str (1 + 2 + 3 + 4 + 5 + 6))"),
    ("arith_sq9", "let () = println (int_to_str (9 * 9))"),
    ("arith_zero_mul", "let () = println (int_to_str (8 * 7 * 0 + 1))"),
    ("arith_mod50_7", "let () = println (int_to_str (50 % 7))"),
    ("arith_neg_add", "let () = println (int_to_str (0 - 3 + 10))"),
    ("arith_chain_paren", "let () = println (int_to_str (2 * (2 + 3) * 2))"),
    ("arith_nested", "let () = println (int_to_str (((1 + 2) * 3 + 4) * 5))"),
    ("if_nested_ff", "let () = println (int_to_str (if true then if false then 0 else 11 else 2))"),
    ("if_nested_cmp", "let () = println (int_to_str (if 3 < 4 then if 4 < 5 then 99 else 1 else 2))"),
    ("cmp_eq00", "let () = println (int_to_str (if 0 == 0 then 1 else 0))"),
    ("cmp_ne12", "let () = println (int_to_str (if 1 == 2 then 1 else 0))"),
    ("cmp_lt35", "let () = println (int_to_str (if 3 < 5 then 1 else 0))"),
    ("cmp_lt53", "let () = println (int_to_str (if 5 < 3 then 1 else 0))"),
    ("cmp_gt94", "let () = println (int_to_str (if 9 > 4 then 1 else 0))"),
    ("bool_tt", "let () = println (int_to_str (if true && true then 1 else 0))"),
    ("bool_ft", "let () = println (int_to_str (if false && true then 1 else 0))"),
    ("bool_or_ff", "let () = println (int_to_str (if false || false then 1 else 0))"),
    ("let_chain_abc", "let a = 1\nlet b = a + 2\nlet c = b * 3\nlet () = println (int_to_str c)"),
    ("let_sq", "let x = 10\nlet y = x * x\nlet () = println (int_to_str y)"),
    ("fn_clamp_neg", "let f n = if n < 0 then 0 else n\nlet () = println (int_to_str (f (0 - 5) + f 12))"),
    ("fn_double_call", "let d x = x + x\nlet () = println (int_to_str (d 3 * d 2))"),
    ("lambda_inc", "let () = println (int_to_str ((fn x -> x + 1) 40))"),
    ("lambda_sub_curried", "let () = println (int_to_str ((fn x -> fn y -> x - y) 50 8))"),
    ("hof_apply_offset", "let k x f = f x\nlet inc z = z + 10\nlet () = println (int_to_str (k 5 inc))"),
    ("hof_twice_inc", "let twice f x = f (f x)\nlet inc n = n + 1\nlet () = println (int_to_str (twice inc 8))"),
    ("poly_id_threef", "let f x = x\nlet () = println (int_to_str (f (f (f 12))))"),
    ("apply_const_neg", "let apply f x = f x\nlet z (_ : int) : int = 0 - 7\nlet () = println (int_to_str (apply z 100))"),
    ("partial_sub_100", "let sub a b = a - b\nlet t = sub 100\nlet () = println (int_to_str (t 37))"),
    ("partial_mul_6_7", "let m a b = a * b\nlet d = m 6\nlet () = println (int_to_str (d 7))"),
    ("two_prints", "let () = println (int_to_str 1)\nlet () = println (int_to_str 2)"),
    ("rec_digit_sum_12345", "let rec ds n = if n == 0 then 0 else n % 10 + ds (n / 10)\nlet () = println (int_to_str (ds 12345))"),
    ("rec_tri_15", "let rec tri n = if n == 0 then 0 else n + tri (n - 1)\nlet () = println (int_to_str (tri 15))"),
    ("rec_acc_sum5", "let rec ma n acc = if n == 0 then acc else ma (n - 1) (acc + n)\nlet () = println (int_to_str (ma 5 0))"),
    ("flip_sub_partial", "let flip f x y = f y x\nlet sub a b = a - b\nlet g = flip sub 3\nlet () = println (int_to_str (g 10))"),
    ("hof_compose_inner", "let c f g x = f (g x)\nlet a x = x + 1\nlet b x = x * 2\nlet () = println (int_to_str (c a b 5))"),
    ("sq_binding", "let square n = n * n\nlet v = square 9\nlet () = println (int_to_str v)"),
    ("if_inside_add", "let () = println (int_to_str (3 + (if 1 == 1 then 10 else 0)))"),
    ("mod_100_3", "let () = println (int_to_str (100 % 3))"),
    ("div_double", "let () = println (int_to_str ((999 / 10) / 10))"),
    ("mul_25_24", "let () = println (int_to_str (25 * 24))"),
    ("min_nested2", "let m a b = if a < b then a else b\nlet () = println (int_to_str (m (m 7 9) 5))"),
    ("max_nested2", "let m a b = if a > b then a else b\nlet () = println (int_to_str (m (m 2 5) 3))"),
    ("second_sum", "let s _ y = y\nlet () = println (int_to_str ((s 0 1) + (s 9 2)))"),
    ("const_discard", "let c x _ = x\nlet () = println (int_to_str (c 42 0))"),
    ("run_id_twice", "let run g = g 3 + g 4\nlet id x = x\nlet () = println (int_to_str (run id))"),
    ("partial_abc_33", "let f a b c = a + b + c\nlet p = f 1\nlet q = p 2\nlet () = println (int_to_str (q 30))"),
    ("rec_cd_6", "let rec cd n = if n == 1 then 1 else n + cd (n - 1)\nlet () = println (int_to_str (cd 6))"),
    ("bool_mix55", "let () = println (int_to_str (if (1 < 2) && (3 < 4) || false then 55 else 0))"),
    ("cmp_ne_45", "let () = println (int_to_str (if 4 == 5 then 0 else 1))"),
    ("if_if_true", "let () = println (int_to_str (if if true then true else false then 88 else 0))"),
    ("print_zeros", "let () = println (int_to_str 0)\nlet () = println (int_to_str 0)"),
    ("mk_add_closure", "let mk_add n = fn x -> x + n\nlet g = mk_add 100\nlet () = println (int_to_str (g 23))"),
    ("partial_addb_capture", "let addb n x = x + n\nlet g = addb 10\nlet () = println (int_to_str (g 32))"),
    ("three_prints", "let () = println (int_to_str 2)\nlet () = println (int_to_str 3)\nlet () = println (int_to_str 5)"),
    ("flip_sub_order", "let flip f x y = f y x\nlet sub a b = a - b\nlet () = println (int_to_str (flip sub 8 9))"),
    ("sub_left_assoc", "let () = println (int_to_str (100 - 20 - 5))"),
    ("div_left_assoc", "let () = println (int_to_str (24 / 2 / 2))"),
    ("rec_dbl_steps", "let rec d n k = if k == 0 then n else d (n * 2) (k - 1)\nlet () = println (int_to_str (d 1 4))"),
    ("let_inner_y", "let x = let y = 5 in y + y\nlet () = println (int_to_str x)"),
    ("if_false_thirteen", "let () = println (int_to_str (if 0 == 1 then 9 else 13))"),
    ("le_same", "let () = println (int_to_str (if 5 <= 5 then 1 else 0))"),
    ("cmp_chain20", "let () = println (int_to_str (if 1 < 2 && 2 <= 3 && 3 < 4 then 20 else 0))"),
    ("apply_inc_once", "let a f x = f x\nlet inc v = v + 1\nlet () = println (int_to_str (a inc 40))"),
    ("pick_int_branch", "let pick t (fa : int -> int) (fb : int -> int) = if t then fa 0 else fb 0\nlet const6 (x : int) : int = 6\nlet const9 (x : int) : int = 9\nlet () = println (int_to_str (pick true const6 const9))"),
    ("ignore_unit_param", "let f _ = 17\nlet () = println (int_to_str (f ()))"),
    ("wildcard_99", "let g _ = 99\nlet () = println (int_to_str (g 0))"),
    ("compose_id_typed", "let c f g x = f (g x)\nlet id (x : int) : int = x\nlet () = println (int_to_str (c id id 43))"),
    ("curry_add1_2_30", "let f x y z = x + y + z\nlet t = f 1 2\nlet () = println (int_to_str (t 30))"),
    ("binom_6_3", "let rec c n k = if k == 0 || k == n then 1 else c (n - 1) (k - 1) + c (n - 1) k\nlet () = println (int_to_str (c 6 3))"),
    ("mod_div_sum", "let () = println (int_to_str ((17 % 6) + (17 / 6)))"),
    ("hof_double_inc", "let s f g x = f (g x)\nlet double x = x * 2\nlet inc y = y + 1\nlet () = println (int_to_str (s double inc 6))"),
]

assert len(PROGRAMS) == 100, len(PROGRAMS)


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        ["dune", "build", "bin/compile_lambdascript.exe"],
        cwd=ROOT,
        check=True,
    )
    failed = []
    for i, (slug, src) in enumerate(PROGRAMS, start=1):
        ok, out = compile_and_run(src)
        path = OUT_DIR / f"combo_{i:03d}_{slug}.ls"
        if not ok:
            failed.append((i, slug, out[:500]))
            print(f"FAIL {path.name}: {out[:200]}", file=sys.stderr)
            continue
        write_case(path, out, src)
        print(path.name, "->", repr(out))
    if failed:
        print(f"\n{len(failed)} failures", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
