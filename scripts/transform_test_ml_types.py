#!/usr/bin/env python3
"""Rewrite Forge type syntax inside test/test.ml string literals only."""

import re
import sys

PATH = sys.argv[1] if len(sys.argv) > 1 else "test/test.ml"


def transform_ls_text(seg: str) -> str:
    # Drop leading quote on type variables; allow end-of-string (e.g. "... -> 'a").
    seg = re.sub(r"'([a-z]+)(?=([^a-z']|$))", r"\1", seg)
    seg = re.sub(r"\bint\b", "Int", seg)
    seg = re.sub(r"\bbool\b", "Bool", seg)
    seg = re.sub(r"\bstr\b", "String", seg)
    seg = re.sub(r"\bchar\b", "Char", seg)
    seg = re.sub(r"\bfloat\b", "Float", seg)
    seg = re.sub(r"\bunit\b", "Unit", seg)
    return seg


def main() -> None:
    with open(PATH, encoding="utf-8") as f:
        raw = f.read()

    def process_v2(content: str) -> str:
        out_parts: list[str] = []
        i = 0
        n = len(content)
        while i < n:
            if i + 1 < n and content[i : i + 2] == "{|":
                end = content.find("|}", i + 2)
                if end == -1:
                    out_parts.append(content[i:])
                    break
                out_parts.append(content[i : i + 2])
                out_parts.append(transform_ls_text(content[i + 2 : end]))
                out_parts.append("|}")
                i = end + 2
                continue
            if i + 1 < n and content[i : i + 2] == "(*":
                end = content.find("*)", i + 2)
                if end == -1:
                    out_parts.append(content[i:])
                    break
                out_parts.append(content[i : end + 2])
                i = end + 2
                continue
            if content[i] == '"':
                j = i + 1
                inner_chars: list[str] = []
                while j < n:
                    if content[j] == "\\":
                        if j + 1 >= n:
                            inner_chars.append("\\")
                            j += 1
                            break
                        inner_chars.append(content[j : j + 2])
                        j += 2
                        continue
                    if content[j] == '"':
                        break
                    inner_chars.append(content[j])
                    j += 1
                inner = "".join(inner_chars)
                st = inner.strip()
                looks_ls = (
                    "fn " in inner
                    or " let " in inner
                    or inner.startswith("let ")
                    or "\nlet " in inner
                    or "type " in inner
                    or " inter " in inner
                    or "\ninter " in inner
                    or inner.startswith("inter ")
                    or " impl " in inner
                    or "\nimpl " in inner
                    or inner.startswith("impl ")
                    or "->" in inner
                    or ("[" in inner and "]" in inner)
                    or ("<" in inner and ">" in inner)
                    or (
                        "'" in inner
                        and re.search(r"'[a-z]", inner) is not None
                        and (
                            "type " in inner
                            or "of " in inner
                            or "->" in inner
                            or "<" in inner
                        )
                    )
                    or st
                    in ("int", "bool", "str", "char", "float", "unit", "string")
                    or (st.startswith("(") and ")" in st)
                    or ("Show " in inner)
                    or ("Monoid" in inner)
                    or ("Option<" in inner)
                    or ("List<" in inner)
                    or ("Pair<" in inner)
                    or ("Tree<" in inner)
                    or ("RBTree<" in inner)
                    or ("Color" in inner and "RBTree" in inner)
                    or ("case " in inner and "do" in inner)
                    or ("val " in inner and ":" in inner)
                )
                if looks_ls:
                    inner = transform_ls_text(inner)
                out_parts.append('"')
                out_parts.append(inner)
                out_parts.append('"')
                i = j + 1
                continue
            out_parts.append(content[i])
            i += 1
        return "".join(out_parts)

    fixed = process_v2(raw)
    with open(PATH, "w", encoding="utf-8") as f:
        f.write(fixed)


if __name__ == "__main__":
    main()
