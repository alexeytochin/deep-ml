#!/usr/bin/env python3
import subprocess
import yaml
from pathlib import Path

# корневой каталог проекта
ROOT = Path.cwd()
PROJECT_DIRS = ["simple-expr", "inf-backprop"]

def run(cmd, cwd=None):
    """Запуск команды с возвратом stdout или stderr"""
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True, cwd=cwd)
    if result.returncode != 0:
        return None, result.stderr.strip()
    return result.stdout.strip(), None

def extract_upper_bounds(bounds_output):
    """
    Из вывода cabal gen-bounds извлекаем только верхние границы
    например: 'text >= 2.1.1 && < 2.2' → 'text < 2.2'
    """
    deps = []
    for line in bounds_output.splitlines():
        line = line.strip().rstrip(",")
        if "&& <" in line and line[0].isalpha():
            parts = line.split("&& <")
            pkg = parts[0].split(">=")[0].strip()
            upper = parts[1].strip()
            deps.append(f"{pkg} < {upper}")
    return deps

def update_package_yaml(yaml_path, upper_bounds):
    """Обновляем package.yaml, добавляя/обновляя верхние границы, не стирая остальные deps"""
    with open(yaml_path) as f:
        data = yaml.safe_load(f)

    data.setdefault("library", {})
    existing_deps = data["library"].get("dependencies", [])

    # превращаем существующие зависимости в dict: pkg -> spec
    dep_dict = {}
    for dep in existing_deps:
        parts = dep.split(maxsplit=1)
        name = parts[0]
        spec = parts[1] if len(parts) > 1 else ""
        dep_dict[name] = spec.strip()

    # обновляем верхние границы из cabal gen-bounds
    for dep in upper_bounds:
        parts = dep.split("<")
        name = parts[0].strip()
        upper = parts[1].strip()
        dep_dict[name] = f"< {upper}"  # заменяем или добавляем

    # собираем обратно список зависимостей
    merged_deps = [f"{k} {v}".strip() for k, v in dep_dict.items()]
    data["library"]["dependencies"] = merged_deps

    with open(yaml_path, "w") as f:
        yaml.safe_dump(data, f, sort_keys=False)

    print(f"  updated dependencies in {yaml_path}")


def process_package(pkg_dir):
    pkg_path = ROOT / pkg_dir
    cabal_files = list(pkg_path.glob("*.cabal"))
    yaml_file = pkg_path / "package.yaml"

    if not cabal_files:
        print(f"Skipping {pkg_dir}: no .cabal file found")
        return
    if not yaml_file.exists():
        print(f"Skipping {pkg_dir}: no package.yaml found")
        return

    cabal_file = cabal_files[0]
    print(f"\nProcessing {pkg_dir} ...")

    # заходим в директорию пакета и вызываем cabal gen-bounds
    out, err = run(f"stack exec --no-ghc-package-path -- cabal gen-bounds {cabal_file}", cwd=pkg_path)
    if err:
        print("  bounds generation failed:")
        print("  ", err.replace("\n", "\n  "))
        return

    bounds = extract_upper_bounds(out)
    if not bounds:
        print("  no dependencies to update")
        return

    update_package_yaml(yaml_file, bounds)

def main():
    for pkg in PROJECT_DIRS:
        process_package(pkg)

if __name__ == "__main__":
    main()
