from pathlib import Path

class GitDirNotFound(Exception):
    pass

def find_git_folder(directory: Path|None = None) -> Path:
    if directory is None:
        directory = Path(".").absolute()

    candidate = directory / Path(".git")
    if candidate.is_dir():
        return candidate
    elif directory.parent == directory:
        raise GitDirNotFound
    else:
        return find_git_folder(directory.parent)

def main() -> None:
    print(find_git_folder())


if __name__ == "__main__":
    main()
