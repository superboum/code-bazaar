from pathlib import Path
import zlib
import io

class GitDirNotFound(Exception):
    pass

def find_git_folder(directory: Path|None = None) -> Path:
    if directory is None:
        directory = Path(".").absolute()

    candidate = directory / Path(".git")
    if candidate.is_dir():
        return candidate
    elif directory.parent == directory:
        raise GitDirNotFound()
    else:
        return find_git_folder(directory.parent)

def find_git_root_obj(git_dir: Path) -> Path:
    # @FIXME will implement later
    pass

def extract_zlib_object(obj: Path) -> bytes:
    return zlib.decompress(obj.read_bytes())

def read_commit(commit: Path) -> str:
    #@FIXME: there is a null terminated string first b'commit 481\x00'
    return extract_zlib_object(commit).decode("utf8")

def main() -> None:
    #print(read_commit(Path("../../.git/objects/5f/ad4a9489e1b28a725760f0ede687eaaf5e53f0")))
    print(extract_zlib_object(Path("../../.git/objects/a1/fb136e2b37492f59160ab8254be1101f9a7650")))


if __name__ == "__main__":
    main()
