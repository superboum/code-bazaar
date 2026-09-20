## Trying to understand git...

### Git base data model

[Similarly to Seafile](https://git.deuxfleurs.fr/quentin/seafile_recovery), Git has a ref/commit/tree/blob approach to storing content.
It is summarized by this picture extracted from chapter 10.3 of git internals:

![git ref/commit/tree/blob model](./doc/git-data-model.png)

All of these 4 concepts (ref/commit/tree/blob), 3 of them (commit/tree/blob) are content addressed objects.
It means that, before being compressed, we compute their `sha1sum`, and it becomes their object name.
Hence, an object that did not change will keep the same sha1 fingerprint and will not be stored multiple times.

### Git packs

*TODO*

### Wire protocols

*TODO*


## References
 - Git Internals
   - [10.2 Git Internals - Git Objects](https://git-scm.com/book/en/v2/Git-Internals-Git-Objects)
   - [10.3 Git Internals - Git References](https://git-scm.com/book/en/v2/Git-Internals-Git-References)
   - [10.4 Git Internals - Packfiles](https://git-scm.com/book/en/v2/Git-Internals-Packfiles)
   - [10.5 Git Internals - The RefSpec](https://git-scm.com/book/en/v2/Git-Internals-The-Refspec)
   - [10.6 Git Internals - Transfer Protocols](https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols)
 - [You can run git on object storage if you re-make packfiles](https://www.tigrisdata.com/blog/objgit-packfiles/)
 - git-repack
   - [git-repack](https://man7.org/linux/man-pages/man1/git-repack.1.html)
   - [Gitlab Gitaly Repack](https://about.gitlab.com/blog/rearchitecting-git-object-database-mainentance-for-scale/)
 - jgit has a distributed filesystem abstraction contributed by Google & used by Gerrit
   - [jgit homepage](https://projects.eclipse.org/projects/technology.jgit)
   - [jgit github](https://github.com/eclipse-jgit/jgit)
   - [jgit DfsRepository](https://github.com/eclipse-jgit/jgit/blob/master/org.eclipse.jgit/src/org/eclipse/jgit/internal/storage/dfs/DfsRepository.java#L31)
   - [jgit aws](https://github.com/johnny0917/jgit-aws)
   - [gitiles](https://gerrit.googlesource.com/gitiles/?utm_source=gemini) - Google open-source git browser built on top of jgit
   - [a google group discussion about jgit+cassandra](https://groups.google.com/g/repo-discuss/c/IekVPmow0yE)
