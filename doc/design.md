# OCI #

## Architecture ##

OCI is composed by four different type of components:

- one conductor which coordinate the work to do and maintain the global state
- many monitors who run on remote computers, execute the workers, take their output
  and save the artifacts
- many workers who sequentially execute a task and can depend on the
  outputs of other workers
- user interface (command line or website) for requesting a task or
  looking at the result

## Main Ideas ##

- Tasks are just usual ocaml functions (cf Richard Jones goaljobs [1](https://rwmj.wordpress.com/2013/09/19/goaljobs-part-1/) [2](https://rwmj.wordpress.com/2013/09/20/goaljobs-part-2/) [3](https://rwmj.wordpress.com/2013/09/20/goaljobs-part-3/) [4](https://rwmj.wordpress.com/2013/09/20/goaljobs-part-4/)):
 - they are not automatically memoized
 - but there is tools for doing this memoization

- Run tests in isolation using "versioned" filesystem with [LXC](http://linuxcontainers.org/) and [AUFS](http://en.wikipedia.org/wiki/Aufs) (cf https://www.docker.io/)
  - Make the filesystem version first class value

- Use a real databases that can be read by different components for
  simple information sharing (cf [genet](http://zoggy.github.io/genet/))
 - use a simple framework for using postgresql
 - every important type of the system can be used as database
   column (artifacts, result, filesystem)
 - for big data (filesystem) the information can be stored separately
 and moved or recomputed when needed

- All the configurations (ini or ml) are versionned on git

## Difference with a Build System ##

- The input of a worker is completely fixed. So there is no reason to
  redo the same worker if something has changed since nothing change.
- Some outputs will never be forgetted, it is normally small data
  (if the test is succesfull, time taken, artifact)