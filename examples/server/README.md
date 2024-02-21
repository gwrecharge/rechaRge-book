# rechaRge client-server

This example demonstrates how to make use of [OBiBa/Rock R server](https://github.com/obiba/rock) to perform `rechaRge` computations on a remote server.

1. Use pre-built Docker image with `rechaRge` and [caRamel](https://cran.r-project.org/package=caRamel) installed
2. Run the R server as a Docker container
3. Connect to the R server using the [rockr](https://cran.r-project.org/package=rockr) R package
4. Upload data and analysis script
5. Follow up analysis execution
6. Download and extract results

To start the R server:

```
make pull up
```

To follow R server output:

```
make rlogs
```

To stop the R server:

```
make stop
```
