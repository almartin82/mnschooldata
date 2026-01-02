# TODO

## pkgdown Build Issues

### Network Timeout Error (2026-01-01)

The pkgdown build fails due to network timeout when trying to connect to
bioconductor.org:

    Error in `httr2::req_perform(req)`:
    ! Failed to perform HTTP request.
    Caused by error in `curl::curl_fetch_memory()`:
    ! Timeout was reached [bioconductor.org]:
    Connection timed out after 10001 milliseconds

**Root cause**: pkgdown’s `cran_link()` function attempts to check if
the package exists on CRAN/Bioconductor to generate sidebar links. When
there’s no network connectivity or timeouts occur, the build fails.

**Workaround options**: 1. Run pkgdown build when network is available
2. Use GitHub Actions for pkgdown builds (recommended - already
configured in `.github/`)

**Note**: This package has no vignettes directory, so only reference
documentation is generated.
