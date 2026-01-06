# TODO

## Critical: Fix MDE Data Source URLs (2026-01-04)

The Minnesota Department of Education (MDE) has changed their
MDEAnalytics data portal. The current URL patterns in
`get_raw_enrollment.R` are returning 404 errors:

- Direct file URLs like
  `/MDEAnalytics/DataDownload/Enrollment_2024_School.xlsx` no longer
  exist
- The portal now uses a WebFOCUS/Information Builders application with
  JavaScript-based downloads
- Downloads require navigating through the iframe at
  `/ibi_apps/WFServlet?IBIF_ex=...`

**Required changes**: 1. Research the new WebFOCUS API or find
alternative download method 2. Update
[`download_mde_enrollment()`](https://almartin82.github.io/mnschooldata/reference/download_mde_enrollment.md)
and
[`download_mde_via_cgi()`](https://almartin82.github.io/mnschooldata/reference/download_mde_via_cgi.md)
in `R/get_raw_enrollment.R` 3. Test with all available years (2007-2024)
4. Remove `eval = FALSE` from vignette once data fetch works

**MDE Portal URLs**: - Main portal:
<https://pub.education.mn.gov/MDEAnalytics/Data.jsp> - WebFOCUS entry:
`/ibi_apps/WFServlet?IBIF_ex=mdea_ddl_topic_select_drilldown` - Schools
& Districts topic: `/MDEAnalytics/DataTopic.jsp?TOPICID=4`

**Alternative sources to investigate**: - Minnesota Report Card:
<https://rc.education.mn.gov/> - SLEDS: <http://sleds.mn.gov/>

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
