structure(list(
    url = "pc/name/no_hit/property/InChIKey/JSON",
    status_code = 404L, headers = structure(list(
        date = "Thu, 08 Feb 2024 09:42:56 GMT",
        server = "Apache", `strict-transport-security` = "max-age=31536000; includeSubDomains; preload",
        `referrer-policy` = "origin-when-cross-origin", `content-security-policy` = "upgrade-insecure-requests",
        `cache-control` = "private", expires = "Thu, 08 Feb 2024 17:42:56 GMT",
        `ncbi-phid` = "95B6D6C35C4A22010000000000000001.m_1",
        `ncbi-sid` = "90C5C09B5C4A21E1_0000SID", `x-throttling-control` = "Request Count status: Green (0%), Request Time status: Green (0%), Service status: Green (23%)",
        `content-type` = "application/json", `set-cookie` = "ncbi_sid=90C5C09B5C4A21E1_0000SID; domain=.nih.gov; path=/; expires=Sat, 08 Feb 2025 09:42:56 GMT",
        vary = "Accept-Encoding", `content-encoding` = "gzip",
        `x-ua-compatible` = "IE=Edge", `x-xss-protection` = "1; mode=block",
        `access-control-allow-origin` = "*", `transfer-encoding` = "chunked"
    ), class = c(
        "insensitive",
        "list"
    )), all_headers = list(list(
        status = 404L, version = "HTTP/1.1",
        headers = structure(list(
            date = "Thu, 08 Feb 2024 09:42:56 GMT",
            server = "Apache", `strict-transport-security` = "max-age=31536000; includeSubDomains; preload",
            `referrer-policy` = "origin-when-cross-origin", `content-security-policy` = "upgrade-insecure-requests",
            `cache-control` = "private", expires = "Thu, 08 Feb 2024 17:42:56 GMT",
            `ncbi-phid` = "95B6D6C35C4A22010000000000000001.m_1",
            `ncbi-sid` = "90C5C09B5C4A21E1_0000SID", `x-throttling-control` = "Request Count status: Green (0%), Request Time status: Green (0%), Service status: Green (23%)",
            `content-type` = "application/json", `set-cookie` = "ncbi_sid=90C5C09B5C4A21E1_0000SID; domain=.nih.gov; path=/; expires=Sat, 08 Feb 2025 09:42:56 GMT",
            vary = "Accept-Encoding", `content-encoding` = "gzip",
            `x-ua-compatible` = "IE=Edge", `x-xss-protection` = "1; mode=block",
            `access-control-allow-origin` = "*", `transfer-encoding` = "chunked"
        ), class = c(
            "insensitive",
            "list"
        ))
    )), cookies = structure(list(
        domain = ".nih.gov",
        flag = TRUE, path = "/", secure = FALSE, expiration = structure(1739007776, class = c(
            "POSIXct",
            "POSIXt"
        )), name = "ncbi_sid", value = "90C5C09B5C4A21E1_0000SID"
    ), row.names = c(
        NA,
        -1L
    ), class = "data.frame"), content = charToRaw("{\n  \"Fault\": {\n    \"Code\": \"PUGREST.NotFound\",\n    \"Message\": \"No CID found\",\n    \"Details\": [\n      \"No CID found that matches the given name\"\n    ]\n  }\n}\n"),
    date = structure(1707385376, class = c("POSIXct", "POSIXt"), tzone = "GMT"), times = c(
        redirect = 0, namelookup = 3.9e-05,
        connect = 0, pretransfer = 0.000103, starttransfer = 0.119938,
        total = 0.122824
    )
), class = "response")
