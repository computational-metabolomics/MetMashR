library(httptest)

# these functions replace the common part of the url with a shortened version
# in order to reduce the path length and prevent check warnings
# example https://github.com/ffverse/ffscrapr

httptest::set_requester(
    function(request) {
        httptest::gsub_request(
            request,
            "https\\://www.lipidmaps.org/rest", "lm/"
        ) %>%
            httptest::gsub_request(
                "https\\://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/",
                "pc/"
            ) %>%
            httptest::gsub_request(
                "https\\://www.metabolomicsworkbench.org/rest/compound/",
                "mwb/"
            ) %>%
            httptest::gsub_request(
                "http\\://classyfire.wishartlab.com/entities",
                "cf/"
            ) %>%
            httptest::gsub_request(
                "http\\://rest.kegg.jp/entities/conv",
                "kg/"
            ) %>%
            # shorten long/percent-encoded path segments used in the
            # lipidmaps lookup tests to keep mock fixture paths portable
            httptest::gsub_request("inchi_key%2Chmdb_id", "ik_hid") %>%
            httptest::gsub_request("PE%2816%3A0_18%3A1%29", "PE1") %>%
            httptest::gsub_request("TG%2816%3A0_16%3A1_18%3A2%29", "TG1")
    }
)

httptest::set_redactor(
    function(response) {
        httptest::gsub_response(
            response,
            "https\\://www.lipidmaps.org/rest", "lm/"
        ) %>%
            httptest::gsub_response(
                "https\\://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/",
                "pc/"
            ) %>%
            httptest::gsub_response(
                "https\\://www.metabolomicsworkbench.org/rest/compound/",
                "mwb/"
            ) %>%
            httptest::gsub_response(
                "http\\://classyfire.wishartlab.com/entities",
                "cf/"
            ) %>%
            httptest::gsub_response(
                "http\\://rest.kegg.jp/entities/conv",
                "kg/"
            ) %>%
            httptest::gsub_response("inchi_key%2Chmdb_id", "ik_hid") %>%
            httptest::gsub_response("PE%2816%3A0_18%3A1%29", "PE1") %>%
            httptest::gsub_response("TG%2816%3A0_16%3A1_18%3A2%29", "TG1")
    }
)
