library(httptest)

# these functions replace the common part of the url with a shortened version
# in order to reduce the path length and prevent check warnings
# example https://github.com/ffverse/ffscrapr

httptest::set_requester(
    function(request){
        httptest::gsub_request(request,'https\\://www.lipidmaps.org/rest',"lm/") %>%
        httptest::gsub_request('https\\://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/',"pc/")
    }
)

httptest::set_redactor(
    function(response){
        httptest::gsub_response(response,'https\\://www.lipidmaps.org/rest',"lm/") %>%
        httptest::gsub_response('https\\://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/',"pc/")
    }
)
