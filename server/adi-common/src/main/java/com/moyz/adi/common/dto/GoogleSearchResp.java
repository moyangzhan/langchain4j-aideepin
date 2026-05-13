package com.moyz.adi.common.dto;

import lombok.Data;

import java.util.List;

@Data
public class GoogleSearchResp {
    private String kind;
    private Queries queries;
    private SearchInformation searchInformation;
    private List<Item> items;
    private GoogleSearchError error;

    @Data
    public static class Queries {
        private Request[] request;
    }

    @Data
    public static class Request {
        private String title;
        private String totalResults;
        private String searchTerms;
        private Integer count;
        private Integer startIndex;
        private String inputEncoding;
        private String outputEncoding;
    }

    @Data
    public static class SearchInformation {
        private double searchTime;
        private String formattedSearchTime;
        private String totalResults;
        private String formattedTotalResults;
    }

    @Data
    public static class Item {
        private String kind;
        private String title;
        private String htmlTitle;
        private String link;
        private String snippet;
    }
}
