package com.moyz.adi.common.interfaces;

public abstract class AbstractSearchService {

    public abstract boolean isEnabled();

    public abstract void briefSearch(String question);

    public abstract void detailSearch(String question);

}
