package com.moyz.adi.common.rag;

import dev.langchain4j.store.graph.neo4j.Neo4jGraph;
import org.neo4j.driver.Driver;

public class AdiNeo4jGraph extends Neo4jGraph {

    public AdiNeo4jGraph(Driver driver) {
        super(driver);
    }

    @Override
    public void refreshSchema() {
//        super.refreshSchema();
    }
}
