package com.moyz.adi.common.config;

import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.BlockAttackInnerInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.DynamicTableNameInnerInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import com.baomidou.mybatisplus.extension.spring.MybatisSqlSessionFactoryBean;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.moyz.adi.common.base.SearchEngineRespTypeHandler;
import com.moyz.adi.common.base.UUIDTypeHandler;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.SearchEngineResp;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.rag.EmbeddingRAG;
import com.moyz.adi.common.rag.GraphRAG;
import com.moyz.adi.common.rag.GraphStore;
import com.moyz.adi.common.service.embedding.model.DashScopeEmbeddingModelService;
import com.moyz.adi.common.service.embedding.model.OpenAiEmbeddingModelService;
import com.moyz.adi.common.util.AdiPropertiesUtil;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.pgvector.PGvector;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.model.embedding.onnx.allminilml6v2.AllMiniLmL6V2EmbeddingModel;
import dev.langchain4j.store.embedding.EmbeddingStore;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Primary;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.http.client.BufferingClientHttpRequestFactory;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;
import org.springframework.web.client.RestTemplate;

import javax.sql.DataSource;
import java.util.UUID;

@Slf4j
@Configuration
public class BeanConfig {

    @Resource
    private AdiProperties adiProperties;

    @Bean
    public RestTemplate restTemplate() {
        log.info("Configuration:create restTemplate");
        SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
        // 设置建立连接超时时间  毫秒
        requestFactory.setConnectTimeout(60000);
        // 设置读取数据超时时间  毫秒
        requestFactory.setReadTimeout(60000);
        RestTemplate restTemplate = new RestTemplate();
        // 注册LOG拦截器
//        restTemplate.setInterceptors(Lists.newArrayList(new LogClientHttpRequestInterceptor()));
        restTemplate.setRequestFactory(new BufferingClientHttpRequestFactory(requestFactory));

        return restTemplate;
    }

    @Bean
    @Primary
    public ObjectMapper objectMapper() {
        log.info("Configuration:create objectMapper");
        ObjectMapper objectMapper = new Jackson2ObjectMapperBuilder().createXmlMapper(false).build();
        objectMapper.registerModules(LocalDateTimeUtil.getSimpleModule(), new JavaTimeModule(), new Jdk8Module());
        //设置null值不参与序列化(字段不被显示)
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        return objectMapper;
    }

    @Bean(name = "mainExecutor")
    @Primary
    public AsyncTaskExecutor mainExecutor() {
        int processorsNum = Runtime.getRuntime().availableProcessors();
        log.info("mainExecutor,processorsNum:{}", processorsNum);
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(processorsNum * 2);
        executor.setMaxPoolSize(100);
        return executor;
    }

    @Bean(name = "imagesExecutor")
    public AsyncTaskExecutor imagesExecutor() {
        int processorsNum = Runtime.getRuntime().availableProcessors();
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        log.info("imagesExecutor corePoolSize:{},maxPoolSize:{}", processorsNum, processorsNum * 2);
        executor.setCorePoolSize(processorsNum);
        executor.setMaxPoolSize(processorsNum * 2);
        return executor;
    }

    @Bean
    @Primary
    public SqlSessionFactory sqlSessionFactory(DataSource dataSource)
            throws Exception {
        MybatisSqlSessionFactoryBean bean = new MybatisSqlSessionFactoryBean();
        bean.setDataSource(dataSource);
        MybatisPlusInterceptor interceptor = new MybatisPlusInterceptor();
        // 分页插件
        interceptor.addInnerInterceptor(new PaginationInnerInterceptor(DbType.POSTGRE_SQL));
        // 防止全表更新
        interceptor.addInnerInterceptor(new BlockAttackInnerInterceptor());
        // 动态表名
        DynamicTableNameInnerInterceptor dynamicTableNameInnerInterceptor = new DynamicTableNameInnerInterceptor();
        dynamicTableNameInnerInterceptor.setTableNameHandler(
                new EmbeddingTableNameHandler("adi_knowledge_base_embedding", "adi_ai_search_embedding")
        );
        interceptor.addInnerInterceptor(dynamicTableNameInnerInterceptor);

        bean.setPlugins(interceptor);
        bean.setMapperLocations(
                new PathMatchingResourcePatternResolver().getResources("classpath*:/mapper/*.xml"));
        MybatisConfiguration configuration = bean.getConfiguration();
        if (null == configuration) {
            configuration = new MybatisConfiguration();
            bean.setConfiguration(configuration);
        }
        bean.getConfiguration().getTypeHandlerRegistry().register(PGvector.class, PostgresVectorTypeHandler.class);
        bean.getConfiguration().getTypeHandlerRegistry().register(SearchEngineResp.class, SearchEngineRespTypeHandler.class);
        bean.getConfiguration().getTypeHandlerRegistry().register(UUID.class, UUIDTypeHandler.class);
        return bean.getObject();
    }

    @Bean
    @DependsOn("initializer")
    public EmbeddingModel initEmbeddingModel() {
        if (adiProperties.getEmbeddingModel().equals(AdiConstant.EmbeddingModel.ALL_MINILM_L6)) {
            return new AllMiniLmL6V2EmbeddingModel();
        }
        AiModel aiModel = AdiPropertiesUtil.getEmbeddingModelByProperty(adiProperties);
        if (aiModel.getPlatform().equals(AdiConstant.ModelPlatform.DASHSCOPE)) {
            return new DashScopeEmbeddingModelService(aiModel).buildModel();
        } else if (aiModel.getPlatform().equals(AdiConstant.ModelPlatform.OPENAI)) {
            return new OpenAiEmbeddingModelService(aiModel).buildModel();
        } else {
            throw new RuntimeException("Unsupported embedding model: " + adiProperties.getEmbeddingModel());
        }
    }

    @Bean
    @Primary
    public EmbeddingRAG initKnowledgeBaseRAGService(EmbeddingStore<TextSegment> kbEmbeddingStore, EmbeddingModel embeddingModel) {
        EmbeddingRAG ragService = new EmbeddingRAG(kbEmbeddingStore);
        ragService.init(embeddingModel);
        return ragService;
    }

    @Bean(name = "searchRagService")
    public EmbeddingRAG initSearchRAG(EmbeddingStore<TextSegment> searchEmbeddingStore, EmbeddingModel embeddingModel) {
        EmbeddingRAG ragService = new EmbeddingRAG(searchEmbeddingStore);
        ragService.init(embeddingModel);
        return ragService;
    }

    @Bean(name = "graphRag")
    @Primary
    public GraphRAG initGraphRAG(GraphStore graphStore) {
        return new GraphRAG(graphStore);
    }

//    @Bean(name = "queryRouterRagService")
//    public RAGService queryRouterRagService() {
//        RAGService ragService = new RAGService("adi_advanced_rag_query_embedding", dataBaseUrl, dataBaseUserName, dataBasePassword);
//        ragService.init();
//        return ragService;
//    }

    @Bean(name = "beanValidator")
    public LocalValidatorFactoryBean validator() {
        return new LocalValidatorFactoryBean();
    }
}
