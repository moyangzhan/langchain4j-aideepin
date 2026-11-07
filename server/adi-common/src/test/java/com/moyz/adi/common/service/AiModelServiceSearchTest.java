package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.AiModelDto;
import com.moyz.adi.common.dto.AiModelSearchReq;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.enums.ModelHealthStatus;
import com.moyz.adi.common.mapper.AiModelMapper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class AiModelServiceSearchTest {

    private AiModelService aiModelService;
    private AiModelMapper aiModelMapper;
    private ModelHealthService modelHealthService;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), AiModel.class);
        aiModelService = new AiModelService();
        aiModelMapper = mock(AiModelMapper.class);
        modelHealthService = mock(ModelHealthService.class);
        ReflectionTestUtils.setField(aiModelService, "baseMapper", aiModelMapper);
        ReflectionTestUtils.setField(aiModelService, "entityClass", AiModel.class);
        ReflectionTestUtils.setField(aiModelService, "modelHealthService", modelHealthService);
        when(aiModelMapper.selectList(any())).thenReturn(List.of(
                model("broken", true),
                model("fine", true),
                model("unprobed", true),
                model("disabled-broken", false),
                model("disabled-unprobed", false)));
        when(modelHealthService.getAllStatuses()).thenReturn(Map.of(
                "broken", health(ModelHealthStatus.UNHEALTHY),
                "fine", health(ModelHealthStatus.HEALTHY),
                "disabled-broken", health(ModelHealthStatus.UNHEALTHY)));
    }

    @Test
    void unhealthyFilterReturnsOnlyEnabledModelsMarkedUnhealthy() {
        Page<AiModelDto> page = aiModelService.search(healthReq("UNHEALTHY"), 1, 10);

        assertEquals(List.of("broken"), names(page));
        assertEquals(1L, page.getTotal());
    }

    @Test
    void healthyFilterCountsUnprobedModelsAsHealthyAndSkipsDisabled() {
        Page<AiModelDto> page = aiModelService.search(healthReq("HEALTHY"), 1, 10);

        assertEquals(List.of("fine", "unprobed"), names(page));
        assertEquals(2L, page.getTotal());
    }

    @Test
    void healthFilterPaginatesAfterFiltering() {
        Page<AiModelDto> second = aiModelService.search(healthReq("HEALTHY"), 2, 1);
        Page<AiModelDto> beyond = aiModelService.search(healthReq("HEALTHY"), 3, 1);
        Page<AiModelDto> overflow = aiModelService.search(healthReq("HEALTHY"), Integer.MAX_VALUE, 10);

        assertEquals(List.of("unprobed"), names(second));
        assertEquals(2L, second.getTotal());
        assertTrue(beyond.getRecords().isEmpty());
        assertEquals(2L, beyond.getTotal());
        assertTrue(overflow.getRecords().isEmpty());
        assertEquals(2L, overflow.getTotal());
    }

    @Test
    @SuppressWarnings("unchecked")
    void healthFilterKeepsOtherConditionsInTheQuery() {
        AiModelSearchReq req = healthReq("HEALTHY");
        req.setPlatform("dashscope");

        aiModelService.search(req, 1, 10);

        ArgumentCaptor<Wrapper<AiModel>> captor = ArgumentCaptor.forClass(Wrapper.class);
        verify(aiModelMapper).selectList(captor.capture());
        String sqlSegment = captor.getValue().getSqlSegment();
        assertTrue(sqlSegment.contains("platform"), sqlSegment);
        assertTrue(sqlSegment.contains("is_deleted"), sqlSegment);
    }

    @Test
    @SuppressWarnings("unchecked")
    void nameFilterFallsBackToDatabasePagination() {
        AiModelSearchReq req = new AiModelSearchReq();
        req.setName("qwen");
        Page<AiModel> dbPage = new Page<>(1, 10);
        dbPage.setRecords(List.of(model("qwen3:8b", true)));
        dbPage.setTotal(1);
        when(aiModelMapper.selectPage(ArgumentMatchers.<Page<AiModel>>any(), any())).thenReturn(dbPage);

        Page<AiModelDto> page = aiModelService.search(req, 1, 10);

        assertEquals(List.of("qwen3:8b"), names(page));
        ArgumentCaptor<Wrapper<AiModel>> captor = ArgumentCaptor.forClass(Wrapper.class);
        verify(aiModelMapper).selectPage(any(), captor.capture());
        String sqlSegment = captor.getValue().getSqlSegment();
        assertTrue(sqlSegment.contains("LIKE"), sqlSegment);
        assertTrue(sqlSegment.contains("name"), sqlSegment);
        verify(aiModelMapper, never()).selectList(any());
    }

    @Test
    void searchWithoutHealthStatusUsesDatabasePagination() {
        Page<AiModel> dbPage = new Page<>(1, 10);
        dbPage.setRecords(List.of(model("fine", true)));
        dbPage.setTotal(1);
        when(aiModelMapper.selectPage(ArgumentMatchers.<Page<AiModel>>any(), any())).thenReturn(dbPage);

        Page<AiModelDto> page = aiModelService.search(new AiModelSearchReq(), 1, 10);

        assertEquals(List.of("fine"), names(page));
        verify(aiModelMapper, never()).selectList(any());
        verifyNoInteractions(modelHealthService);
    }

    private static AiModelSearchReq healthReq(String healthStatus) {
        AiModelSearchReq req = new AiModelSearchReq();
        req.setHealthStatus(healthStatus);
        return req;
    }

    private static AiModel model(String name, boolean enabled) {
        AiModel model = new AiModel();
        model.setName(name);
        model.setIsEnable(enabled);
        return model;
    }

    private static ModelHealthService.HealthCheckResult health(ModelHealthStatus status) {
        return ModelHealthService.HealthCheckResult.builder().status(status).build();
    }

    private static List<String> names(Page<AiModelDto> page) {
        return page.getRecords().stream().map(AiModelDto::getName).toList();
    }
}
