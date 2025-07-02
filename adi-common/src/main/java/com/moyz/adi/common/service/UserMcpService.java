package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.UserMcpDto;
import com.moyz.adi.common.dto.mcp.McpCommonParam;
import com.moyz.adi.common.dto.mcp.McpCustomizedParamDefinition;
import com.moyz.adi.common.dto.mcp.UserMcpCustomizedParam;
import com.moyz.adi.common.dto.mcp.UserMcpUpdateReq;
import com.moyz.adi.common.entity.Mcp;
import com.moyz.adi.common.entity.UserMcp;
import com.moyz.adi.common.mapper.UserMcpMapper;
import com.moyz.adi.common.util.AesUtil;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.PrivilegeUtil;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.agent.tool.ToolSpecification;
import dev.langchain4j.mcp.client.DefaultMcpClient;
import dev.langchain4j.mcp.client.McpClient;
import dev.langchain4j.mcp.client.transport.McpTransport;
import dev.langchain4j.mcp.client.transport.http.HttpMcpTransport;
import dev.langchain4j.mcp.client.transport.stdio.StdioMcpTransport;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static com.moyz.adi.common.enums.ErrorEnum.A_USER_MCP_SERVER_NOT_FOUND;
import static java.util.stream.Collectors.toMap;

@Slf4j
@Service
public class UserMcpService extends ServiceImpl<UserMcpMapper, UserMcp> {

    @Resource
    private McpService mcpService;

    public List<UserMcp> searchEnableByUserId(Long userId) {
        return this.lambdaQuery()
                .eq(UserMcp::getUserId, userId)
                .eq(UserMcp::getIsEnable, true)
                .eq(UserMcp::getIsDeleted, false)
                .list();
    }

    public Page<UserMcpDto> searchByUserId(Long userId, Integer currentPage, Integer pageSize) {
        Page<UserMcp> page = this.lambdaQuery()
                .eq(UserMcp::getUserId, userId)
                .eq(UserMcp::getIsDeleted, false)
                .orderByDesc(UserMcp::getUpdateTime)
                .page(new Page<>(currentPage, pageSize));

        List<UserMcpDto> dtoList = new ArrayList<>();

        List<Mcp> mcpList = new ArrayList<>();
        if (!page.getRecords().isEmpty()) {
            mcpList = mcpService.listByIds(page.getRecords().stream()
                    .map(UserMcp::getMcpId)
                    .distinct()
                    .toList());
        }
        for (UserMcp userMcp : page.getRecords()) {
            UserMcpDto dto = new UserMcpDto();
            BeanUtils.copyProperties(userMcp, dto);

            Mcp mcp = mcpList.stream()
                    .filter(item -> item.getId().equals(dto.getMcpId()))
                    .findFirst()
                    .orElse(null);
            setMcpInfo(dto, mcp);
            dtoList.add(dto);
        }
        Page<UserMcpDto> result = MPPageUtil.convertToPage(page, UserMcpDto.class);
        result.setRecords(dtoList);
        return result;
    }

    public UserMcpDto saveOrUpdate(UserMcpUpdateReq editReq) {
        if (null == editReq.getMcpCustomizedParams() && null == editReq.getIsEnable()) {
            log.warn("UserMcp edit request is empty, editReq: {}", editReq);
            return null;
        }
        Mcp mcp = mcpService.getOrThrow(editReq.getMcpId(), false);
        UserMcp userMcp = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(UserMcp::getMcpId, editReq.getMcpId())
                .eq(UserMcp::getUserId, ThreadContext.getCurrentUserId())
                .eq(UserMcp::getIsDeleted, false)
                .one();
        List<UserMcpCustomizedParam> paramSettings = editReq.getMcpCustomizedParams();
        if (null == userMcp) {
            userMcp = new UserMcp();
            userMcp.setUuid(UuidUtil.createShort());
            userMcp.setMcpId(editReq.getMcpId());
            userMcp.setUserId(ThreadContext.getCurrentUserId());
            if (null != paramSettings) {
                encryptParams(paramSettings, mcp);
                userMcp.setMcpCustomizedParams(paramSettings);
            }
            if (null != editReq.getIsEnable()) {
                userMcp.setIsEnable(editReq.getIsEnable());
            }
            baseMapper.insert(userMcp);
        } else {
            UserMcp updateObj = new UserMcp();
            updateObj.setId(userMcp.getId());
            if (null != paramSettings) {
                encryptParams(paramSettings, mcp);
                updateObj.setMcpCustomizedParams(paramSettings);
            }
            if (null != editReq.getIsEnable()) {
                updateObj.setIsEnable(editReq.getIsEnable());
            }
            baseMapper.updateById(userMcp);
        }
        UserMcpDto dto = new UserMcpDto();
        BeanUtils.copyProperties(userMcp, dto);
        setMcpInfo(dto, mcp);
        return dto;
    }

    public List<McpClient> createMcpClients(Long userId, List<Long> mcpIds) {
        List<McpClient> result = new ArrayList<>();
        if (mcpIds == null || mcpIds.isEmpty()) {
            log.warn("No MCP IDs provided for creating MCP clients.");
            return result;
        }

        //过滤出用户已启用的MCP
        Map<Long, UserMcp> mcpIdToUserMcp = this.lambdaQuery()
                .in(UserMcp::getMcpId, mcpIds)
                .eq(UserMcp::getUserId, userId)
                .eq(UserMcp::getIsDeleted, false)
                .eq(UserMcp::getIsEnable, true)
                .list()
                .stream()
                .collect(toMap(UserMcp::getMcpId, Function.identity(), (a, b) -> a));

        // 查询MCP信息
        List<Mcp> mcpInfos = mcpService.listByIds(mcpIdToUserMcp.keySet().stream().toList(), true);
        for (Mcp mcp : mcpInfos) {
            UserMcp userMcp = mcpIdToUserMcp.get(mcp.getId());
            if (userMcp == null) {
                log.warn("No user MCP params found for MCP ID: {}", mcp.getId());
                continue;
            }

            // 解密用户设置的MCP参数
            decryptParams(userMcp.getMcpCustomizedParams(), mcp);

            McpTransport transport;
            if (AdiConstant.McpConstant.TRANSPORT_TYPE_SSE.equals(mcp.getTransportType())) {
                String httpQueryString = createHttpQueryString(mcp, userMcp);
                String url = mcp.getSseUrl();
                if (!url.contains("?")) {
                    url = url + "?";
                }
                transport = new HttpMcpTransport.Builder()
                        .sseUrl(url + httpQueryString)
                        .timeout(Duration.ofSeconds(mcp.getSseTimeout() > 0 ? mcp.getSseTimeout() : 30))
                        .logRequests(true)
                        .logResponses(true)
                        .build();
            } else {
                Map<String, String> environment = createEnvironment(mcp, userMcp);
                transport = new StdioMcpTransport.Builder()
                        .command(List.of(mcp.getStdioCommand(), mcp.getStdioArg()))
                        .environment(environment)
                        .build();
            }
            McpClient mcpClient = new DefaultMcpClient.Builder()
                    .transport(transport)
                    .build();
            result.add(mcpClient);
        }

        return result;
    }

    public void enable(String uuid) {
        UserMcp userMcp = PrivilegeUtil.checkAndGetByUuid(uuid, this.query(), A_USER_MCP_SERVER_NOT_FOUND);
        UserMcp updateObj = new UserMcp();
        updateObj.setId(userMcp.getId());
        updateObj.setIsEnable(true);
        baseMapper.updateById(updateObj);
    }

    public void softDelete(String uuid) {
        PrivilegeUtil.checkAndDelete(uuid, this.query(), this.update(), A_USER_MCP_SERVER_NOT_FOUND);
    }

    /**
     * 加密用户的MCP设置（仅对需要加密的字段进行加密）
     * ps：目前暂时只在数据库层做加密，前后端交互时数据加解密方式待定
     *
     * @param setting mcp设置项
     * @param mcp     mcp对象
     */
    private void encryptParams(List<UserMcpCustomizedParam> setting, Mcp mcp) {
        for (UserMcpCustomizedParam userMcpCustomizedParam : setting) {
            mcp.getCustomizedParamDefinitions().stream().filter(item -> item.getName().equals(userMcpCustomizedParam.getName()) && Boolean.TRUE.equals(item.getRequireEncrypt()))
                    .findFirst()
                    .ifPresent(item -> {
                        userMcpCustomizedParam.setValue(AesUtil.encrypt(String.valueOf(userMcpCustomizedParam.getValue())));
                        userMcpCustomizedParam.setEncrypted(true);
                    });
        }
    }

    /**
     * 解密用户设置的MCP参数
     *
     * @param mcpParams 用户设置的已加密的mcp参数
     * @param mcp       mcp对象
     */
    private void decryptParams(List<UserMcpCustomizedParam> mcpParams, Mcp mcp) {
        for (UserMcpCustomizedParam userMcpCustomizedParam : mcpParams) {
            mcp.getCustomizedParamDefinitions().stream()
                    .filter(item -> item.getName().equals(userMcpCustomizedParam.getName()) && Boolean.TRUE.equals(item.getRequireEncrypt()))
                    .findFirst()
                    .ifPresent(item -> {
                        userMcpCustomizedParam.setValue(AesUtil.decrypt(String.valueOf(userMcpCustomizedParam.getValue())));
                        userMcpCustomizedParam.setEncrypted(false);
                    });
        }
    }

    public void setMcpInfo(UserMcpDto dto, Mcp mcp) {
        if (mcp == null) {
            return;
        }
        dto.setMcpInfo(mcp);
        //解密以传到前端
        decryptParams(dto.getMcpCustomizedParams(), mcp);
    }

    /**
     * 创建MCP server中以http方式传输时所需的查询参数
     *
     * @param mcp     MCP对象
     * @param userMcp 用户MCP对象
     * @return 查询参数字符串
     */
    private String createHttpQueryString(Mcp mcp, UserMcp userMcp) {
        StringBuilder httpQueryParams = new StringBuilder();
        Map<String, String> environment = createEnvironment(mcp, userMcp);
        for (Map.Entry<String, String> entry : environment.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();
            if (value != null && !value.isEmpty()) {
                httpQueryParams.append(key).append("=").append(value).append("&");
            }
        }
        if (httpQueryParams.isEmpty()) {
            return ""; // 如果没有参数，则返回空字符串
        }
        return httpQueryParams.substring(0, httpQueryParams.length() - 1); // 去掉最后的&
    }


    /**
     * 创建MCP server中以stdio方式传输时所需的环境变量
     *
     * @param mcp     MCP对象
     * @param userMcp 用户MCP对象
     * @return 环境变量映射
     */
    private Map<String, String> createEnvironment(Mcp mcp, UserMcp userMcp) {
        Map<String, String> environment = new HashMap<>();
        for (McpCommonParam initParams : mcp.getPresetParams()) {
            environment.put(initParams.getName(), String.valueOf(initParams.getValue()));
        }
        for (McpCustomizedParamDefinition uninitParam : mcp.getCustomizedParamDefinitions()) {
            // MCP中定义的未初始化参数，需要使用用户设置的值
            UserMcpCustomizedParam userParam = userMcp.getMcpCustomizedParams().stream()
                    .filter(param -> param.getName().equals(uninitParam.getName()))
                    .findFirst()
                    .orElse(null);
            if (null == userParam) {
                log.warn("No user MCP param found for uninitialized parameter: {}", uninitParam.getName());
                continue;
            }
            environment.put(uninitParam.getName(), String.valueOf(userParam.getValue()));
        }
        return environment;
    }
}
