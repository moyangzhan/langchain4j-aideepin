package com.moyz.adi.common.service;

import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.vo.AgentRequest;
import com.moyz.adi.common.vo.AgentResult;

/**
 * Agent 服务接口
 * <p>
 * Agent service interface. Phase 1: local execution via Character pipeline.
 * Future Phase 2: A2A protocol for remote agent invocation.
 * </p>
 * <p>
 * Agent 服务接口。第一阶段：通过 Character 管道本地执行。
 * 未来第二阶段：A2A 协议远程 Agent 调用。
 * </p>
 */
public interface AgentService {

    /**
     * 调用 Agent（Character）处理任务
     * <p>
     * Invoke an agent (Character) with the given request.
     * </p>
     *
     * @param request Agent 调用请求 / Agent invocation request
     * @param user    当前用户 / Current user
     * @param uuid    本次调用的唯一标识（用于 token 追踪）/ Unique identifier for this invocation
     * @return Agent 调用结果 / Agent call result
     */
    AgentResult invoke(AgentRequest request, User user, String uuid);
}
