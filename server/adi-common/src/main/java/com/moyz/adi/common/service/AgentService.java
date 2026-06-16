package com.moyz.adi.common.service;

import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.vo.AgentRequest;
import com.moyz.adi.common.vo.AgentResult;

/**
 * Agent execution interface — executes a Character in agent mode.
 *
 * <p>Character is the identity/configuration (system prompt, knowledge base, MCP tools, etc.).
 * When a Character is invoked to perform a task, it runs in agent mode — hence "Agent".
 * Agent = Character in execution.</p>
 *
 * <p>Phase 1: local in-process execution via Character pipeline.
 * Phase 2: A2A protocol for remote agent invocation.</p>
 */
public interface AgentService {

    /**
     * Execute a Character in agent mode with the given request.
     *
     * @param request Agent invocation request (specifies which Character and how to run it)
     * @param user    Current user
     * @param uuid    Unique identifier for this invocation (used for token tracking)
     * @return Agent execution result
     */
    AgentResult invoke(AgentRequest request, User user, String uuid);
}
