package com.moyz.adi.chat.controller.externalapi.v1;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.extapi.ExtApiWfRunReq;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.workflow.WorkflowStarter;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "External API - Workflow")
@RestController
@RequestMapping("/ext/v1/workflow")
@Validated
public class ExtWorkflowController {

    @Resource
    private WorkflowStarter workflowStarter;

    @Operation(summary = "Run a workflow")
    @PostMapping(value = "/run")
    public Object workflowRun(@RequestBody ExtApiWfRunReq req) {
        User user = ThreadContext.getCurrentUser();
        String wfUuid = ThreadContext.getExtApiEntityUuid();

        List<ObjectNode> inputs = req.getInputs() != null ? req.getInputs() : List.of();

        if ("blocking".equalsIgnoreCase(req.getResponseMode())) {
            return workflowStarter.blocking(user, wfUuid, inputs);
        }

        return workflowStarter.streaming(user, wfUuid, inputs);
    }
}
