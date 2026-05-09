package com.moyz.adi.common.workflow;

import com.moyz.adi.common.exception.BaseException;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;

import java.util.HashSet;
import java.util.Set;

@Slf4j
@EqualsAndHashCode(callSuper = true)
@Data
public class GraphCompileNode extends CompileNode {
    private CompileNode root;

    public void appendToLeaf(CompileNode node) {
        boolean exists = false;
        CompileNode tail = root;
        Set<String> visited = new HashSet<>();
        visited.add(tail.getId());
        while (!tail.getNextNodes().isEmpty()) {
            tail = tail.getNextNodes().get(0);
            if (!visited.add(tail.getId())) {
                log.error("appendToLeaf检测到链表环，节点{}", tail.getId());
                throw new BaseException(com.moyz.adi.common.enums.ErrorEnum.B_WF_RUN_ERROR);
            }
            if (tail.getId().equals(node.getId())) {
                exists = true;
                break;
            }
        }
        if (!exists) {
            tail.getNextNodes().add(node);
        }
    }

    public CompileNode getTail() {
        if (root.nextNodes.isEmpty()) {
            return root;
        }
        CompileNode tail = root.nextNodes.get(0);
        Set<String> visited = new HashSet<>();
        visited.add(root.getId());
        visited.add(tail.getId());
        while (!tail.getNextNodes().isEmpty()) {
            tail = tail.getNextNodes().get(0);
            if (!visited.add(tail.getId())) {
                log.error("getTail检测到链表环，节点{}", tail.getId());
                throw new BaseException(com.moyz.adi.common.enums.ErrorEnum.B_WF_RUN_ERROR);
            }
        }
        return tail;
    }
}
