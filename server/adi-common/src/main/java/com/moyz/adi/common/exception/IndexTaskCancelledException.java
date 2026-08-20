package com.moyz.adi.common.exception;

/**
 * 索引任务协作式取消：任务在安全点（每个 embed 批次前等）检测到版本推进后抛出，
 * 由 IndexTaskService 捕获并执行自清理 + 最新版本重入队。不用于错误场景。
 */
public class IndexTaskCancelledException extends RuntimeException {

    public IndexTaskCancelledException(String message) {
        super(message);
    }
}
