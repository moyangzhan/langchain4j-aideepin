package com.moyz.adi.common.enums;

public enum ErrorEnum {
    SUCCESS("00000", "成功"),
    A_URL_NOT_FOUND("A0001", "地址不存在"),
    A_PARAMS_ERROR("A0002", "参数校验不通过"),
    A_REQUEST_TOO_MUCH("A0003", "访问次数太多"),
    A_LOGIN_ERROR("A0004", "登陆失败，账号或密码错误"),
    A_LOGIN_ERROR_MAX("A0005", "失败次数太多，请输入验证码重试"),
    A_LOGIN_CAPTCHA_ERROR("A0006", "验证码不正确"),
    A_USER_NOT_EXIST("A0007", "用户不存在"),
    A_CONVERSATION_NOT_EXIST("A0008", "对话不存在"),
    A_IMAGE_NUMBER_ERROR("A0009", "图片数量不对"),
    A_IMAGE_SIZE_ERROR("A0010", "图片尺寸不对"),
    A_FILE_NOT_EXIST("A0011", "文件不存在"),
    A_DRAWING("A0012", "作图还未完成"),
    A_USER_EXIST("A0013", "账号已经存在，请使用账号密码登录"),
    A_FIND_PASSWORD_CODE_ERROR("A0014", "重置码已过期或不存在"),
    A_USER_WAIT_CONFIRM("A0015", "用户未激活"),
    A_USER_NOT_AUTH("A0016", "用户无权限"),
    A_DATA_NOT_FOUND("A0017", "数据不存在"),
    A_UPLOAD_FAIL("A0018", "上传失败"),
    A_QA_ASK_LIMIT("A0019", "请求次数太多"),
    A_QA_ITEM_LIMIT("A0020", "知识点生成已超额度"),
    A_CONVERSATION_EXIST("A0021", "会话(角色)已存在"),
    A_MODEL_NOT_FOUND("A0022", "模型不存在"),
    A_MODEL_ALREADY_EXIST("A0023", "模型已存在"),
    A_CONVERSATION_NOT_FOUND("A0024", "会话(角色)找不到"),
    A_AI_IMAGE_NOT_FOUND("A0024", "图片找不到"),
    A_ENABLE_MODEL_NOT_FOUND("A0025", "没有可用的模型"),
    A_DOC_INDEX_DOING("A0026", "文档索引正在进行中，请稍后重试"),
    A_PRESET_CONVERSATION_NOT_EXIST("A0027", "预设会话或角色不存在"),
    A_CONVERSATION_TITLE_EXIST("A0028", "会话(角色)标题已存在"),
    A_AI_IMAGE_NO_AUTH("A0029", "无权限查看该图片"),
    A_USER_NOT_FOUND("A0030", "用户不存在"),
    A_ACTIVE_CODE_INVALID("A0031", "激活码已失效"),
    A_OLD_PASSWORD_INVALID("A0032", "原密码不正确"),
    A_OPT_TOO_FREQUENTLY("A0032", "操作太频繁"),
    A_DRAW_NOT_FOUND("A00233", "绘图记录找不到"),
    B_UNCAUGHT_ERROR("B0001", "未捕捉异常"),
    B_COMMON_ERROR("B0002", "业务出错"),
    B_GLOBAL_ERROR("B0003", "全局异常"),
    B_SAVE_IMAGE_ERROR("B0004", "保存图片异常"),
    B_FIND_IMAGE_404("B0005", "无法找到图片"),
    B_DAILY_QUOTA_USED("B0006", "今天额度已经用完"),
    B_MONTHLY_QUOTA_USED("B0007", "当月额度已经用完"),
    B_LLM_NOT_SUPPORT("B0008", "LLM不支持该功能"),
    B_LLM_SECRET_KEY_NOT_SET("B0009", "LLM的secret key没设置"),
    B_MESSAGE_NOT_FOUND("B0008", "消息不存在"),
    B_LLM_SERVICE_DISABLED("B0009", "LLM服务不可用"),
    B_KNOWLEDGE_BASE_IS_EMPTY("B0010", "知识库内容为空"),
    B_NO_ANSWER("B0011", "[无答案]"),
    B_SAVE_FILE_ERROR("B0012", "保存文件异常"),
    B_BREAK_SEARCH("B0013", "中断搜索"),
    B_GRAPH_FILTER_NOT_FOUND("B0014", "图过滤器未定义"),
    B_DB_ERROR("B0015", "数据库查询异常"),
    B_ACTIVE_USER_ERROR("B0016", "激活用户失败"),
    B_RESET_PASSWORD_ERROR("B0017", "重置密码失败"),
    B_IMAGE_LOAD_ERROR("B0018", "加载图片失败"),
    B_IO_EXCEPTION("B0019", "IO异常"),
    B_SERVER_EXCEPTION("B0020", "服务端异常"),
    B_DELETE_FILE_ERROR("B0020", "删除文件异常"),
    C_DRAW_FAIL("C0001", "大模型生成图片失败,原因:{0}");

    private final String code;
    private final String info;

    ErrorEnum(String code, String info) {
        this.code = code;
        this.info = info;
    }

    public static ErrorEnum getErrorEnum(String code) {
        ErrorEnum result = null;
        for (ErrorEnum c : ErrorEnum.values()) {
            if (c.getCode().equals(code)) {
                result = c;
                break;
            }
        }
        if (null == result) {
            result = B_COMMON_ERROR;
        }
        return result;
    }

    public String getCode() {
        return code;
    }

    public String getInfo() {
        return info;
    }

}
