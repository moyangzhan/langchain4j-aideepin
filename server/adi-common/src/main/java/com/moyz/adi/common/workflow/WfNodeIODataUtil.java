package com.moyz.adi.common.workflow;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.enums.WfIODataTypeEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.CollectionUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataFilesContent;
import com.moyz.adi.common.workflow.def.*;
import org.apache.commons.collections4.CollectionUtils;

import java.util.*;

import static com.moyz.adi.common.cosntant.AdiConstant.IMAGE_EXTENSIONS;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_INPUT_PARAM_NAME;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;

public class WfNodeIODataUtil {

    public static final Map<WfIODataTypeEnum, Class<? extends WfNodeIO>> INPUT_TYPE_TO_NODE_IO_DEF = new HashMap<>();

    static {
        INPUT_TYPE_TO_NODE_IO_DEF.put(WfIODataTypeEnum.TEXT, WfNodeIOText.class);
        INPUT_TYPE_TO_NODE_IO_DEF.put(WfIODataTypeEnum.BOOL, WfNodeIOBool.class);
        INPUT_TYPE_TO_NODE_IO_DEF.put(WfIODataTypeEnum.NUMBER, WfNodeIONumber.class);
        INPUT_TYPE_TO_NODE_IO_DEF.put(WfIODataTypeEnum.OPTIONS, WfNodeIOOptions.class);
        INPUT_TYPE_TO_NODE_IO_DEF.put(WfIODataTypeEnum.FILES, WfNodeIOFiles.class);

    }

    public static NodeIOData createNodeIOData(ObjectNode data) {
        JsonNode nameObj = data.get("name");
        JsonNode content = data.get("content");
        if (null == nameObj || null == content) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        String name = nameObj.asText();
        JsonNode typeNode = content.get("type");
        if (null == typeNode) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        Integer type = typeNode.asInt();
        JsonNode titleNode = content.get("title");
        String title = null != titleNode ? titleNode.asText() : name;
        JsonNode value = content.get("value");
        NodeIOData result = null;
        if (WfIODataTypeEnum.TEXT.getValue().equals(type)) {
            result = NodeIOData.createByText(name, title, value.asText());
        } else if (WfIODataTypeEnum.NUMBER.getValue().equals(type)) {
            result = NodeIOData.createByNumber(name, title, value.asDouble());
        } else if (WfIODataTypeEnum.BOOL.getValue().equals(type)) {
            result = NodeIOData.createByBool(name, title, value.asBoolean());
        } else if (WfIODataTypeEnum.FILES.getValue().equals(type)) {
            if (value.isArray()) {
                List<String> fileUrls = new ArrayList<>();
                Iterator<JsonNode> iterator = value.values().iterator();
                while (iterator.hasNext()) {
                    fileUrls.add(iterator.next().asText());
                }
                result = NodeIOData.createByFiles(name, title, fileUrls);
            }
        } else if (WfIODataTypeEnum.OPTIONS.getValue().equals(type)) {
            if (value instanceof ObjectNode) {
                result = NodeIOData.createByOptions(name, title, JsonUtil.toMap(value));
            }
        }
        return result;
    }

    /**
     * 1.如果没有名称为 output 的输出参数，则需要新增 <br/>
     * 2.判断是否已经有文本类型的输出参数，如果有，则复制该参数并将参数名改为 output <br/>
     * 3.如果没有文本类型的参数，则复制第一个参数，并将参数名改为 output
     *
     * @param inputs 输入参数列表
     * @return 输出参数列表
     */
    public static List<NodeIOData> changeInputsToOutputs(List<NodeIOData> inputs) {
        if (CollectionUtils.isEmpty(inputs)) {
            return new ArrayList<>();
        }
        List<NodeIOData> result = CollectionUtil.deepCopy(inputs);

        boolean outputExist = false;
        NodeIOData defaultInputName = null, txtExist = null, first = null;
        for (NodeIOData nodeIOData : result) {
            if (null == first) {
                first = nodeIOData;
            }
            if (DEFAULT_OUTPUT_PARAM_NAME.equals(nodeIOData.getName())) {
                outputExist = true;
            } else if (DEFAULT_INPUT_PARAM_NAME.equals(nodeIOData.getName())) {
                defaultInputName = nodeIOData;
            } else if (null == txtExist && WfIODataTypeEnum.TEXT.getValue().equals(nodeIOData.getContent().getType())) {
                txtExist = nodeIOData;
            }
        }

        if (outputExist) {
            return result;
        }

        if (null != defaultInputName) {
            defaultInputName.setName(DEFAULT_OUTPUT_PARAM_NAME);
        } else if (null != txtExist) {
            txtExist.setName(DEFAULT_OUTPUT_PARAM_NAME);
        } else if (null != first) {
            first.setName(DEFAULT_OUTPUT_PARAM_NAME);
        }

        return result;
    }

    /**
     * 将输入输出中的文件url转成markdown格式的文件地址<br/>
     * 将变量渲染到模板时使用该方法，其他情况交由前端处理
     *
     * @param ioDataList 输入输出列表
     */
    public static void changeFilesContentToMarkdown(List<NodeIOData> ioDataList) {
        ioDataList.forEach(input -> {
            if (input.getContent() instanceof NodeIODataFilesContent filesContent) {
                List<String> newValues = new ArrayList<>();
                for (String s : filesContent.getValue()) {
                    if (IMAGE_EXTENSIONS.contains(s.substring(s.lastIndexOf(".") + 1))) {
                        newValues.add("![" + filesContent.getTitle() + "](" + s + ")");
                    } else {
                        newValues.add("[" + filesContent.getTitle() + "](" + s + ")");
                    }
                }
                filesContent.setValue(newValues);
            }
        });
    }

    /**
     * 从持久化的 {参数名: {type, title, value}} JSON 中提取 type=4 文件参数的纯 uuid：
     * URL 形态（http(s) 外链与本地 /file/ 相对链接——画图节点产物以 URL 持久化，其引用由 GC 复核面覆盖）
     * 不是 adi_file 的 uuid，直接跳过；结果去重。运行建立时的引用登记与运行删除时的引用解除共用此源，保持计数对称。
     * <p>
     * Extract plain uuids from type=4 (files) parameters of a persisted {name: {type, title, value}} JSON:
     * URL-shaped values (http(s) links and local /file/ relative links — draw-node products persist as
     * URLs; the GC recount surface covers them) are not adi_file uuids and are skipped; the result is
     * deduplicated. Reference attach on run creation and detach on runtime deletion share this source
     * so the counting stays symmetric.
     * </p>
     */
    public static List<String> extractFileUuids(ObjectNode persisted) {
        List<String> result = new ArrayList<>();
        collectFileUuids(persisted, false, result);
        return result;
    }

    /**
     * 提取 type=4 文件参数涉及的全部文件 uuid，与复核 SQL 同口径：URL 形态归一化为末段路径
     * 去扩展名后的 uuid（URL 产物无 detach 路径，引用消散后靠实查钳制收敛）；结果去重。
     * <p>
     * Extract every file uuid touched by type=4 (files) parameters, normalized the same way as the
     * recount SQL: URL-shaped values reduce to the uuid in the last path segment without extension
     * (URL products have no detach path; their counts converge through the live clamp once the
     * reference dissolves); the result is deduplicated.
     * </p>
     */
    public static List<String> extractFileUuidsForRecount(ObjectNode persisted) {
        List<String> result = new ArrayList<>();
        collectFileUuids(persisted, true, result);
        return result;
    }

    private static void collectFileUuids(ObjectNode persisted, boolean normalizeUrls, List<String> result) {
        if (null == persisted) {
            return;
        }
        persisted.properties().forEach(entry -> {
            JsonNode content = entry.getValue();
            if (content instanceof ObjectNode contentObj
                    && WfIODataTypeEnum.FILES.getValue().equals(contentObj.path("type").asInt(-1))) {
                JsonNode value = contentObj.path("value");
                if (value.isArray()) {
                    value.forEach(item -> {
                        String text = item.asText("");
                        if (item.isNull() || text.isBlank()) {
                            return;
                        }
                        boolean urlShaped = text.startsWith("http") || text.startsWith("/file/");
                        if (urlShaped && !normalizeUrls) {
                            return;
                        }
                        String uuid = urlShaped ? normalizeUrlToUuid(text) : text;
                        if (!uuid.isBlank() && !result.contains(uuid)) {
                            result.add(uuid);
                        }
                    });
                }
            }
        });
    }

    private static String normalizeUrlToUuid(String text) {
        String lastSegment = text.substring(text.lastIndexOf('/') + 1);
        int dot = lastSegment.lastIndexOf('.');
        return dot > 0 && dot < lastSegment.length() - 1 ? lastSegment.substring(0, dot) : lastSegment;
    }
}
