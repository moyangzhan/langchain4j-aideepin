package com.moyz.adi.common.workflow.node.httprequest;

import com.aliyun.core.utils.StringUtils;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.Consts;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.DefaultHttpRequestRetryHandler;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_ERROR_OUTPUT_PARAM_NAME;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;

@Slf4j
public class HttpRequestNode extends AbstractWfNode {

    public HttpRequestNode(WorkflowComponent wfComponent, WorkflowNode node, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, node, wfState, nodeState);
    }

    protected NodeProcessResult onProcess() {
        List<NodeIOData> outputData = new ArrayList<>();

        HttpRequestNodeConfig nodeConfig = checkAndGetConfig(HttpRequestNodeConfig.class);
        RequestConfig requestConfig = RequestConfig.custom()
                .setConnectionRequestTimeout(nodeConfig.getTimeout() * 1000)
                .setConnectTimeout(nodeConfig.getTimeout() * 1000)
                .build();
        try (CloseableHttpClient httpClient = HttpClients.custom()
                .setDefaultRequestConfig(requestConfig)
                .setRetryHandler(new DefaultHttpRequestRetryHandler(nodeConfig.getRetryTimes(), true))
                .build()) {
            String url = appendParams(nodeConfig.getUrl(), nodeConfig.getParams());
            String contentType = nodeConfig.getContentType();
            HttpUriRequest httpRequest;
            if (HttpGet.METHOD_NAME.equalsIgnoreCase(nodeConfig.getMethod())) {
                httpRequest = new HttpGet(url);
                setHeaders(httpRequest, nodeConfig.getHeaders());
            } else if (HttpPost.METHOD_NAME.equalsIgnoreCase(nodeConfig.getMethod())) {
                HttpPost httpPost = new HttpPost(url);
                httpRequest = httpPost;
                setHeaders(httpRequest, nodeConfig.getHeaders());
                if (contentType.equalsIgnoreCase("text/plain")) {
                    StringEntity jsonEntity = new StringEntity(nodeConfig.getTextBody(), ContentType.TEXT_PLAIN.withCharset(Consts.UTF_8));
                    httpPost.setEntity(jsonEntity);
                } else if (contentType.equalsIgnoreCase("application/json")) {
                    StringEntity jsonEntity = new StringEntity(JsonUtil.toJson(nodeConfig.getJsonBody()), ContentType.APPLICATION_JSON.withCharset(Consts.UTF_8));
                    httpPost.setEntity(jsonEntity);
                } else if (contentType.equalsIgnoreCase("multipart/form-data")) {
                    MultipartEntityBuilder entityBuilder = MultipartEntityBuilder.create();
                    if (nodeConfig.getFormDataBody() != null) {
                        for (HttpRequestNodeConfig.Param entry : nodeConfig.getFormDataBody()) {
                            if (entry.getValue() instanceof File) {
                                entityBuilder.addPart(entry.getName(), new FileBody((File) entry.getValue()));
                            } else {
                                entityBuilder.addTextBody(entry.getName(), entry.getValue().toString());
                            }
                        }
                    }
                    httpPost.setEntity(entityBuilder.build());
                } else if (contentType.equalsIgnoreCase("application/x-www-from-urlencoded")) {
                    StringEntity jsonEntity = new StringEntity(JsonUtil.toJson(nodeConfig.getFormUrlencodedBody()), ContentType.APPLICATION_FORM_URLENCODED.withCharset(Consts.UTF_8));
                    httpPost.setEntity(jsonEntity);
                }
            } else {
                log.error("不支持的请求方式:{}", nodeConfig.getMethod());
                throw new RuntimeException();
            }
            try (CloseableHttpResponse response = httpClient.execute(httpRequest)) {
                int statusCode = response.getStatusLine().getStatusCode();
                String responseBody = EntityUtils.toString(response.getEntity(), Consts.UTF_8);
                if (Boolean.TRUE.equals(nodeConfig.getClearHtml())) {
                    Document doc = Jsoup.parse(responseBody);
                    responseBody = doc.body().text();
                }
                if (statusCode == 200) {
                    NodeIOData output = NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "响应内容", responseBody);
                    outputData.add(output);
                } else {
                    log.error("请求失败:{}", response);
                    NodeIOData output = NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "错误内容", responseBody);
                    outputData.add(output);
                }
                outputData.add(NodeIOData.createByText("status_code", "http状态码", String.valueOf(statusCode)));
            }
        } catch (IOException e) {
            log.error("请求失败:{}", e.getMessage());
        }
        return NodeProcessResult.builder().content(outputData).build();
    }

    private String appendParams(String url, List<HttpRequestNodeConfig.Param> params) {
        String result = "";
        if (!params.isEmpty()) {
            //获取Map集合中的键集合
            Iterator<HttpRequestNodeConfig.Param> iterator = params.iterator();
            StringBuilder stringBuffer = new StringBuilder();
            while (iterator.hasNext()) {
                HttpRequestNodeConfig.Param item = iterator.next();
                String value = item.getValue().toString();
                stringBuffer.append("&");
                stringBuffer.append(item.getName());
                stringBuffer.append("=");
                stringBuffer.append(value);
            }
            result = stringBuffer.toString();
        }
        if (StringUtils.isBlank(result)) {
            return url;
        } else {
            if (url.contains("?")) {
                return url + result;
            } else {
                return url + "?" + result;
            }
        }
    }

    private void setHeaders(HttpUriRequest httpRequest, List<HttpRequestNodeConfig.Param> headers) {
        for (HttpRequestNodeConfig.Param entry : headers) {
            httpRequest.addHeader(entry.getName(), entry.getValue().toString());
        }
    }
}
