package com.moyz.adi.common.helper;

import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

@Slf4j
public class HttpHelper {
    private HttpHelper(){}
    public static String getBodyString(HttpServletRequest request) throws IOException {
        StringBuilder sb = new StringBuilder();
        try (
                InputStream inputStream = request.getInputStream();
                BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))
        ) {
            String line = "";
            while ((line = reader.readLine()) != null) {
                sb.append(line);
            }
        } catch (IOException e) {
            log.error("error", e);
        }
        return sb.toString();
    }
}