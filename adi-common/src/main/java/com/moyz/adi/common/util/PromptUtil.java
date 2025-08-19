package com.moyz.adi.common.util;

import com.moyz.adi.common.exception.BaseException;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.data.message.Content;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.TextContent;
import dev.langchain4j.model.input.Prompt;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.moyz.adi.common.cosntant.AdiConstant.*;
import static com.moyz.adi.common.enums.ErrorEnum.A_USER_QUESTION_NOT_FOUND;

public class PromptUtil {

    public static String createPrompt(String question, String information, String extraInfo) {
        if (StringUtils.isBlank(question)) {
            throw new BaseException(A_USER_QUESTION_NOT_FOUND);
        }
        if (StringUtils.isAllBlank(information, extraInfo)) {
            return question;
        } else if (StringUtils.isBlank(information) && StringUtils.isNotBlank(extraInfo)) {
            return PROMPT_EXTRA_TEMPLATE.apply(Map.of("question", question, "extraInfo", extraInfo)).text();
        } else if (StringUtils.isNotBlank(information) && StringUtils.isBlank(extraInfo)) {
            return PROMPT_INFO_TEMPLATE.apply(Map.of("question", question, "information", Matcher.quoteReplacement(information))).text();
        }
        return PROMPT_INFO_EXTRA_TEMPLATE.apply(Map.of("question", question, "information", Matcher.quoteReplacement(information), "extraInfo", extraInfo)).text();
    }

    public static List<Content> findImagesContentInParameters(Method method, Object[] args) {
        return Arrays.stream(method.getParameters())
                .filter(PromptUtil::isImageParameter)
                .flatMap(parameter -> {
                    List<ImageContent> imageContents = new ArrayList<>();
                    Object arg = args[Arrays.asList(method.getParameters()).indexOf(parameter)];
                    if (arg instanceof List imageList) {
                        for (Object ic : imageList) {
                            if (ic instanceof ImageContent imageContent) imageContents.add(imageContent);
                        }
                    }
                    return imageContents.stream();
                })
                .collect(Collectors.toList());
    }

    public static boolean isImageParameter(Parameter parameter) {
        return parameter.isAnnotationPresent(dev.langchain4j.service.UserMessage.class) &&
               (
                       parameter.getType().equals(Image.class)
                       || parameter.getType().equals(ImageContent.class)
                       || parameter.getType().equals(List.class)
               );
    }

    public static List<Content> promptAndImages(Prompt prompt, List<Content> imageContent) {
        return Stream.concat(Stream.of(TextContent.from(prompt.text())), imageContent.stream()).toList();
    }
}
