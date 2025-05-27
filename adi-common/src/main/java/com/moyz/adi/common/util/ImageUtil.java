package com.moyz.adi.common.util;

import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisOutput;
import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisResult;
import com.alibaba.dashscope.exception.NoApiKeyException;
import com.alibaba.dashscope.utils.OSSUtils;
import com.moyz.adi.common.service.FileService;
import dev.langchain4j.data.message.Content;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.internal.Utils;
import jakarta.activation.MimetypesFileTypeMap;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class ImageUtil {

    public static boolean isRGB(String imagePath) {
        try {
            // 读取图片
            BufferedImage image = ImageIO.read(new File(imagePath));
            // 获取图片的颜色模型
            int colorModel = image.getColorModel().getColorSpace().getType();
            if (colorModel == BufferedImage.TYPE_INT_RGB) {
                return true;
            }
        } catch (IOException e) {
            log.error("isARGB error", e);
        }
        return false;
    }

    public static void rgbConvertToRgba(String rbgPath, String argbPath) {
        try {
            // 读取RGB图片
            BufferedImage rgbImage = ImageIO.read(new File(rbgPath));

            // 创建一个RGBA图片，与原始RGB图片大小相同
            BufferedImage rgbaImage = new BufferedImage(rgbImage.getWidth(), rgbImage.getHeight(), BufferedImage.TYPE_INT_ARGB);

            // 将RGB图片绘制到RGBA图片上，并设置透明度为不透明
            Graphics2D g = rgbaImage.createGraphics();
            g.drawImage(rgbImage, 0, 0, null);
            g.dispose();

            // 保存RGBA图片
            ImageIO.write(rgbaImage, "png", new File(argbPath));

            System.out.println("转换完成！");
        } catch (IOException e) {
            log.error("error", e);
        }
    }

    public static File rgbConvertToRgba(File file, String rgbaPath) {
        try {
            BufferedImage image = ImageIO.read(file);

            // 获取图片的颜色模型
            int colorModel = image.getColorModel().getColorSpace().getType();
            if (colorModel != BufferedImage.TYPE_INT_ARGB) {
                // 创建一个RGBA图片，与原始RGB图片大小相同
                BufferedImage rgbaImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_ARGB);

                // 将RGB图片绘制到RGBA图片上，并设置透明度为不透明
                Graphics2D g = rgbaImage.createGraphics();
                g.drawImage(image, 0, 0, null);
                g.dispose();
                // 保存RGBA图片
                ImageIO.write(rgbaImage, "png", new File(rgbaPath));

                return new File(rgbaPath);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return file;
    }

    /**
     * 图片地址转ImageContent
     * 如果是本地图片则转成base64，其他网站的图片使用url
     *
     * @param imageUrls
     * @return
     */
    public static List<Content> urlsToImageContent(List<String> imageUrls) {
        if (CollectionUtils.isEmpty(imageUrls)) {
            return new ArrayList<>();
        }
        List<Content> result = new ArrayList<>();
        try {
            for (String imageUrl : imageUrls) {
                log.info("urlsToImageContent,imageUrl:{}", imageUrl);
                if (!imageUrl.contains("http") && imageUrl.length() == 32) {
                    String absolutePath = SpringUtil.getBean(FileService.class).getImagePath(imageUrl);
                    File file = new File(absolutePath);
                    MimetypesFileTypeMap mimetypesFileTypeMap = new MimetypesFileTypeMap();
                    String mimeType = mimetypesFileTypeMap.getContentType(file);
                    try (FileInputStream fileInputStream = new FileInputStream(file)) {
                        byte[] fileBytes = new byte[(int) file.length()];
                        fileInputStream.read(fileBytes);
                        result.add(ImageContent.from(Base64.getEncoder().encodeToString(fileBytes), mimeType));
                    }
                } else {
                    result.add(ImageContent.from(imageUrl));
                }
            }
        } catch (IOException e) {
            log.error("urlsToImageContent error", e);
        }
        return result;
    }

    public static void createThumbnail(String inputFile, String outputFile, int thumbWidth, int thumbHeight, int quality) throws IOException {
        File input = new File(inputFile);
        BufferedImage image = ImageIO.read(input);
        int imageWidth = image.getWidth();
        int imageHeight = image.getHeight();

        // 计算缩放比例
        double thumbRatio = (double)thumbWidth / (double)thumbHeight;
        double imageRatio = (double)imageWidth / (double)imageHeight;
        if (thumbRatio < imageRatio) {
            thumbHeight = (int)(thumbWidth / imageRatio);
        } else {
            thumbWidth = (int)(thumbHeight * imageRatio);
        }

        // 创建缩略图
        Image thumbnail = image.getScaledInstance(thumbWidth, thumbHeight, Image.SCALE_SMOOTH);
        BufferedImage outputImage = new BufferedImage(thumbWidth, thumbHeight, BufferedImage.TYPE_INT_RGB);
        outputImage.getGraphics().drawImage(thumbnail, 0, 0, null);

        // 保存缩略图
        File output = new File(outputFile);
        ImageIO.write(outputImage, "JPEG", output);
    }

    public static List<dev.langchain4j.data.image.Image> imagesFrom(ImageSynthesisResult result) {
        return Optional.of(result)
                .map(ImageSynthesisResult::getOutput)
                .map(ImageSynthesisOutput::getResults)
                .orElse(Collections.emptyList())
                .stream()
                .map(resultMap -> resultMap.get("url"))
                .map(url -> dev.langchain4j.data.image.Image.builder().url(url).build())
                .collect(Collectors.toList());
    }

    public static String imageUrl(dev.langchain4j.data.image.Image image, String model, String apiKey) {
        String imageUrl;

        if (image.url() != null) {
            imageUrl = image.url().toString();
        } else if (Utils.isNotNullOrBlank(image.base64Data())) {
            String filePath = saveDataAsTemporaryFile(image.base64Data(), image.mimeType());
            try {
                imageUrl = OSSUtils.upload(model, filePath, apiKey);
            } catch (NoApiKeyException e) {
                throw new RuntimeException(e);
            }
        } else {
            throw new IllegalArgumentException("Failed to get image url from " + image);
        }

        return imageUrl;
    }

    public static String saveDataAsTemporaryFile(String base64Data, String mimeType) {
        String tmpDir = System.getProperty("java.io.tmpdir", "/tmp");
        String tmpFileName = UUID.randomUUID().toString();
        if (Utils.isNotNullOrBlank(mimeType)) {
            // e.g. "image/png", "image/jpeg"...
            int lastSlashIndex = mimeType.lastIndexOf("/");
            if (lastSlashIndex >= 0 && lastSlashIndex < mimeType.length() - 1) {
                String fileSuffix = mimeType.substring(lastSlashIndex + 1);
                tmpFileName = tmpFileName + "." + fileSuffix;
            }
        }

        Path tmpFilePath = Paths.get(tmpDir, tmpFileName);
        byte[] data = Base64.getDecoder().decode(base64Data);
        try {
            Files.copy(new ByteArrayInputStream(data), tmpFilePath, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return tmpFilePath.toAbsolutePath().toString();
    }
}
