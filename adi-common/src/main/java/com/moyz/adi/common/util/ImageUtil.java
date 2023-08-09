package com.moyz.adi.common.util;

import lombok.extern.slf4j.Slf4j;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

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
            e.printStackTrace();
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
}
