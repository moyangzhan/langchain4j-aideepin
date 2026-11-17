package com.moyz.adi.common.file;

import com.moyz.adi.common.entity.AdiFile;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * readLocalImage 读取后不得持有文件句柄：图片渲染（原图/缩略图）紧邻文件删除，
 * Windows 上句柄未释放会令物理删除失败。
 * <p>
 * readLocalImage must not hold the file handle after returning: image rendering is
 * immediately followed by file deletion, and on Windows an open handle makes deleting
 * the physical file fail.
 * </p>
 */
class LocalFileUtilTest {

    @TempDir
    Path tempDir;

    @Test
    void readLocalImageReleasesFileHandleSoDeleteSucceeds() throws Exception {
        Path image = tempDir.resolve("pic.png");
        BufferedImage source = new BufferedImage(3, 2, BufferedImage.TYPE_INT_RGB);
        ImageIO.write(source, "png", image.toFile());

        AdiFile adiFile = new AdiFile();
        adiFile.setUuid("0123456789abcdef0123456789abcdef");
        adiFile.setExt("png");
        adiFile.setPath(image.toString());

        BufferedImage read = LocalFileUtil.readLocalImage(adiFile, false, tempDir.resolve("thumbnails").toString());
        assertEquals(3, read.getWidth());
        assertEquals(2, read.getHeight());

        assertTrue(Files.deleteIfExists(image));
    }
}
