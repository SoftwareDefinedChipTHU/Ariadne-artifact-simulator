import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths}
import java.util.Arrays

package OutParse{
object Main{
    private case class Encoding(charset: Charset, bomBytes: Int)

    private def readSample(path: java.nio.file.Path, limit: Int = 4096): Array[Byte] = {
        val input = Files.newInputStream(path)
        val buffer = new Array[Byte](limit)
        var total = 0

        try {
            var bytesRead = input.read(buffer, total, buffer.length - total)
            while (bytesRead >= 0 && total < buffer.length) {
                total += bytesRead
                if (total < buffer.length) {
                    bytesRead = input.read(buffer, total, buffer.length - total)
                }
            }
        } finally {
            input.close()
        }

        Arrays.copyOf(buffer, total)
    }

    private def detectEncoding(sample: Array[Byte]): Encoding = {
        if (sample.length >= 3 &&
            sample(0) == 0xEF.toByte &&
            sample(1) == 0xBB.toByte &&
            sample(2) == 0xBF.toByte) {
            Encoding(StandardCharsets.UTF_8, 3)
        } else if (sample.length >= 2 &&
                   sample(0) == 0xFF.toByte &&
                   sample(1) == 0xFE.toByte) {
            Encoding(StandardCharsets.UTF_16LE, 2)
        } else if (sample.length >= 2 &&
                   sample(0) == 0xFE.toByte &&
                   sample(1) == 0xFF.toByte) {
            Encoding(StandardCharsets.UTF_16BE, 2)
        } else {
            var evenZeroBytes = 0
            var oddZeroBytes = 0
            var index = 0

            while (index < sample.length) {
                if (sample(index) == 0) {
                    if ((index & 1) == 0) evenZeroBytes += 1
                    else oddZeroBytes += 1
                }
                index += 1
            }

            val pairs = sample.length / 2
            if (pairs > 0 && oddZeroBytes * 2 >= pairs && oddZeroBytes > evenZeroBytes * 2) {
                Encoding(StandardCharsets.UTF_16LE, 0)
            } else if (pairs > 0 && evenZeroBytes * 2 >= pairs && evenZeroBytes > oddZeroBytes * 2) {
                Encoding(StandardCharsets.UTF_16BE, 0)
            } else {
                Encoding(StandardCharsets.UTF_8, 0)
            }
        }
    }

    private def skipFully(input: InputStream, byteCount: Int): Unit = {
        var remaining = byteCount.toLong
        while (remaining > 0) {
            val skipped = input.skip(remaining)
            if (skipped > 0) {
                remaining -= skipped
            } else if (input.read() >= 0) {
                remaining -= 1
            } else {
                throw new java.io.EOFException("Unexpected end of file while skipping the BOM")
            }
        }
    }

    def main(args:Array[String]): Unit={
        val logPath = Paths.get(args.lift(0).getOrElse("output.log"))
        val encoding = detectEncoding(readSample(logPath))
        val input = Files.newInputStream(logPath)
        skipFully(input, encoding.bomBytes)
        val reader = new BufferedReader(new InputStreamReader(input, encoding.charset), 64 * 1024)

        try {
            println(s"parse starts (${encoding.charset.name()})")
            var line = reader.readLine()
            while (line != null) {
                val trimmed = line.trim
                if (trimmed.contains("Program finished") || trimmed.contains("differ")) {
                    println(line)
                }
                line = reader.readLine()
            }
        } finally {
            reader.close()
        }
    }
}
}
