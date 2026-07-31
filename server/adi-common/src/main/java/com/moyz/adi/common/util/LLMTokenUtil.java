package com.moyz.adi.common.util;

import dev.langchain4j.model.output.TokenUsage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.text.MessageFormat;
import java.time.Duration;
import java.util.List;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.TOKEN_USAGE_KEY;

@Slf4j
public class LLMTokenUtil {

    /**
     * Cache the token usage of a single LLM call under the given uuid.
     * <p>Values are appended as a pair {@code [inputTokenCount, outputTokenCount]} to a Redis list, so multiple
     * LLM calls within one user request accumulate on the same key. The key TTL is refreshed to 10 minutes on
     * every write; there is no explicit delete, so an unusually long request (>10 min since the last write)
     * may expire before the total is read back — a known trade-off.</p>
     *
     * @param stringRedisTemplate stringRedisTemplate
     * @param uuid                unique identifier of the user request
     * @param tokenUsage          token usage of this LLM call
     */
    public static void cacheTokenUsage(StringRedisTemplate stringRedisTemplate, String uuid, TokenUsage tokenUsage) {
        if (tokenUsage == null) {
            return;
        }
        String redisKey = MessageFormat.format(TOKEN_USAGE_KEY, uuid);
        stringRedisTemplate.expire(redisKey, Duration.ofMinutes(10));
        stringRedisTemplate.opsForList().rightPushAll(redisKey, String.valueOf(tokenUsage.inputTokenCount()), String.valueOf(tokenUsage.outputTokenCount()));
    }

    /**
     * Sum the token usage cached under the given uuid across all LLM calls of one user request.
     * <p>The Redis list holds pairs {@code [in1, out1, in2, out2, ...]}; this sums them up into a total
     * {@code Pair<inputTokens, outputTokens>}. A trailing unpaired element (input with no matching output)
     * is still counted toward the input total. Returns {@code (0, 0)} when the key is absent or expired.</p>
     *
     * @param stringRedisTemplate stringRedisTemplate
     * @param uuid                unique identifier of the user request
     * @return Pair<Integer, Integer> Pair&lt;total input token count, total output token count&gt;
     */
    public static Pair<Integer, Integer> calAllTokenCostByUuid(StringRedisTemplate stringRedisTemplate, String uuid) {
        List<String> tokenCountList = stringRedisTemplate.opsForList().range(MessageFormat.format(TOKEN_USAGE_KEY, uuid), 0, -1);
        int inputTokenCount = 0;
        int outputTokenCount = 0;
        if (!CollectionUtils.isEmpty(tokenCountList)) {
            int tokenCountListSize = tokenCountList.size();
            int i = 0;
            while (i < tokenCountListSize) {
                inputTokenCount += Integer.parseInt(tokenCountList.get(i));
                i++;
                if (i < tokenCountListSize) {
                    outputTokenCount += Integer.parseInt(tokenCountList.get(i));
                }
                i++;
            }
        }
        return Pair.of(inputTokenCount, outputTokenCount);
    }
}
